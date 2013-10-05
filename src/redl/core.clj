(ns redl.core
  (:require clojure.main
            reply.hacks.printing
            [clojure.string :as str]
            [clojure.core.async :as async]
            clj-stacktrace.repl) 
  (:use [clojure.repl :only [pst]]
        [complete.core :only [top-level-classes nested-classes]]
        [clojure.pprint :only [pprint]]))

(defn- dbg
  [& strs]
  (.println System/out (str/join " " strs))
  (.flush System/out))

;This var determines whether repl results are `println`ed or `pprint`ed.
(def pretty-print (atom true))

;This var tracks how many nested breaks there are active
(def ^:dynamic *repl-depth* 0)
;This var is used by the eval-with-locals subsystem
(declare ^:dynamic *locals*)

;The next 3 functions were borrowd from GeorgeJahad's debug-repl
(defmacro local-bindings
  "Produces a map of the names of local bindings to their values."
  []
  (let [symbols (keys &env)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))

(defn view-locals
  []
  *locals*)

(defn eval-with-locals
  "Evals a form with given locals. The locals should be a map of symbols to
  values."
  [locals form]
  (binding [*locals* locals]
    (eval
      `(let ~(vec (mapcat #(list % `(*locals* '~%)) (keys locals)))
         ~form))))

(defn eval-with-state-and-locals
  "Evaluates a form with a given state and local binding. The local binding
  is the return value of using the `local-bindings` macro at the place
  you wish to capture the locals. State is a map containing the
  repl state, which has the following keys: `:*1`, `:*2`, `:*3`, `:*e`,
  and `:ns`. It will return the updated state, along with the extra keys
  `:out`, `:err`, and `:result`, which will be bound to all the stdout and
  stderr of the evaluated form, and it's value. The value will also be
  included `pprint`ed in stdout for display convenience."
  [form state locals]
  (let [out-writer (java.io.StringWriter.)
        err-writer (java.io.StringWriter.)]
    (binding [*ns* *ns*
              *out* (java.io.PrintWriter. out-writer)
              *err* (java.io.PrintWriter. err-writer)
              *1 (:*1 state)
              *2 (:*2 state)
              *3 (:*3 state)
              *e (:*e state)]
      (with-redefs [clojure.core/print-sequential reply.hacks.printing/print-sequential
                    clojure.repl/pst clj-stacktrace.repl/pst] 
        (let [ex (atom nil)
              repl-thread (atom (Thread/currentThread))
              result (try
                       (in-ns (:ns state))
                       (doto (eval-with-locals locals form)
                         ((if @pretty-print pprint println)))
                       (catch Throwable t
                         ;`::continue` is a special case for debugging
                         (if (contains? (ex-data t) ::continue)
                           (throw t)
                           (do
                             (doto (clojure.main/repl-exception t)
                               (pst)) 
                             (reset! ex t)))))]
          (reset! repl-thread nil) 
          (->
            (if-let [e @ex]
              (assoc state :*e e :result ::error)
              (assoc state
                     :result result
                     :*1 result
                     :*2 *1
                     :*3 *2))
            (assoc 
              :ns (ns-name *ns*)
              :repl-depth *repl-depth*
              :out (str out-writer)
              :err (str err-writer))))))))

(def ^:dynamic *repl-input*)
(def ^:dynamic *repl-output*)

(defn late-bound-repl-loop
  "This creates a repl loop with the given initial state and, optionally,
  locals. This is late-bound because it uses repl-input and repl-output,
  call `deref` such that it is possible for an evaluated form to recursively
  create another late-bound-repl-loop."
  ([state]
   (late-bound-repl-loop state {}))
  ([state locals]
   (loop [state state]
     (let [state' (eval-with-state-and-locals
                    (async/<!! *repl-input*) state locals)]
       (async/>!! *repl-output* state')
       (recur state')))))

(defn break*
  "Invoke this to drop into a new sub-repl, which
  can return into the parent repl at any time. Must supply
  locals, that will be in scope in the new subrepl."
  [locals]
  (try
    (binding [*repl-depth* (inc *repl-depth*)]
      (async/>!! *repl-output* {:out "Encountered break, waiting for input..."
                                :err ""
                                :ns (ns-name *ns*)
                                :repl-depth *repl-depth*})
      (late-bound-repl-loop {:ns (ns-name *ns*)
                             :*1 *1 :*2 *2 :*3 *3 :*e *e}
                            locals))
    (catch clojure.lang.ExceptionInfo ex
      (assert (contains? (ex-data ex) ::continue))
      (::continue (ex-data ex)))))

(defmacro break
  "Invoke this to drop into a new sub-repl. It will automatically capture
  the locals visible from the place it is invoked. To return from the `break`
  statement, call `continue`. See continue for details on the return value
  of break."
  ([]
   `(break nil))
  ([value]
   `(let [bindings# (local-bindings)
          value# ~value
          debug-result# 
          (break* bindings#)]
      (if (= debug-result# ::no-arg)
        value#
        debug-result#))))

(defn continue
  "Invoke this from inside a debug repl to return up a level.
  
  If no value is provided, the corresponding `(break argument)` will return
  its argument or `nil` if invoked as `(break)`. If a value is provided,
  `break` will return that value and discard the provided argument."
  ([]
   (continue ::no-arg))
  ([value]
   (if (zero? *repl-depth*)
     (throw (ex-info "Cannot call continue when not in a break statement!" {}))
     (throw (ex-info "" {::continue value})))))

(defn eval-worker
  "Creates an eval worker thread that can transfer its IO control
   down the stack. Returns `[in out thread]`, where `in` and `out`
   are the channels to send and recieve messages, and `thread` is
   a promise containing the actual worked Thread, for debugging."
  [ns]
  (let [in (async/chan)
        out (async/chan)
        thread (promise)]
    ;;TODO: make this thread called "repl-%id"?
    (async/thread
      (deliver thread (Thread/currentThread))
      (binding [*repl-input* in
                *repl-output* out]
        (late-bound-repl-loop {:ns ns})))
    [in out thread]))

(def supervisor-ids (atom 0))
(def supervisors (atom {}))

(defn do-wait
  [latest-state worker-out out]
  (let [t (async/timeout 1000)]
    (async/alt!!
      worker-out
      ([state]
       (dbg "got result from worker")
       (async/>!! out state)
       [false state])
      t
      ([_]
       (dbg "got timeout from worker")
       (async/>!! out (assoc latest-state
                             :out "Worker has not yet finished computation. Try meta commands [wait, stack, interrupt, stop, help]."
                             :err ""))
       [true latest-state]))))

(defn do-stacktrace
  [state ^Thread thread out]
  (let [stacktrace (.getStackTrace thread)
        pretty (with-out-str
                 (doseq [ste stacktrace]
                   (clojure.stacktrace/print-trace-element ste)
                   (println)))]
    (async/>!! out (assoc state
                          :out (str "Stack trace of thread: "
                                    (.getName thread) "\n\n"
                                    pretty)
                          :err ""))))

(defn print-help
  [state out]
  (async/>!! out (assoc state
                        :out "Enter wait, stack, interrupt, or stop"
                        :err "")))
(defn eval-supervisor
  "Creates an eval supervisor thread that will create and drive
   an eval-worker. If the worker becomes unresponsive, the
   supervisor allows meta-control of the worker (stop, threadump, wait).

   Returns the supervisor id, which allows it to be controlled in
   the repl."
  [ns]
  (let [id (swap! supervisor-ids inc)
        in (async/chan)
        out (async/chan)
        [worker-in worker-out thread :as worker] (eval-worker ns)]
    (swap! supervisors assoc id [in out])
    (async/thread
      (loop [busy false
             latest-state {:ns ns}]
        (dbg "super waiting for input")
        (let [form (async/<!! in)]
          (dbg "super got input" form)
          (if busy
            ;; When busy, try doing an op
            (do (dbg "busy, interpretting") (condp = (if (= (first form) `do) (second form) form)
              'wait (let [[busy state] (do-wait latest-state worker-out out)]
                      (recur busy state))
              'stack (do (do-stacktrace latest-state @thread out)
                         (recur true latest-state))
              'interrupt (do (.interrupt @thread) 
                             (let [[busy state] (do-wait latest-state worker-out out)]
                               (recur busy state)))
              'stop (do (async/>!! out (assoc latest-state
                                              :out "Stopped thread; creating new worker..."
                                              :err ""))
                        (.stop @thread) 
                        ;; Clear out
                        (swap! supervisors dissoc id)
                        ;; Make a new worker thread
                        #_(recur true latest-state (eval-worker ns)))
              (do (print-help latest-state out)
                  (recur true latest-state))))
            ;; Not busy, do a new eval
            (do (dbg "not busy, sending to worker") (async/>!! worker-in form) (dbg "not busy, sent to worker")
                (let [[busy state] (do-wait latest-state worker-out out)]
                  (dbg "finished waiting for worker" busy state worker)
                  (recur busy state)))))))
    id))

(defn make-repl
  ([]
   (make-repl 'user))
  ([ns]
   (eval-supervisor ns)))

(defn repl-eval
  "Takes a repl id and a form, and evaluates that form on the given repl."
  [repl form]
  (if-let [[input output] (@supervisors repl)]
    (do (async/>!! input form)
        (async/<!! output))
    (do
      {:ns 'user
       :out "This repl doesn't exist. You must start a new one."
       :err ""})))


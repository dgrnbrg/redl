(ns redl.core
  (:require clojure.main
            reply.hacks.printing
            clj-stacktrace.repl) 
  (:use [clojure.repl :only [pst]]
        [complete.core :only [top-level-classes nested-classes]]
        [clojure.pprint :only [pprint]]))

;This var determines whether repl results are `println`ed or `pprint`ed.
(def *pretty-print* (atom true))

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

(def repl-timeout-ms 1000)

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
                         ((if @*pretty-print* pprint println)))
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

;These dynamic vars hold the thread-local atoms that store the input
;and output promises for the repl. They hold atoms so that all reads/writes
;can be done as late as possible, which allows for debuggers further down
;the stack to temporarily take over the IO.
(def ^:dynamic repl-input)
(def ^:dynamic repl-output)

(defn late-bound-repl-loop
  "This creates a repl loop with the given initial state and, optionally,
  locals. This is late-bound because it uses repl-input and repl-output,
  call `deref` such that it is possible for an evaluated form to recursively
  create another late-bound-repl-loop."
  ([state]
   (late-bound-repl-loop state {}))
  ([state locals]
   (loop [state state]
     (let [state' (eval-with-state-and-locals @@repl-input state locals)]
       (deliver @repl-output state')
       (reset! repl-input (promise))
       (reset! repl-output (promise))
       (recur state')))))

(defn repl-entry-point
  "Creates a no-arg function that, when invoked, becomes the repl, and returns
  that function along with the input/output atom pair.

  When an input is delivered, wait on the output promise to get a
  map containing the evaluation results."
  [ns]
  (binding [repl-input (atom (promise))
            repl-output (atom (promise))]
    [(bound-fn* #(late-bound-repl-loop {:ns ns}))
     repl-input repl-output]))

(defn break*
  "Invoke this to drop into a new sub-repl, which
  can return into the parent repl at any time. Must supply
  locals, that will be in scope in the new subrepl."
  [locals]
  (try
    (binding [*repl-depth* (inc *repl-depth*)]
      (deliver @repl-output {:out "Encountered break, waiting for input..."
                             :err ""
                             :ns (ns-name *ns*)
                             :repl-depth *repl-depth*})
      (reset! repl-input (promise)) 
      (reset! repl-output (promise)) 
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

;This tracks the active repls and allows for them to have new ids generated
(def repls (atom {}))
(def repl-id (atom 0))

(defn make-repl
  "Returns the id of a newly-created repl Thread. That repl will start
  in the given namespace, or `user` if none is provided."
  ([]
   (make-repl 'user))
  ([ns]
   (let [my-id (swap! repl-id inc)
         [replf input output] (repl-entry-point ns)
         thread (doto (Thread. replf)
                  (.setName (str "redl-" my-id))
                  .start)
         data {:input input
               :output output
               :id my-id}]
     (swap! repls assoc my-id data)
     my-id)))

(defn repl-eval
  "Takes a repl id and a form, and evaluates that form on the given repl."
  [repl form]
  (let [{:keys [input output id]} (@repls repl)
        _ (deliver @input form)
        result (deref @output 9500 {:out "Critical timeout-repl has been lost"})]
    result))

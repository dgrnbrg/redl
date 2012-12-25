(ns redl.core
  (:require clojure.main)
  (:use [clojure.string :only [split]]
        [complete.core :only [top-level-classes nested-classes]]
        [clojure.pprint :only [pprint]]))

(defn common-subseq-score
  "Takes 2 seqs, and checks for common subseq. Allows any characters to be
  skipped between matches, and returns how many were skipped in the order of
  each gap in the returned vector. If `:anchor` appears, the next character can have no padding
  before it.
  
  `sub` is the thing being tested as a subsquence.
  `super` is the thing being tested whether it contains `pattern`
  `reserved-set` can only appear in `super` when they match something in `sub`"
  ([sub super reserved-set]
   (if (= sub [:anchor])
     ;special case for an anchor and nothing else
     [0 (count super)]
     (loop [[token & pattern :as sub] sub
            [target & rest-target :as super] super
            anchoring? false
            score 0
            scores []]
       (cond
         ;Pattern fully consumed
         (empty? sub)
         (if anchoring?
           (when (empty? super)
             (conj scores 0))
           (when-not (some reserved-set super)
             (conj scores (count super))))

         ;Enabled anchoring mode
         (= token :anchor)
         (recur pattern super true score scores)

         ;text fully consumed
         (empty? super)
         nil

         ;consume pattern token
         (= token target)
         (recur pattern
                rest-target
                false
                0
                (conj scores score))

         ;didn't match; so fail
         anchoring?
         nil

         ;check if fail due to reserved set
         (reserved-set target)
         nil

         ;penalize score
         :else
         (recur sub rest-target anchoring? (inc score) scores)))))
  ([sub super]
   (common-subseq-score sub super #{})))

(let [delimiters "/.$"
      regex-with-lookback (re-pattern
                            (str "(?<=[" delimiters "])"))
      regex (re-pattern (str \[ delimiters \]))]

  (defn str->pattern
    "Converts a string to a pattern. Makes each letter following
    a period or slash into an anchor."
    [string]
    (loop [[head & tail] (split string regex-with-lookback)
           final []]
      (if head
        (if (re-matches regex head)
          (recur tail (into final head))
          (recur tail (into final (cons :anchor head))))
        final))))

(defn score-match
  "Takes a match score and returns a numeric value.
  
  Ignores the last element of the score vec, since
  this heuristic doesn't penalize a long tail.
  
  If 2 scores are equal, they follow alphabetical order"
  [score-vec match]
  (let [general-power 2
        depth-penalty 1.5]
    (loop [[head & tail] (butlast score-vec)
           penalty 1
           score 0]
      (if head
        (recur tail (* penalty depth-penalty)
               (+ score
                  (Math/pow (Math/abs head) general-power)))
        [score  match]))))

(defn nses-for-ns
  "Returns a map of string->namespace mappings for a given
  namespace, including all qualified and aliases nses."
  [ns]
  (into (ns-aliases ns)
        (map (juxt ns-name identity) (all-ns))))

(defn fuzzy-match-pairs
  "Takes a seq of pairs of `Named` names and descriptive objects,
  and returns a seq of triples where the elements are a score vector,
  string name, and descriptive object (in that order). If a pair
  didn't fuzzy match, it is not included in the output."
  [pattern pairs]
  (->> pairs
    (map (fn [[k v]]
           [(common-subseq-score
              pattern
              (name k)
              (partial = \.))
            (name k)
            v]))
    (filter first)))

(defn extract-param-types
  "Takes a `Constructor` or `Method` and returns a vector
  of the arguments."
  [target]
  (mapv (comp symbol #(.getSimpleName %)) (.getParameterTypes target)))

(defn class-matches
  "Takes an ns and a pattern, and returns the classes that
  match that pattern in that ns."
  [ns pattern]
  (->> @(if (.contains pattern "$")
          nested-classes top-level-classes)
    (concat (keys (ns-imports ns)))
    (map (juxt identity (constantly {:class true})))
    (fuzzy-match-pairs pattern)
    (map (fn [[score class-name desc]]
           (let [class (ns-resolve ns (symbol class-name))]
             [score class-name
              (assoc desc
                     :arglists (->> (.getConstructors class)
                                 (map extract-param-types)
                                 (set)
                                 (sort)))])))))

(defn methods->completions
  "Takes a seq of Method objects and returns a seq of pairs
  that can be fuzzy-matched and include additional metadata,
  such as argument lists."
  [methods]
  (->> (for [method methods]
         {:method (.getName method)
          :argtypes (extract-param-types method)})
    (group-by :method) 
    (map (fn [[name bodies]]
           [name {:method true
                  :arglists (sort (set (map :argtypes bodies)))}]))))

(defn unscoped-completions
  "Takes a namespace and a pattern without a forward slash,
  and returns a list of triples, where each triple is a
  [completion score, string name, descriptive object] for
  the possible completions."
  [ns pattern] 
  (let [compiled-pattern (str->pattern pattern)
        candidates (-> (nses-for-ns ns)
                     (into (ns-map ns))
                     (into (when (= (first pattern) \.)
                             (->> (for [class (vals (ns-imports ns))
                                        method (.getMethods class)]
                                    method)
                               methods->completions
                               (map (fn [[k v]]
                                      [(str \. k) v]))))))
        matches (fuzzy-match-pairs compiled-pattern candidates)
        class-matches (class-matches ns pattern)
        sorted (sort-by (fn [[score n]]
                          (score-match score n))
                        (concat class-matches matches))]
    sorted))

(defn static? [^java.lang.reflect.Member member]
  (java.lang.reflect.Modifier/isStatic (.getModifiers member)))

(defn scoped-completions
  "Takes a namespace and a pattern with a forward slash,
  and returns a list of triples where each triple is a
  [completion score, string name, descriptive object] for
  the possible completions."
  [ns pattern]
  (let [slash-index (.indexOf pattern "/")
        ns-pattern (str->pattern (.substring pattern 0 slash-index))
        whole-pattern (str->pattern pattern)
        candidate-nses (->> (nses-for-ns ns)
                         (fuzzy-match-pairs ns-pattern)
                         (map next))
        candidate-vars (->> candidate-nses
                         (mapcat (fn [[sym ns]]
                                   (let [ns-name (name sym)]
                                     (->> (ns-publics ns)
                                       (map (fn [[k v]]
                                              [(str ns-name \/ (name k)) v]))
                                       (fuzzy-match-pairs whole-pattern))))))
        candidate-classes (class-matches ns ns-pattern)
        candidate-members (->> candidate-classes
                            (mapcat (fn [[_ class-name]]
                                      (let [class (ns-resolve ns (symbol class-name))
                                            fields (->> (.getDeclaredFields class)
                                                     (filter static?)
                                                     (map (juxt #(str class-name \/ (.getName %))
                                                                (constantly {:field true}))))
                                            methods (->> (.getMethods class)
                                                      (filter static?)
                                                      methods->completions
                                                      (map (fn [[k v]]
                                                             [(str class-name \/ k) v])))]
                                        (fuzzy-match-pairs whole-pattern
                                                           (concat fields methods))))))
        sorted (sort-by (fn [[score n]] (score-match score n))
                        (concat candidate-members candidate-vars))]
    sorted))

(defn completions
  "Finds fuzzy completions for a pattern in the given namespace.

  Could return a namespace, an alias, a var from the ns's map,
  or a namespace or alias-qualified var."
  [ns pattern]
  (map
    next
    (if-not (.contains pattern "/")
      ;Namespace, aliases, or var
      (unscoped-completions ns pattern)
      ;qualified var
      (scoped-completions ns pattern))))

(defn ->vim-omnicomplete
  "Pass the pair of `[name var-or-ns]` to this function
  and it will return an omnicomplete-compatible map."
  [[name var-or-ns]]
  (cond
    (var? var-or-ns)
    (let [v var-or-ns
          m (meta var-or-ns)]
      {:word name
       :menu (pr-str (:arglists m (symbol "")))
       :info (str "  " (:doc m))
       :kind (if (:arglists m) "f" "v")})
    (instance? clojure.lang.Namespace var-or-ns)
    {:word name
     :kind "t"
     :info "" ;TODO: include ns doc
     }
    ;Static member of java class
    (:method var-or-ns)
    {:word name
     :kind "f"
     :menu (pr-str (:arglists var-or-ns (symbol "")))
     }
    (:field var-or-ns)
    {:word name
     :kind "m"
     }
    ;plain java class
    (:class var-or-ns)
    {:word name
     :kind "t"
     :menu (pr-str (:arglists var-or-ns (symbol "")))
     }))

;Repl id maps to
;- which ns to eval wihin
;- a promise to which the next form to be evaled should be delivered
;- a handle to the actual thread so that it can be killed
;
;API:
;- a function to create a new repl, add it to the mapping, and return the id
;- a function to evaluate a form on a repl
;- a function to interrupt evaluation on a given repl?
;- a function to stop/kill a given repl?

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
      (let [ex (atom nil)
            result (try
                     (in-ns (:ns state))
                     (doto (eval-with-locals locals form)
                       pprint)
                     (catch Throwable t
                       ;`::continue` is a special case for debugging
                       (if (contains? (ex-data t) ::continue)
                         (throw t)
                         (do
                           (doto (clojure.main/repl-exception t)
                             (.printStackTrace *err*))
                           (reset! ex t)))))]
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
            :err (str err-writer)))))))

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
        result (deref @output 8000 {:err "Repl thread timed out"})
        result (select-keys result [:out :err :result :repl-depth])]
    result))

(ns redl.core
  (:use [clojure.string :only [split]]
        [complete.core :only [top-level-classes nested-classes]]))

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

(defn class-matches
  "Takes an ns and a pattern, and returns the classes that
  match that pattern in that ns."
  [ns pattern]
  (->> @(if (.contains pattern "$")
          nested-classes top-level-classes)
    (concat (keys (ns-imports ns)))
    (map (juxt identity (constantly {:class true})))
    (fuzzy-match-pairs pattern)) )

(defn methods->completions
  "Takes a seq of Method objects and returns a seq of pairs
  that can be fuzzy-matched and include additional metadata,
  such as argument lists."
  [methods]
  (->> (for [method methods]
         {:method (.getName method)
          :argtypes (mapv (comp symbol #(.getSimpleName %)) (.getParameterTypes method))})
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
     }))

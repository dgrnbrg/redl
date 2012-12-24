(ns redl.core
  (:use [clojure.string :only [split]]))

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

(defn completions
  "Finds fuzzy completions for a pattern in the given namespace.

  Could return a namespace, an alias, a var from the ns's map,
  or a namespace or alias-qualified var."
  [ns pattern]
  (let [split-pattern (split pattern #"/")
        split-pattern (if (and (= (count split-pattern) 1)
                               (not= pattern (first split-pattern)))
                        (conj split-pattern "")
                        split-pattern)
        publics (ns-publics ns)
        aliases (ns-aliases ns)
        nses (all-ns)]
    (map
      next
      (condp = (count split-pattern)
        ;Namespace, aliases, or var
        1
        (let [pattern (str->pattern pattern)
              candidates (into aliases
                               (concat
                                 (map (juxt ns-name identity) nses)
                                 (ns-map ns)))
              matches (->> candidates
                        (map (fn [[k v]]
                               [(common-subseq-score
                                  pattern
                                  (name k)
                                  (partial = \.))
                                (name k)
                                v]))
                        (filter
                          (fn [[score k v]]
                            score)))
              sorted (sort-by (fn [[score n]]
                                (score-match score n)) matches)]
          sorted)

        ;qualified var
        2
        (let [ns-pattern (str->pattern (first split-pattern))
              whole-pattern (str->pattern pattern)
              candidate-nses (->> (into aliases
                                        (map (juxt ns-name identity) nses))
                               (map (fn [[sym ns]]
                                      [(common-subseq-score
                                         ns-pattern
                                         (name sym)
                                         (partial = \.))
                                       sym
                                       ns]))
                               (filter first)
                               (map next))
              candidate-vars (->> candidate-nses
                               (mapcat (fn [[sym ns]]
                                         (let [ns-name (name sym)]
                                           (map (fn [[k v]]
                                                  (let [var-name (name k)
                                                        completion (str
                                                                     ns-name \/
                                                                     var-name)]
                                                    [(common-subseq-score
                                                       whole-pattern
                                                       completion
                                                       #{\. \/})
                                                     completion
                                                     v])) (ns-publics ns)))))
                               (filter first)
                               (sort-by (fn [[score n]] (score-match score n))))]
          candidate-vars)

        ;invalid
        (throw (ex-info (str "too many pattern parts" split-pattern) {}))
        ))
    ))

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
    :else
    (throw (ex-info "Not a var or ns" {:bad var-or-ns}))))

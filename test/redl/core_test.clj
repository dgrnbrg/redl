(ns redl.core-test
  (:use expectations
        redl.core))

(expect
  [0 0 0 0]
  (common-subseq-score [1 2 3] [1 2 3]))

(expect
  [1 0 0 0]
  (common-subseq-score [1 2 3] [0 1 2 3]))

(expect
  [0 1 0 0]
  (common-subseq-score [1 2 3] [1 1 2 3]))

(expect
  [0 5]
  (common-subseq-score [:a] [:a 1 2 3 4 5]))

(expect
  [5 0]
  (common-subseq-score [:a] [1 2 3 4 5 :a]))

(expect
  [5 5]
  (common-subseq-score [:a] [1 2 3 4 5 :a 1 2 3 4 5]))

(expect
  nil
  (common-subseq-score [:a] [:b]))

(expect
  [0 1]
  (common-subseq-score [:anchor :a] [:a :b]))

(expect
  nil
  (common-subseq-score [:anchor :a] [:b :a]))

(expect
  [0 0]
  (common-subseq-score [:a :anchor] [:a]))

(expect
  nil
  (common-subseq-score [:a :anchor] [:a :b]))

(expect
  [1 0]
  (common-subseq-score [:a :anchor] [:b :a]))

(expect
  [0 0]
  (common-subseq-score [:a :anchor] [:a]))

(expect
  [0 0]
  (common-subseq-score [:a :anchor] [:a]))

(expect
  [0 2 3 0 3]
  (common-subseq-score (str->pattern "f./q") "foo.bar/quux")) 

(expect
  nil
  (common-subseq-score (str->pattern "f./q") "foo..bazbar/quux"
                       #{\. \/}))

(expect
  nil
  (common-subseq-score (str->pattern "f.") "foo..bazbar"
                       #{\. \/}))

(expect
  nil
  (common-subseq-score (str->pattern "f./q") "foo/quux"))

(expect
  "clojure.set/union" 
  (in (map first (completions 'redl.core "c.st/"))))

(expect
  "clojure.string/join"
  (in (map first (completions 'redl.core "c.st/j"))))

(expect
  "clojure.set/project"
  (in (map first (completions 'redl.core "c.st/p"))))

(expect
  "clojure.string"  
  (in (map first (completions 'redl.core "c."))))

(expect
  "clojure.core" 
  (in (map first (completions 'redl.core "c."))))

(expect
  "clojure.core" 
  (in (map first (completions 'redl.core "."))))

(expect
  "re-matches" 
  (in (map first (completions 'redl.core "re-"))))

(expect
  "re-find" 
  (in (map first (completions 'redl.core "re-"))))

(expect
  nil
  (some (comp (partial = "clojure.java.io") first)
        (completions 'redl.core "c.")))

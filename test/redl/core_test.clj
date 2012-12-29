(ns redl.core-test
  (:use expectations
        [redl complete core])
  (:import java.lang.Thread))

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

(expect
  "java.awt.geom.Rectangle2D" 
  (in (map first (completions 'redl.core "j.a..Rct"))))

(expect
  "java.awt.geom.Rectangle2D$Float" 
  (in (map first (completions 'redl.core "j.a..Rct$"))))

(expect
  "Math/PI"
  (in (map first (completions 'redl.core "M/P"))))

(expect
  "java.lang.Math/PI"
  (in (map first (completions 'redl.core "j..M/P"))))

(expect
  "java.lang.Math/cos"
  (in (map first (completions 'redl.core "j..M/cos"))))

(expect
  "Math/atan2"
  (in (map first (completions 'redl.core "M/atan2"))))

(expect
  ".start"
  (in (map first (completions 'redl.core-test ".st"))))

(let [my-repl (make-repl)]
  ;Ensure repl has *1, *2, *3, and *e set up
  (expect [nil nil nil nil]
          (:result (repl-eval my-repl '[*1 *2 *3 *e])))

  (expect {:result [3 2 1]
           :ns 'user}
          (in (do
                (repl-eval my-repl '(.foo bar))
                (repl-eval my-repl 1)
                (repl-eval my-repl 2)
                (repl-eval my-repl 3)
                (repl-eval my-repl '[*1 *2 *3]))))

  (expect (not (nil? (:result (repl-eval my-repl '*e)))))

  ;Change ns to avoid syntax-quoting confusion in test
  (expect
    {:ns 'redl.core-test}
    (in (repl-eval my-repl `(in-ns 'redl.core-test))))

  ;Basic functionality
  (expect {:result 12 :repl-depth 0}
          (in (repl-eval my-repl `(* 3 4))))
  
  ;Test that debug repl nests and resumes correct
  (expect {:out "Encountered break, waiting for input..."
           :repl-depth 1}
          (in (repl-eval my-repl `(- (break) 10))))

  ;Stays at the current level if no change is made
  (expect {:out "3\n" :repl-depth 1}
          (in (repl-eval my-repl 3)))

  ;Go to deeper levl
  (expect {:out "Encountered break, waiting for input..."
           :repl-depth 2}
          (in (repl-eval my-repl `(println (break)))))

  ;Return from inner level
  (expect {:out "hello\nnil\n" :repl-depth 1}
          (in (repl-eval my-repl `(continue "hello"))))

  ;Return from outer level
  (expect {:result 7 :repl-depth 0}
          (in (repl-eval my-repl `(continue 17))))

  ;Test local binding
  (expect {:out "Encountered break, waiting for input..."}
          (in (repl-eval my-repl '(let [x 3 y 7] (break 5)))))

  (expect {:result 21}
          (in (repl-eval my-repl '(continue (* x y)))))

  ;Test default return value logic
  (expect {:out "Encountered break, waiting for input..."}
          (in (repl-eval my-repl '(let [x 3 y 7] (break 5)))))

  (expect {:result 5}
          (in (repl-eval my-repl '(continue))))

  (expect {:out "Encountered break, waiting for input..."}
          (in (repl-eval my-repl '(break))))

  (expect {:result nil}
          (in (repl-eval my-repl '(continue))))

  ;Ensure locals shadow correctly
  (expect {:out "Encountered break, waiting for input..."}
          (in (repl-eval my-repl '(let [x 3 y 7] (break 5)))))

  (expect {:out "Encountered break, waiting for input..."}
          (in (repl-eval my-repl '(let [x 2 y 2] (break)))))

  ;Inner is 2 + 2
  (expect {:result 4}
          (in (repl-eval my-repl '(continue (+ x y)))))

  ;Outer is 3 + 7
  (expect {:result 10}
          (in (repl-eval my-repl '(continue (+ x y)))))

  ;Ensure continue gives sane error message
  (expect #"Cannot call continue when not in a break statement!"
          (-> (repl-eval my-repl '(continue))
            :out))
  
  (expect #"Cannot call continue when not in a break statement!"
          (-> (repl-eval my-repl '(continue))
            :out)))

;Now, we try interleaving 2 repls to confirm no disruption occurs
(let [repl1 (make-repl)
      repl2 (make-repl)]
  (expect {:out "Encountered break, waiting for input..."
           :repl-depth 1}
          (in (repl-eval repl1 '(let [x 3 y 4]
                                  (redl.core/break)))))

  (expect {:out "Encountered break, waiting for input..."
           :repl-depth 1}
          (in (repl-eval repl2 '(let [x 6 z 7]
                                  (redl.core/break)))))

  (expect {:result 7}
          (in (repl-eval repl1 '(+ x y))))

  (expect {:result 13}
          (in (repl-eval repl2 '(+ x z)))))

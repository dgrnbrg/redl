(defproject redl "0.2.4"
  :description "Read Eval Debug Loop"
  :url "http://github.com/dgrnbrg/vim-redl"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :deploy-repositories  [["releases" :clojars]]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [clojure-complete "0.2.2"]
                 [reply "0.1.2"]
                 [clj-stacktrace "0.2.5"]
                 [org.clojure/core.async "0.1.242.0-44b1e3-alpha"]
                 [expectations "1.4.17"]]) 

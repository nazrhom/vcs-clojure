<<<<<<< A.clj
(defproject ring/ring-devel "1.6.0-beta6"
||||||| O.clj
(defproject ring/ring-devel "1.5.0"
=======
(defproject ring/ring-devel "1.5.1"
>>>>>>> B.clj
  :description "Ring development and debugging libraries."
  :url "https://github.com/ring-clojure/ring"
  :scm {:dir ".."}
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.5.1"]
<<<<<<< A.clj
                 [ring/ring-core "1.6.0-beta6"]
||||||| O.clj
                 [ring/ring-core "1.5.0"]
=======
                 [ring/ring-core "1.5.1"]
>>>>>>> B.clj
                 [hiccup "1.0.5"]
                 [clj-stacktrace "0.2.8"]
                 [ns-tracker "0.3.0"]]
  :profiles
  {:1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
   :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
   :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}})

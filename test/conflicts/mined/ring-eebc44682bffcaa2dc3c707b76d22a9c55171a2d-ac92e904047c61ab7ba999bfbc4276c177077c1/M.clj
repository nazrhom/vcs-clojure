<<<<<<< A.clj
(defproject ring/ring-devel "1.2.0-SNAPSHOT"
||||||| O.clj
(defproject ring/ring-devel "1.1.5"
=======
(defproject ring/ring-devel "1.1.6"
>>>>>>> B.clj
  :description "Ring development and debugging libraries."
  :url "https://github.com/ring-clojure/ring"
<<<<<<< A.clj
  :dependencies [[ring/ring-core "1.2.0-SNAPSHOT"]
||||||| O.clj
  :dependencies [[ring/ring-core "1.1.5"]
=======
  :dependencies [[ring/ring-core "1.1.6"]
>>>>>>> B.clj
                 [hiccup "1.0.0"]
                 [clj-stacktrace "0.2.4"]
<<<<<<< A.clj
                 [ns-tracker "0.1.2"]]
  :profiles
  {:1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
   :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}})
||||||| O.clj
                 [ns-tracker "0.1.2"]])
=======
                 [ns-tracker "0.2.0"]])
>>>>>>> B.clj

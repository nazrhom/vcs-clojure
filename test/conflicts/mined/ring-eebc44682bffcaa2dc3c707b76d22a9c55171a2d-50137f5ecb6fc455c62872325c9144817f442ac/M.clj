<<<<<<< A.clj
(defproject ring/ring-servlet "1.2.0-SNAPSHOT"
||||||| O.clj
(defproject ring/ring-servlet "1.1.5"
=======
(defproject ring/ring-servlet "1.1.6"
>>>>>>> B.clj
  :description "Ring servlet utilities."
  :url "https://github.com/ring-clojure/ring"
<<<<<<< A.clj
  :dependencies [[ring/ring-core "1.2.0-SNAPSHOT"]
                 [javax.servlet/servlet-api "2.5"]]
  :profiles
  {:1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
   :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}})
||||||| O.clj
  :dependencies [[ring/ring-core "1.1.5"]
                 [javax.servlet/servlet-api "2.5"]])
=======
  :dependencies [[ring/ring-core "1.1.6"]
                 [javax.servlet/servlet-api "2.5"]])
>>>>>>> B.clj

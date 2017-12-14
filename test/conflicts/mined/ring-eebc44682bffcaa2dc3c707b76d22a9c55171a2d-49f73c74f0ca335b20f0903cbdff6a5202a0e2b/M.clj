<<<<<<< A.clj
(defproject ring "1.2.0-SNAPSHOT"
||||||| O.clj
(defproject ring "1.1.5"
=======
(defproject ring "1.1.6"
>>>>>>> B.clj
  :description "A Clojure web applications library."
  :url "https://github.com/ring-clojure/ring"
  :dependencies
<<<<<<< A.clj
    [[ring/ring-core "1.2.0-SNAPSHOT"]
     [ring/ring-devel "1.2.0-SNAPSHOT"]
     [ring/ring-jetty-adapter "1.2.0-SNAPSHOT"]
     [ring/ring-servlet "1.2.0-SNAPSHOT"]]
||||||| O.clj
    [[ring/ring-core "1.1.5"]
     [ring/ring-devel "1.1.5"]
     [ring/ring-jetty-adapter "1.1.5"]
     [ring/ring-servlet "1.1.5"]]
=======
    [[ring/ring-core "1.1.6"]
     [ring/ring-devel "1.1.6"]
     [ring/ring-jetty-adapter "1.1.6"]
     [ring/ring-servlet "1.1.6"]]
>>>>>>> B.clj
  :plugins
    [[lein-sub "0.2.1"]
     [codox "0.6.1"]]
  :sub
    ["ring-core"
     "ring-devel"
     "ring-jetty-adapter"
     "ring-servlet"]
  :codox
    {:sources ["ring-core/src"
               "ring-devel/src"
               "ring-jetty-adapter/src"
               "ring-servlet/src"]})

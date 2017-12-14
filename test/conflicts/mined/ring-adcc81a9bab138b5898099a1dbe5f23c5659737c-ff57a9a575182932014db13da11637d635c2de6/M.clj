<<<<<<< A.clj
(defproject ring "1.2.0-SNAPSHOT"
||||||| O.clj
(defproject ring "1.1.6"
=======
(defproject ring "1.1.7"
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
    [[ring/ring-core "1.1.6"]
     [ring/ring-devel "1.1.6"]
     [ring/ring-jetty-adapter "1.1.6"]
     [ring/ring-servlet "1.1.6"]]
=======
    [[ring/ring-core "1.1.7"]
     [ring/ring-devel "1.1.7"]
     [ring/ring-jetty-adapter "1.1.7"]
     [ring/ring-servlet "1.1.7"]]
>>>>>>> B.clj
  :plugins
<<<<<<< A.clj
    [[lein-sub "0.2.1"]
     [codox "0.6.3"]]
||||||| O.clj
    [[lein-sub "0.2.0"]
     [codox "0.6.1"]]
=======
    [[lein-sub "0.2.0"]
     [codox "0.6.3"]]
>>>>>>> B.clj
  :sub
    ["ring-core"
     "ring-devel"
     "ring-jetty-adapter"
     "ring-servlet"]
  :codox
<<<<<<< A.clj
    {:src-dir-uri "http://github.com/ring-clojure/ring/blob/1.1.6"
     :src-linenum-anchor-prefix "L"
     :sources ["ring-core/src"
||||||| O.clj
    {:sources ["ring-core/src"
=======
    {:src-dir-uri "http://github.com/ring-clojure/ring/blob/1.1.7"
     :src-linenum-anchor-prefix "L"
     :sources ["ring-core/src"
>>>>>>> B.clj
               "ring-devel/src"
               "ring-jetty-adapter/src"
               "ring-servlet/src"]})

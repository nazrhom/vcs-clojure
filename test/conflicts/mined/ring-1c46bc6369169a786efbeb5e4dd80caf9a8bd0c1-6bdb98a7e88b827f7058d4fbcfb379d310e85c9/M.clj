<<<<<<< A.clj
(defproject ring "1.6.0-beta6"
||||||| O.clj
(defproject ring "1.5.0"
=======
(defproject ring "1.5.1"
>>>>>>> B.clj
  :description "A Clojure web applications library."
  :url "https://github.com/ring-clojure/ring"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.5.1"]
<<<<<<< A.clj
                 [ring/ring-core "1.6.0-beta6"]
                 [ring/ring-devel "1.6.0-beta6"]
                 [ring/ring-jetty-adapter "1.6.0-beta6"]
                 [ring/ring-servlet "1.6.0-beta6"]]
||||||| O.clj
                 [ring/ring-core "1.5.0"]
                 [ring/ring-devel "1.5.0"]
                 [ring/ring-jetty-adapter "1.5.0"]
                 [ring/ring-servlet "1.5.0"]]
=======
                 [ring/ring-core "1.5.1"]
                 [ring/ring-devel "1.5.1"]
                 [ring/ring-jetty-adapter "1.5.1"]
                 [ring/ring-servlet "1.5.1"]]
>>>>>>> B.clj
  :plugins [[lein-sub "0.2.4"]
            [lein-codox "0.9.5"]]
  :sub ["ring-core"
        "ring-devel"
        "ring-jetty-adapter"
        "ring-servlet"]
  :codox {:output-path "codox"
          :source-uri "http://github.com/ring-clojure/ring/blob/{version}/{filepath}#L{line}"
          :source-paths ["ring-core/src"
                         "ring-devel/src"
                         "ring-jetty-adapter/src"
                         "ring-servlet/src"]})
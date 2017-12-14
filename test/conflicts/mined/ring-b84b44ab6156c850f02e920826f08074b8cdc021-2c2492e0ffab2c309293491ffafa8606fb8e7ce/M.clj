<<<<<<< A.clj
(defproject ring/ring-jetty-adapter "0.2.6"
||||||| O.clj
(defproject ring/ring-jetty-adapter "0.2.5"
=======
(defproject ring/ring-jetty-adapter "0.3.0-beta1"
>>>>>>> B.clj
  :description "Ring Jetty adapter."
  :url "http://github.com/mmcgrana/ring"
<<<<<<< A.clj
  :dependencies [[ring/ring-core "0.2.6"]
                 [ring/ring-servlet "0.2.6"]
||||||| O.clj
  :dependencies [[ring/ring-core "0.2.5"]
                 [ring/ring-servlet "0.2.5"]
=======
  :dependencies [[ring/ring-core "0.3.0-beta1"]
                 [ring/ring-servlet "0.3.0-beta1"]
>>>>>>> B.clj
                 [org.mortbay.jetty/jetty "6.1.14"]
                 [org.mortbay.jetty/jetty-util "6.1.14"]]
  :dev-dependencies [[lein-clojars "0.6.0"]])

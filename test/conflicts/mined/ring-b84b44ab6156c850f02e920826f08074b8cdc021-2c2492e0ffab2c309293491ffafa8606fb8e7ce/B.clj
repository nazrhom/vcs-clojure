(defproject ring/ring-jetty-adapter "0.3.0-beta1"
  :description "Ring Jetty adapter."
  :url "http://github.com/mmcgrana/ring"
  :dependencies [[ring/ring-core "0.3.0-beta1"]
                 [ring/ring-servlet "0.3.0-beta1"]
                 [org.mortbay.jetty/jetty "6.1.14"]
                 [org.mortbay.jetty/jetty-util "6.1.14"]]
  :dev-dependencies [[lein-clojars "0.6.0"]])

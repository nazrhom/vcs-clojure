(defproject ring/ring-devel "1.1.6"
  :description "Ring development and debugging libraries."
  :url "https://github.com/ring-clojure/ring"
  :dependencies [[ring/ring-core "1.1.7"]
                 [hiccup "1.0.0"]
                 [clj-stacktrace "0.2.5"]
                 [ns-tracker "0.2.0"]]
  :profiles
  {:1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
   :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}})

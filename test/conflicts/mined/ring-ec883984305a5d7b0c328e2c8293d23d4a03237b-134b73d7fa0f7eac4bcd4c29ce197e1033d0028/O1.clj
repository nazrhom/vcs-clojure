(defproject ring/ring-devel "1.1.4"
  :description "Ring development and debugging libraries."
  :url "https://github.com/ring-clojure/ring"
  :dependencies [[ring/ring-core "1.1.5"]
                 [hiccup "1.0.0"]
                 [clj-stacktrace "0.2.4"]
                 [ns-tracker "0.1.2"]]
  :profiles
  {:1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
   :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}})

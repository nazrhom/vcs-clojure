(defproject incanter/incanter-core "1.3.0-SNAPSHOT"
  :description "Incanter-core is the core module of the Incanter project."
  :dependencies [[org.clojure/clojure "1.3.0-beta3"]
                 [org.clojure/math.combinatorics "0.0.1-SNAPSHOT"]
                 [incanter/parallelcolt "0.9.4"]]
  :dev-dependencies [[lein-clojars "0.6.0"]
		     [swank-clojure "1.2.0"]]
  :java-source-path "src")

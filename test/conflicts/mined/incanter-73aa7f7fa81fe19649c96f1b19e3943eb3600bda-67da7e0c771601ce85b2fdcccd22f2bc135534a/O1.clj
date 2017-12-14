(defproject incanter "1.2.3"
  :description "Incanter is a Clojure-based, R-like statistical programming and data visualization environment."
  :dependencies [[incanter/incanter-core "1.2.4"]
                 [incanter/incanter-io "1.2.4"]
                 [incanter/incanter-charts "1.2.4"]
                 [incanter/incanter-processing "1.2.4"]
                 [incanter/incanter-mongodb "1.2.4"]
                 [incanter/incanter-pdf "1.2.4"]
                 [incanter/incanter-latex "1.2.4"]
                 [incanter/incanter-excel "1.2.4"]
                 [swingrepl "1.0.0-SNAPSHOT"]
                 [jline "0.9.94"]]
  :dev-dependencies [[lein-clojars "0.7.0"]]
  :main incanter.main)

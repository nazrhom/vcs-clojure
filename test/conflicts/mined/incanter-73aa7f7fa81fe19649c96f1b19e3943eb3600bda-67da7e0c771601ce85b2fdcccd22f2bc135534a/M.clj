<<<<<<< A.clj
(defproject incanter "1.3.0-SNAPSHOT"
||||||| O.clj
(defproject incanter "1.2.3"
=======
(defproject incanter "1.2.4"
>>>>>>> B.clj
  :description "Incanter is a Clojure-based, R-like statistical programming and data visualization environment."
<<<<<<< A.clj
  :dependencies [[incanter/incanter-core "1.3.0-SNAPSHOT"]
                 [incanter/incanter-io "1.3.0-SNAPSHOT"]
                 [incanter/incanter-charts "1.3.0-SNAPSHOT"]
                 [incanter/incanter-processing "1.3.0-SNAPSHOT"]
                 [incanter/incanter-mongodb "1.3.0-SNAPSHOT"]
                 [incanter/incanter-pdf "1.3.0-SNAPSHOT"]
                 [incanter/incanter-latex "1.3.0-SNAPSHOT"]
                 [incanter/incanter-excel "1.3.0-SNAPSHOT"]
                 [swingrepl "1.3.0"]
||||||| O.clj
  :dependencies [[incanter/incanter-core "1.2.3"]
                 [incanter/incanter-io "1.2.3"]
                 [incanter/incanter-charts "1.2.3"]
                 [incanter/incanter-processing "1.2.3"]
                 [incanter/incanter-mongodb "1.2.3"]
                 [incanter/incanter-pdf "1.2.3"]
                 [incanter/incanter-latex "1.2.3"]
                 [incanter/incanter-excel "1.2.3"]
                 [swingrepl "1.0.0-SNAPSHOT"]
=======
  :dependencies [[incanter/incanter-core "1.2.4"]
                 [incanter/incanter-io "1.2.4"]
                 [incanter/incanter-charts "1.2.4"]
                 [incanter/incanter-processing "1.2.4"]
                 [incanter/incanter-mongodb "1.2.4"]
                 [incanter/incanter-pdf "1.2.4"]
                 [incanter/incanter-latex "1.2.4"]
                 [incanter/incanter-excel "1.2.4"]
                 [swingrepl "1.0.0-SNAPSHOT"]
>>>>>>> B.clj
                 [jline "0.9.94"]]
  :dev-dependencies [[lein-clojars "0.7.0"]]
  :main incanter.main)

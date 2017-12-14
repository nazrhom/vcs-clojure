<<<<<<< A.clj
(defproject incanter "1.2.2-SNAPSHOT"
||||||| O.clj
(defproject incanter "1.2.1-SNAPSHOT"
=======
(defproject incanter "1.2.1"
>>>>>>> B.clj
  :description "Incanter is a Clojure-based, R-like statistical programming and data visualization environment."
  :dependencies [
                 [org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-master-SNAPSHOT"]
<<<<<<< A.clj
                 [incanter/incanter-core "1.2.2-SNAPSHOT"]
                 [incanter/incanter-core "1.2.2-SNAPSHOT"]
                 [incanter/incanter-io "1.2.2-SNAPSHOT"]
                 [incanter/incanter-charts "1.2.2-SNAPSHOT"]
                 [incanter/incanter-processing "1.2.2-SNAPSHOT"]
                 [incanter/incanter-mongodb "1.2.2-SNAPSHOT"]
                 [incanter/incanter-pdf "1.2.2-SNAPSHOT"]
                 [incanter/incanter-latex "1.2.2-SNAPSHOT"]]
||||||| O.clj
                 [incanter/incanter-core "1.2.1-SNAPSHOT"]
                 [incanter/incanter-core "1.2.1-SNAPSHOT"]
                 [incanter/incanter-io "1.2.1-SNAPSHOT"]
                 [incanter/incanter-charts "1.2.1-SNAPSHOT"]
                 [incanter/incanter-processing "1.2.1-SNAPSHOT"]
                 [incanter/incanter-mongodb "1.2.1-SNAPSHOT"]
                 [incanter/incanter-pdf "1.2.1-SNAPSHOT"]]
=======
                 ;[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 ;[org.clojure/clojure-contrib "1.2.0-master-SNAPSHOT"]
                 [incanter/incanter-core "1.2.1"]
                 [incanter/incanter-core "1.2.1"]
                 [incanter/incanter-io "1.2.1"]
                 [incanter/incanter-charts "1.2.1"]
                 [incanter/incanter-processing "1.2.1"]
                 [incanter/incanter-mongodb "1.2.1"]
                 [incanter/incanter-pdf "1.2.1"]]
>>>>>>> B.clj
  :dev-dependencies [[lein-clojars "0.5.0-SNAPSHOT"]
                     [leiningen/lein-swank "1.1.0"]
                     [jline "0.9.94"]])

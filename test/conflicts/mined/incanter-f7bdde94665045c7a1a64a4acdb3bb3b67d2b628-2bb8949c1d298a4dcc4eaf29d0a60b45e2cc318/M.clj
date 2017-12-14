<<<<<<< A.clj
(defproject incanter/incanter-core "1.2.1"
||||||| O.clj
(defproject incanter/incanter-core "1.2.2-SNAPSHOT"
=======
(defproject incanter/incanter-core "1.2.3-SNAPSHOT"
>>>>>>> B.clj
  :description "Incanter-core is the core module of the Incanter project."
  :dependencies [
                 [org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-master-SNAPSHOT"]
<<<<<<< A.clj
                 ;[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 ;[org.clojure/clojure-contrib "1.2.0-master-SNAPSHOT"]
                 [incanter/parallelcolt "0.7.2"]]
||||||| O.clj
                 [incanter/parallelcolt "0.7.2"]]
=======
                 [incanter/parallelcolt "0.9.4"]]
>>>>>>> B.clj
  :dev-dependencies [[lein-javac "0.0.2-SNAPSHOT"]
                     [lein-clojars "0.5.0-SNAPSHOT"]
                     [leiningen/lein-swank "1.1.0"]])

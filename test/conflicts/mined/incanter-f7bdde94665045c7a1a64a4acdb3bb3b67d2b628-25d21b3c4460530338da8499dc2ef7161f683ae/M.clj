<<<<<<< A.clj
(defproject incanter/incanter-charts "1.2.1"
||||||| O.clj
(defproject incanter/incanter-charts "1.2.2-SNAPSHOT"
=======
(defproject incanter/incanter-charts "1.2.3-SNAPSHOT"
>>>>>>> B.clj
  :description "Incanter-charts is the JFreeChart module of the Incanter project."
<<<<<<< A.clj
  :dependencies [[incanter/incanter-core "1.2.1"]
                 [incanter/jfreechart "1.0.13"]]
  :dev-dependencies [[incanter/incanter-io "1.2.1"]
||||||| O.clj
  :dependencies [[incanter/incanter-core "1.2.2-SNAPSHOT"]
                 [incanter/jfreechart "1.0.13-no-gnujaxp"]]
  :dev-dependencies [[incanter/incanter-io "1.2.2-SNAPSHOT"]
=======
  :dependencies [[incanter/incanter-core "1.2.3-SNAPSHOT"]
                 [incanter/jfreechart "1.0.13-no-gnujaxp"]]
  :dev-dependencies [[incanter/incanter-io "1.2.3-SNAPSHOT"]
>>>>>>> B.clj
                     [lein-clojars "0.5.0-SNAPSHOT"]
                     [leiningen/lein-swank "1.1.0"]])

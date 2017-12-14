;; The only requirement of the project.clj file is that it includes a
;; defproject form. It can have other code in it as well, including
;; loading other task definitions.

<<<<<<< A.clj
(defproject leiningen "2.0.0-SNAPSHOT"
||||||| O.clj
(defproject leiningen "1.6.1"
=======
(defproject leiningen "1.6.2"
>>>>>>> B.clj
  :description "Automate Clojure projects without setting your hair on fire."
  :url "https://github.com/technomancy/leiningen"
  :license {:name "Eclipse Public License"}
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [clucy "0.2.2"]
                 [lancet "1.0.1"]
                 [jline "0.9.94" :exclusions [junit]]
                 [robert/hooke "1.1.2"]
                 [org.apache.maven/maven-ant-tasks "2.1.3" :exclusions [ant]]]
  :disable-implicit-clean true
  :eval-in-leiningen true)

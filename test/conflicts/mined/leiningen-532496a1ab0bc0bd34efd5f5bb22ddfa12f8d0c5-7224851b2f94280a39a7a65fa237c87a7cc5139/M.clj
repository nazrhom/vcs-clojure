;; This is Leiningen's own project configuration. See doc/TUTORIAL.md
;; file as well as sample.project.clj for help writing your own.

(defproject leiningen "2.0.0-SNAPSHOT"
  :description "Automate Clojure projects without setting your hair on fire."
  :url "https://github.com/technomancy/leiningen"
<<<<<<< A.clj
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[leiningen-core "2.0.0-SNAPSHOT"]
                 [clucy "0.2.3"]
                 [useful "0.7.6-alpha1"]
                 [lein-newnew "0.2.4"]
                 [reply "0.1.0-alpha4"]
                 [org.clojure/data.xml "0.0.3"]
                 [bultitude "0.1.3"]]
  ;; checkout-deps don't work with :eval-in :leiningen
  :profiles {:dev {:resource-paths ["leiningen-core/dev-resources"]
                   :test-paths ["leiningen-core/test"]}}
  :test-selectors {:default (complement :busted)}
  :eval-in :leiningen)

;;; Release Checklist

;; * update NEWS, bin/lein, bin/lein.bat, project.clj, pom
;; * rm -rf target, compile :all, generate uberjar, upload
;; * test self-install
;; * git tag
;; * push, push tags, update stable branch
;; * announce on mailing list
;; * bump version numbers (bin/lein, bin/lein.bat, project.clj)
;; * regenerate pom.xml
||||||| O.clj
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
=======
  :license {:name "Eclipse Public License"}
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [clucy "0.2.2"]
                 [lancet "1.0.1"]
                 [jline "0.9.94" :exclusions [junit]]
                 [robert/hooke "1.1.2"]
                 [org.apache.maven/maven-ant-tasks "2.1.3" :exclusions [ant]]]
  :test-selectors {:offline (complement :online)}
  :disable-implicit-clean true
  :eval-in-leiningen true)
>>>>>>> B.clj

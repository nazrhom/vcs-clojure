(defproject leiningen "2.0.0-SNAPSHOT"
  :description "Automate Clojure projects without setting your hair on fire."
  :url "https://github.com/technomancy/leiningen"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[leiningen-core "2.0.0-SNAPSHOT"]
                 [clucy "0.2.3"]
                 [useful "0.7.6-alpha1"]
                 [lein-newnew "0.2.2"]
                 [reply "0.1.0-alpha4"]
                 [org.clojars.ninjudd/data.xml "0.0.1-SNAPSHOT"]]
  :test-selectors {:default (complement :busted)}
  :eval-in-leiningen true)


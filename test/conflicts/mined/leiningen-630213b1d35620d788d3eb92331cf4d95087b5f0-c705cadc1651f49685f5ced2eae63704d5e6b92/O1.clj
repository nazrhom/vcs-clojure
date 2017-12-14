(def default-profiles
  "Profiles get merged into the project map. The :dev and :user
  profiles are active by default."
  (atom {:default {:resources-path ["dev-resources"]
                   :dependencies '[[org.clojure/tools.nrepl "0.0.5"
                                    :exclusions [org.clojure/clojure]]
                                   [clojure-complete "0.1.4"
                                    :exclusions [org.clojure/clojure]]
                                   [org.thnetos/cd-client "0.3.3"
                                    :exclusions [org.clojure/clojure]]]}
         :test {:test-path ["test"]}
         :debug {:debug true}}))


(def default-profiles
  "Profiles get merged into the project map. The :dev and :user
  profiles are active by default."
  (atom {:default {:resources-path ["dev-resources"]
                   :test-path ["test"]}
         :test {}
         :debug {:debug true}}))


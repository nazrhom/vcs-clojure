(def default-profiles
  "Profiles get merged into the project map. The :dev, :provided, and :user
  profiles are active by default."
  (atom {:default [:base :system :user :provided :dev]
         :base {:resource-paths ["dev-resources"]
                :jvm-opts tiered-jvm-opts
                :checkout-deps-shares [:source-paths
                                       :test-paths
                                       :resource-paths
                                       :compile-path]}
         :leiningen/test {:injections [hooke-injection]
                          :test-selectors {:default (with-meta
                                                      '(constantly true)
                                                      {:displace true})}}
         :update {:update :always}
         :offline {:offline? true}
         :debug {:debug true}}))


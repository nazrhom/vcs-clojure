(defn- warn-user-repos []
  (when (->> (vals (user/profiles))
             (map (comp second :repositories))
             (apply concat) (some :url))
    (println "WARNING: :repositories detected in user-level profile!")
    (println "See https://github.com/technomancy/leiningen/wiki/Repeatability")))


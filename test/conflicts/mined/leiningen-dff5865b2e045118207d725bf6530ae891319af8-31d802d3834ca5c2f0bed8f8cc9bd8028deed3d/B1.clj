(defn- warn-user-repos []
  (when (->> (vals (user/profiles))
             (map (comp second :repositories))
             (apply concat) (some :url))
    (log/warn 
      ":repositories detected in user-level profile!\n"
      "See https://github.com/technomancy/leiningen/wiki/Repeatability")))


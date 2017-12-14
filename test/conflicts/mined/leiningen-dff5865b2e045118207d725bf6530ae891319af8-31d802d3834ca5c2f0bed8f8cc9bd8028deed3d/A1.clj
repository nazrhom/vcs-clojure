(defn- warn-user-repos [profiles]
  (let [has-url? (fn [entry] (or (string? entry) (:url entry)))
        profiles (filter #(->> (second %)
                               :repositories
                               (map second)
                               (some has-url?))
                         profiles)]
    (when (and (seq profiles)
               (not (System/getenv "LEIN_SUPPRESS_USER_LEVEL_REPO_WARNINGS")))
      (println
       "WARNING: :repositories detected in user-level profiles!"
       (vec (map first profiles)))
      (println "See https://github.com/technomancy/leiningen/wiki/Repeatability"))))


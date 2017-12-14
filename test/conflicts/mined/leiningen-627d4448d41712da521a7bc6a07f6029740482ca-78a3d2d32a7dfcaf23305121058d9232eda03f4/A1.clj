(defn profile-scope-target-path [project profiles]
  (let [n #(if (map? %) (subs (sha1 (pr-str %)) 0 8) (name %))]
    (if (:target-path project)
      (update-in project [:target-path] format
                 (str/join "+" (map n (remove #{:default :provided} profiles))))
      project)))
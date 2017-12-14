(defn make-target-repo [repo-url auth-options]
  (let [repo (make-repository ["remote repository" repo-url])]
    (when-let [auth (make-auth repo-url auth-options)]
      (.addAuthentication repo auth))
    repo))


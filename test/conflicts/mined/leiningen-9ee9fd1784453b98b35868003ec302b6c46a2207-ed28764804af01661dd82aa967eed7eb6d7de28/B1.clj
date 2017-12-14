(defn make-auth [url options]
  (let [auth (Authentication.)
        user-options (when-let [user-opts (resolve 'user/leiningen-auth)]
                       (get @user-opts url))
        {:keys [username password passphrase
                private-key]} (merge user-options options)]
    (when username (.setUserName auth username))
    (when password (.setPassword auth password))
    (when passphrase (.setPassphrase auth passphrase))
    (when private-key (.setPrivateKey auth private-key))
    auth))

(defn make-target-repo [project options]
  (let [deploy-opts (merge (:deploy-to project) options)
        repo-url (if (snapshot? project)
                   (:snapshots deploy-opts)
                   (:releases deploy-opts))
        repo (make-repository ["remote repository" repo-url])]
    (when-let [auth (make-auth repo-url options)]
      (.addAuthentication repo auth))
    repo))


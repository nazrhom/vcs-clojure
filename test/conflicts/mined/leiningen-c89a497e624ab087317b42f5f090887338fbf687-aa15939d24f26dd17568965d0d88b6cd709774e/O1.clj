(defn get-proxy-settings
  "Returns a map of the JVM proxy settings"
  []
  (when-let [proxy (System/getenv "http_proxy")]
    (let [url (try (URL. proxy)
                   (catch java.net.MalformedURLException _
                     (URL. (str "http://" proxy))))
          user-info (.getUserInfo url)
          [username password] (and user-info (.split user-info ":"))]
      {:host (.getHost url)
       :port (.getPort url)
       :username username
       :password password})))


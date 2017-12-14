(defn localhost
  "Returns the local host name."
  []
  (try
    (.. InetAddress getLocalHost getHostName)
    (catch Exception _
      (warn "could not determine local host name")
      "localhost")))


(defn localhost
  "Returns the local host name."
  []
  (.. InetAddress getLocalHost getHostName))


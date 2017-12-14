(defn registry-builder []
  (-> (RegistryBuilder/create)
      (.register "http" PlainConnectionSocketFactory/INSTANCE)
      (.register "https" insecure-socket-factory)
      (.build)))


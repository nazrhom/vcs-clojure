(defn watch-and-reload-with-opts [opts]
  (defonce watch-and-reload-singleton
    (watch-and-reload*
     (merge { :retry-count 100 
              :jsload-callback default-jsload-callback
              :websocket-url (str "ws:" js/location.host "/figwheel-ws")}
            opts))))

(defn watch-and-reload [& opts]
  (watch-and-reload-with-opts opts))


(defn watch-and-reload-with-opts [opts]
  (defonce watch-and-reload-singleton
    (watch-and-reload* (merge { :retry-count 100 
                                :jsload-callback (fn [url]
                                                  (.dispatchEvent (.querySelector js/document "body")
                                                                  (js/CustomEvent. "figwheel.js-reload"
                                                                                   (js-obj "detail" url))))
                                :websocket-url (str "ws://" js/location.host "/figwheel-ws")}
                              opts))))


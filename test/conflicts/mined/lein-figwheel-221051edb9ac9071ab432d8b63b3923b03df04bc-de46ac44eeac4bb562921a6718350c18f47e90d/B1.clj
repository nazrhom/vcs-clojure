(defn start-static-server [{:keys [js-dirs http-server-root] :as opts}]
  (let [http-s-root (or http-server-root "public")]
    (start-server (merge opts {:ring-handler
                               (routes
                                (GET "/" [] (resource-response "index.html" {:root http-s-root}))
                                (route/resources "/" :root http-s-root)
                                (route/not-found "<h1>Page not found</h1>"))
                               :http-server-root http-s-root}))))


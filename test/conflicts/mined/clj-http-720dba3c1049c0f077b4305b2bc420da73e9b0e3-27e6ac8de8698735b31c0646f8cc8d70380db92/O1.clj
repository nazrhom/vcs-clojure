(defn wrap-query-params
  "Middleware converting the :query-params option to a querystring on
  the request."
  [client]
  (fn [{:keys [query-params content-type]
       :or {content-type :x-www-form-urlencoded}
       :as req}]
    (if query-params
      (client (-> req (dissoc :query-params)
                  (update-in [:query-string]
                             (fn [old-query-string new-query-string]
                               (if-not (empty? old-query-string)
                                 (str old-query-string "&" new-query-string)
                                 new-query-string))
                             (generate-query-string
                              query-params
                              (content-type-value content-type)))))
      (client req))))


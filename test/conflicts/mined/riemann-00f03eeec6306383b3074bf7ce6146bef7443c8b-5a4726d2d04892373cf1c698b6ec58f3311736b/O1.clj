(defn influxdb-9
  "Returns a function which accepts an event, or sequence of events, and writes
  them to InfluxDB. Compatible with the 0.9.x series.
  (influxdb-9 {:host \"influxdb.example.com\"
               :db \"my_db\"
               :retention \"raw\"
               :tag-fields #{:host :sys :env}})
  0.9 Options:
  `:retention`      Name of retention policy to use. (optional)
  `:tag-fields`     A set of event fields to map into InfluxDB series tags.
                    (default: `#{:host}`)
  `:tags`           A common map of tags to apply to all points. (optional)
  `:timeout`        HTTP timeout in milliseconds. (default: `5000`)"
  [opts]
  (let [write-url
        (str (cond->
          (format "%s://%s:%s/write?db=%s&precision=s" (:scheme opts) (:host opts) (:port opts) (:db opts))
          (:retention opts)
            (str "&rp=" (:retention opts))))

        http-opts
        (cond->
          {:socket-timeout (:timeout opts 5000) ; ms
           :conn-timeout   (:timeout opts 5000) ; ms
           :content-type   "text/plain"}
          (:username opts)
            (assoc :basic-auth [(:username opts)
                                (:password opts)]))

        tag-fields
        (:tag-fields opts #{:host})]
    (fn stream
      [events]
      (let [events (if (sequential? events) events (list events))
            points (events->points-9 tag-fields events)]
        (http/post write-url
          (assoc http-opts :body (->> points
            (map lineprotocol-encode-9)
            (clojure.string/join "\n"))))))))


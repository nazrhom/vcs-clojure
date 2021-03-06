(def ^{:doc "available logging patterns"}
  layouts
  {:riemann       (EnhancedPatternLayout. "%p [%d] %t - %c - %m%n%throwable")
   :json-event    (JSONEventLayoutV0.)
   :json-event-v0 (JSONEventLayoutV0.)
   :json-event-v1 (JSONEventLayoutV1.)})

(defn get-layout
  "Fetch a logging layout by name"
  [layout-name]
  (let [layout (get layouts (or layout-name :riemann))]
    (when (nil? layout)
      (binding [*out* *err*] (println "invalid logging layout specified: " layout-name)))
    (or layout (:riemann layouts))))


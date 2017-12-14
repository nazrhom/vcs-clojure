(defn start-figwheel!
  [{:keys [figwheel-options all-builds build-ids] :as options}]
  (let [system (create-figwheel-system options)]
    (try
      (component/start system)
      (catch Throwable e
        (let [orig-exception  (unest-component-exception e)
              [escape reason] ((juxt :escape-system-exceptions :reason)
                               (ex-data orig-exception))]
          (if escape
            (do (println (.getMessage orig-exception))
                (when-not (= reason :initial-cljs-build-exception)
                  (throw (.getCause orig-exception))))
            (throw e)))))))


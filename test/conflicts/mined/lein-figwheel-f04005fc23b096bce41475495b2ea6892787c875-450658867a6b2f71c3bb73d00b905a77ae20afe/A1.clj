(defn start-figwheel!
  [{:keys [figwheel-options all-builds build-ids] :as options}]
  #_(vc/validate-api-config! options)
  (let [options (update-in options [:all-builds] config/prep-builds)
        system (create-figwheel-system options)]
    (component/start system)))


(defn run-compiler [project {:keys [data] :as figwheel-internal-config-data}]
  (run-local-project
   project (get data :all-builds)
   '(require 'figwheel-sidecar.repl-api)
   `(do
      (figwheel-sidecar.repl-api/system-asserts)
      (figwheel-sidecar.repl-api/start-figwheel-from-lein '~figwheel-internal-config-data))))

(defn figwheel
  "Autocompile ClojureScript and serve the changes over a websocket (+ plus static file server)."
  [project & build-ids]
  (fc/system-asserts)
  (when-let [config-data (validate-figwheel-conf project)]
    (let [{:keys [data] :as figwheel-internal-data}
          (-> config-data
              fc/config-data->figwheel-internal-config-data
              fc/prep-builds)
          {:keys [figwheel-options all-builds]} data
          ;; TODO this is really outdated
          errors (fc/check-config figwheel-options
                                  (fc/narrow-builds*
                                   all-builds
                                   build-ids))
          figwheel-internal-final (fc/populate-build-ids figwheel-internal-data build-ids)]
      #_(pp/pprint figwheel-internal-final)
      (if (empty? errors)
        (run-compiler project figwheel-internal-final)
        (mapv println errors)))))

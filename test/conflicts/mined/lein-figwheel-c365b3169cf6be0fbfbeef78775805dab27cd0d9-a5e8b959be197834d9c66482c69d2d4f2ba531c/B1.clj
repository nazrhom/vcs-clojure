(defn run-compiler [project {:keys [all-builds build-ids] :as autobuild-opts}]
  (if (empty? all-builds)
    (do
      (println "\nFigwheel: No builds detected. Exiting ...")
      (System/exit 1))
    (run-local-project
     project all-builds
     '(require 'figwheel-sidecar.repl-api)
     `(do
        (figwheel-sidecar.repl-api/system-asserts)
        (figwheel-sidecar.repl-api/start-figwheel-from-lein '~autobuild-opts)))))

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
        (run-compiler project
                      { :figwheel-options figwheel-options
                        :all-builds all-builds
                        :build-ids  (vec build-ids)})
        (mapv println errors)))))

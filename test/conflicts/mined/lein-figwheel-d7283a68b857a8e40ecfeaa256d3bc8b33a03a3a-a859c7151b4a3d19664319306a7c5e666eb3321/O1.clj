(defn validate-figwheel-conf-helper []
  (when-let [validate-loop (resolve 'figwheel-sidecar.config-check.validate-config/validate-loop)]
    (if (figwheel-edn?)
      (validate-loop
        (repeatedly #(slurp "figwheel.edn"))
        {:file (io/file "figwheel.edn")
         :figwheel-options-only true})
      (validate-loop
        (cons project
              (repeatedly #(lproj/set-profiles (lproj/read)
                                               (:included-profiles (meta project))
                                               (:excluded-profiles (meta project)))))
        {:file (io/file "project.clj")}))))


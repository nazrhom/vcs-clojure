(defn prep [project]
  ;; This must exist before the project is launched.
  (.mkdirs (io/file (:compile-path project "/tmp")))
  (try (classpath/get-classpath project)
       (catch DependencyResolutionException e
         (main/info (.getMessage e))
         (main/info "Check :dependencies and :repositories for typos.")
         (main/info "It's possible the specified jar is not in any repository.")
         (main/info "If so, see \"Free-floating Jars\" under http://j.mp/repeatability")
         (main/abort)))
  (prep-tasks project)
  (.mkdirs (io/file (:compile-path project "/tmp")))
  (when-let [prepped (:prepped (meta project))]
    (deliver prepped true)))


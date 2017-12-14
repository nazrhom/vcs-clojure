(defn -main
  "Command-line entry point."
  [& raw-args]
  (try
    (user/init)
    (let [project (project/init-project
                   (if (.exists (io/file "project.clj"))
                     (project/read)
                     (assoc (project/make (:user (user/profiles)))
                       :eval-in :leiningen :prep-tasks [])))
          [task-name args] (task-args raw-args project)]
      (when (:min-lein-version project) (verify-min-version project))
      (configure-http)
      (resolve-and-apply project raw-args))
    (catch Exception e
      (if (or *debug* (not (:exit-code (ex-data e))))
        (.printStackTrace e)
        (when-not (:suppress-msg (ex-data e))
          (println (.getMessage e))))
      (exit (:exit-code (ex-data e) 1))))
  (exit 0))

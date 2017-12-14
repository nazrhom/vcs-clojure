(defmethod eval-in :leiningen [project form]
  (when (:debug project)
    (System/setProperty "clojure.debug" "true"))
  ;; :dependencies are loaded the same way as plugins in eval-in-leiningen
  (project/load-plugins project :dependencies)
  (project/init-lein-classpath project)
  (doseq [opt (get-jvm-args project)
          :when (.startsWith opt "-D")
          :let [[_ k v] (re-find #"^-D(.*?)=(.*)$" opt)]]
    (System/setProperty k v))
  (eval form))


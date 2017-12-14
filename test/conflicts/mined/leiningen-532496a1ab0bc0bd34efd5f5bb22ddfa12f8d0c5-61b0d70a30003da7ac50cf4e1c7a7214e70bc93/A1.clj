(defn get-jar-filename
  ([project uberjar?]
     (let [target (doto (io/file (:target-path project)) .mkdirs)
           suffix (if uberjar? "-standalone.jar" ".jar")
           jar-name (or (project (if uberjar? :uberjar-name :jar-name))
                        (str (:name project) "-" (:version project) suffix))]
       (str (io/file target jar-name))))
  ([project] (get-jar-filename project false)))


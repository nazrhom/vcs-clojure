(defn get-jar-filename
  ([project jar-name]
     (let [target-dir (:target-dir project)]
       (.mkdirs (file target-dir))
       (str target-dir "/" jar-name)))
  ([project] (get-jar-filename project (get-default-jar-name project))))

(defn get-default-uberjar-name [project]
  (or (:uberjar-name project)
      (str (:name project) \- (:version project) "-standalone.jar")))

(defn- filespecs [project deps-fileset]
  (let [javasrc (:java-source-path project)
        cljsrc (:source-path project)]
    (concat
     [{:type :bytes
       :path (format "META-INF/maven/%s/%s/pom.xml"
                     (:group project)
                     (:name project))
       :bytes (make-pom project)}
      {:type :bytes
       :path (format "META-INF/maven/%s/%s/pom.properties"
                     (:group project)
                     (:name project))
       :bytes (make-pom-properties project)}
      {:type :path :path (:compile-path project)}
      {:type :path :path (str (:root project) "/project.clj")}]
     (when (and (:resources-path project)
                (.exists (file (:resources-path project))))
       [{:type :path :path (:resources-path project)}])
     (when (and javasrc
                (not (:omit-source project))
                (not (.startsWith javasrc cljsrc))
                (not (.startsWith cljsrc javasrc)))
       [{:type :path :path javasrc}])
     (when-not (:omit-source project)
       [{:type :path :path cljsrc}])
     (shell-wrapper-filespecs project deps-fileset))))

(defn extract-jar
  "Unpacks jar-file into target-dir. jar-file can be a JarFile
  instance or a path to a jar file on disk."
  [jar-file target-dir]
  (let [jar (if (isa? jar-file JarFile)
              jar-file
              (JarFile. jar-file true))
        entries (enumeration-seq (.entries jar))
        target-file #(file target-dir (.getName %))]
    (doseq [entry entries :when (not (.isDirectory entry))
            :let [f (target-file entry)]]
      (.mkdirs (.getParentFile f))
      (copy (.getInputStream jar entry) f))))


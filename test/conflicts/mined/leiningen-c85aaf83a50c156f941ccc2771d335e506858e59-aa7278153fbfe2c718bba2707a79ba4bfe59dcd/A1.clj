(defn local-repo-path
  ([group name version]
     (local-repo-path {:group group :name name :version version}))
  ([{:keys [group name version]}]
     (format "$HOME/.m2/repository/%s/%s/%s/%s-%s.jar"
             (.replaceAll group "\\." "/") name version name version)))

(defn- script-classpath-for [project deps-fileset]
  (string/join ":" (conj (for [dep (when (:dependencies project)
                                     (-> deps-fileset
                                         (.getDirectoryScanner ant-project)
                                         (.getIncludedFiles)))]
                           (format "$HOME/.m2/repository/%s" dep))
                         (local-repo-path project))))


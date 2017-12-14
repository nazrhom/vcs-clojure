(defn local-repo-path
  ([group name version]
     (local-repo-path {:group group :name name :version version}))
  ([{:keys [group name version]}]
     (unix-path (format
                 "$HOME/.m2/repository/%s/%s/%s/%s-%s.jar"
                 (.replace group "." "/") name version name version))))

(defn- script-classpath-for [project deps-fileset system]
  (let [deps (-> deps-fileset
                 (.getDirectoryScanner lancet/ant-project)
                 (.getIncludedFiles))
        unix-paths (conj (for [dep deps]
                           (unix-path (format "$HOME/.m2/repository/%s" dep)))
                         (local-repo-path project))]
    (case system
          :unix (string/join ":" unix-paths)
          :windows (string/join ";" (for [path unix-paths]
                                      (windows-path
                                       (.replace path "$HOME"
                                                 "%USERPROFILE%")))))))


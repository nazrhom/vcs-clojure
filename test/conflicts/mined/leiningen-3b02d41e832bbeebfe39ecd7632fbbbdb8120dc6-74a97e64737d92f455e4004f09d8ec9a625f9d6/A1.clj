(defn- get-jvm-args [project]
  (remove empty?
          `(~@(when-let [opts (System/getenv "JVM_OPTS")] [opts])
            ~@(:jvm-opts project)
            ~@(:jvm-opts (user-settings)))))


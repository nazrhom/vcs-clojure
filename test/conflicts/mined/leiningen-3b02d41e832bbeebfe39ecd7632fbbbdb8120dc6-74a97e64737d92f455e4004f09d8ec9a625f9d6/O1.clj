;; Split this function out for better testability.
(defn- get-raw-input-args []
  (.getInputArguments (ManagementFactory/getRuntimeMXBean)))

(defn ^{:internal true} get-input-args
  "Returns a vector of input arguments, accounting for a bug in RuntimeMXBean
  that splits arguments which contain spaces. Removes -Xbootclasspath as it
  requires special handling on the called code."
  []
  ;; RuntimeMXBean.getInputArguments() is buggy when an input argument
  ;; contains spaces. For an input argument of -Dprop="hello world" it
  ;; returns ["-Dprop=hello", "world"]. Try to work around this bug.
  (letfn [(join-broken-args [v arg] (if (= \- (first arg))
                                      (conj v arg)
                                      (conj (vec (butlast v))
                                            (format "%s %s" (last v) arg))))]
    (remove #(re-find #"^-Xbootclasspath.+" %)
            (reduce join-broken-args [] (get-raw-input-args)))))

(defn- get-jvm-args [project]
  (concat (get-input-args) (:jvm-opts project) (:jvm-opts (user-settings))))


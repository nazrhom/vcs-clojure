(defn validate-loop [projects options]
  (let [{:keys [figwheel-options-only file]} options]
    (if-not (.exists (io/file file))
      (do
        (println "Configuration file" (str (:file options)) "was not found")
        (System/exit 1))
      (let [file (io/file file)]
        (println "Figwheel: Validating the configuration found in" (str file))
        (loop [fix false]
          (let [config (get-data-fn)]
            (if (and config
                     (not (validate-config-data config figwheel-options-only)))
              config
              (do
                (try (.beep (java.awt.Toolkit/getDefaultToolkit)) (catch Exception e))
                (println (color-text (str "Figwheel: There are errors in your configuration file - " (str file)) :red))
                (let [choice (or (and fix "f")
                                 (do
                                   (println "Figwheel: Would you like to:")
                                   (println "(f)ix the error live while Figwheel watches for config changes?")
                                   (println "(q)uit and fix your configuration?")
                                   (println "(s)tart Figwheel anyway?")
                                   (print "Please choose f, q or s and then hit Enter [f]: ")
                                   (flush)
                                   (get-choice ["f" "q" "s"])))]
                  (condp = choice
                    nil false
                    "q" false
                    "s" config
                    "f" (if (:file options)
                          (do
                            (println "Figwheel: Waiting for you to edit and save your" (str file) "file ...")
                            (file-change-wait file (* 120 1000))
                            (recur true (rest projects)))
                          (do ;; this branch shouldn't be taken
                            (Thread/sleep 1000)
                            (recur true)))))))
            ))))))

(defn color-validate-loop [get-data-fn options]
  (with-color
    (validate-loop get-data-fn options)))


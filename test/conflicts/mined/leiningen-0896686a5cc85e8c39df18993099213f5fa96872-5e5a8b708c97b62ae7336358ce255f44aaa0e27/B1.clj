(defn exit
  "Exit the process. Rebind *exit-process?* in order to suppress actual process
  exits for tools which may want to continue operating. Never call
  System/exit directly in Leiningen's own process."
  ([exit-code & msg]
     (if *exit-process?*
       (do (shutdown-agents)
           (System/exit exit-code))
       (throw (ex-info (if (seq msg)
                         (apply print-str msg)
                         "Suppressed exit")
                {:exit-code exit-code}))))
  ([] (exit 0)))


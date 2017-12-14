(def figwheel-options-rules
  (doall
   (distinct
    (concat
     (or-spec 'HawkWatcher :barbary :java :polling)
     (spec 'HawkOptionsMap {:watcher (ref-schema 'HawkWatcher)})
     (or-spec 'ReloadCljFiles
              (ref-schema 'Boolean)
              {:clj  (ref-schema 'Boolean)
               :cljc (ref-schema 'Boolean)})
     (spec 'FigwheelOptions
          {:http-server-root  string?
           ; :builds is added below
           :server-port       integer?
           :server-ip         string?
           :css-dirs          [string?]
           :ring-handler      (ref-schema 'Named)
           :builds-to-start   [(ref-schema 'Named)]
           :reload-clj-files  (ref-schema 'ReloadCljFiles)
           :server-logfile    string?
           :open-file-command string?
           :repl              (ref-schema 'Boolean)
           :nrepl-port        integer?
           :nrepl-host        string?
           :hawk-options      (ref-schema 'HawkOptionsMap)
           :nrepl-middleware  [(ref-schema 'Named)]
           :validate-config   (ref-schema 'Boolean)
           :load-all-builds   (ref-schema 'Boolean)})
    (spec 'HawkOptionsMap {:watcher (ref-schema 'HawkWatcher)})
    (or-spec 'HawkWatcher :barbary :java :polling)
    (or-spec 'ReloadCljFiles
             (ref-schema 'Boolean)
             {:clj  (ref-schema 'Boolean)
              :cljc (ref-schema 'Boolean)})
    (spec 'CljsbuildOptions
          {
           :builds               (ref-schema 'CljsBuilds)
           :repl-listen-port     integer?
           :repl-launch-commands {named? [(ref-schema 'Named)]}
           :test-commands        {named? [(ref-schema 'Named)]}
           :crossovers           [anything?]
           :crossover-path       [anything?]
           :crossover-jar        (ref-schema 'Boolean)})
    (or-spec 'CljsBuilds
             [(ref-schema 'BuildOptionsMap)]
             {named? (ref-schema 'BuildOptionsMap)})
    (get-docs
     ['CljsbuildOptions])))))


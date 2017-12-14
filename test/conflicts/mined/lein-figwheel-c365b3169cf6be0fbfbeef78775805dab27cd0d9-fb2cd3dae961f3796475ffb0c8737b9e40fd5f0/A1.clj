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
           :validate-config   (ref-schema 'Boolean)})
         (get-docs ['FigwheelOptions])))))

(def build-options-rules
  (doall
   (distinct
    (concat
     (or-spec 'WebsocketHost :js-client-host :server-ip :server-hostname string?)
     (or-spec 'FigwheelClientOptions
              (ref-schema 'Boolean)
              {:build-id            string?
               :websocket-host      (ref-schema 'WebsocketHost)
               :websocket-url       string?
               :on-jsload           (ref-schema 'Named)
               :before-jsload       (ref-schema 'Named)
               :on-cssload          (ref-schema 'Named)
               :on-message          (ref-schema 'Named)
               :on-compile-fail     (ref-schema 'Named)
               :on-compile-warning  (ref-schema 'Named)
               :reload-dependents   (ref-schema 'Boolean)
               :debug               (ref-schema 'Boolean)
               :autoload            (ref-schema 'Boolean)
               :heads-up-display    (ref-schema 'Boolean)
               :load-warninged-code (ref-schema 'Boolean)
               :retry-count         integer?
               :devcards            (ref-schema 'Boolean)
               :eval-fn             (ref-schema 'Named)})
     (spec 'BuildOptionsMap
           {:id              (ref-schema 'Named)
            :source-paths    [string?]
            :figwheel        (ref-schema 'FigwheelClientOptions)
            ;:compiler        (ref-schema 'CompilerOptions)
            :notify-command  [string?]
            :jar             (ref-schema 'Boolean)
            :incremental     (ref-schema 'Boolean)
            :assert          (ref-schema 'Boolean)
            :warning-handlers [anything?]})
     (assert-not-empty 'BuildOptionsMap :source-paths)
     (requires-keys 'BuildOptionsMap :source-paths #_:compiler)
     (get-docs ['CompilerOptions
                'FigwheelClientOptions
                'BuildOptionsMap
                'ReloadCljFiles])))))

;; this is not good I'm having to inject rules for build-options
;; versus compiler
(defn build-options-map-compiler-options [key]
  {:pre [(keyword? key)]
   :post [(sequence %)]}
  (distinct
   (concat
    (spec 'BuildOptionsMap {key (ref-schema 'CompilerOptions)})
    (requires-keys 'BuildOptionsMap key))))

(def common-validation-rules-base
  (doall
   (distinct
    (concat
     shared-type-rules
     cljs-compiler-rules
     figwheel-options-rules
     build-options-rules))))

(def figwheel-cljsbuild-rules
  (doall
   (distinct
    (concat
     common-validation-rules-base
     (spec 'CljsbuildOptions
          {:builds               (ref-schema 'CljsBuilds)
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


(defn ^:no-project-needed repl
  "Start a repl session either with the current project or standalone.

Subcommands:

<none> -> :start

:start [:port port] This will launch an nREPL server and connect a
  client to it. If the :port key is specified, or present under
  :repl-options in the project map, that port will be used for the
  server, otherwise it is chosen randomly.  When starting outside of a
  project, the nREPL server will run internally to Leiningen.

:headless [:port port]
  This will launch an nREPL server and wait, rather than connecting
  a client to it.

:connect [dest]
  Connects to an already running nREPL server. Dest can be:
  - an HTTP URL -- connects to an HTTP nREPL endpoint;
  - host:port -- connects to the specified host and port;
  - port -- resolves host from the LEIN_REPL_HOST environment
      variable or :repl-options, in that order, and defaults to
      localhost.
  If no dest is given, resolves the port from :repl-options and the host
  as described above."
  ([project] (repl project ":start"))
  ([project subcommand & opts]
     (let [host (repl-host project)
           port (or (opt-port opts) (repl-port project))]
       (case subcommand
         ":start" (if trampoline/*trampoline?*
                    (trampoline-repl project port)
                    (let [port (server project false)]
                      (client project host port)))
         ":headless" (start-server project (ack-port project) true)
         ":connect" (client project host (or (first opts) port))
         (main/abort "Unknown subcommand")))))

;; A note on testing the repl task: it has a number of modes of operation
;; which need to be tested individually:
;; * :start (normal operation)
;; * :headless (server-only)
;; * :connect (client-only)
;; These three modes should really each be tested in each of these contexts:
;; * :eval-in :subprocess (default)
;; * :eval-in :trampoline
;; * :eval-in :leiningen (:connect prolly doesn't need separate testing here)
;; Visualizing a 3x3 graph with checkboxes is an exercise left to the reader.
;; Possibly worth testing in TERM=dumb (no completion) as well as a regular
;; terminal, but that doesn't need to happen separately for each
;; subcommand. This is more about testing reply than the repl task itself.

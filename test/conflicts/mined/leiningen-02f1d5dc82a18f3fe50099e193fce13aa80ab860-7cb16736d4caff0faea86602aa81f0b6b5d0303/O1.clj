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
         ":headless" (start-server project nil true)
         ":connect" (client project host (or (first opts) port))
         (main/abort "Unknown subcommand")))))

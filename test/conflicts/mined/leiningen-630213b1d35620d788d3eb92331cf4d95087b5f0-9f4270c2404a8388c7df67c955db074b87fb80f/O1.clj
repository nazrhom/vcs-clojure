(ns leiningen.repl
  "Start a repl session either with the current project or standalone."
  (:require clojure.main
            [reply.main :as reply]
            [clojure.java.io :as io]
            [leiningen.core.eval :as eval]
            [clojure.tools.nrepl :as nrepl]
            [leiningen.core.user :as user]
            [leiningen.core.classpath :as classpath]))

(defn- start-server [project port ack-port]
  (if project
    (eval/eval-in-project project
                          `(do (clojure.tools.nrepl.server/start-server
                                 :port ~port :ack-port ~ack-port))
                          '(do (require 'clojure.tools.nrepl.server)
                               (require 'complete.core)))
    (nrepl.server/start-server :port port :ack-port ack-port)))

(defn ^:no-project-needed repl
  "Start a repl session either with the current project or standalone.

This will launch an nREPL server behind the scenes that reply will connect to.
If a :repl-port key is present in project.clj, that port will be used for the
server, otherwise it is chosen randomly. If you run this command inside of a
project, it will be run in the context of that classpath. If the command is
run outside of a project, it'll be standalone and the classpath will be
that of Leiningen."
  ([] (repl nil))
  ([project]
   (nrepl.ack/reset-ack-port!)
   (.start
     (Thread.
       (bound-fn []
         (start-server project
                       (Integer.
                         (or (System/getenv "LEIN_REPL_PORT")
                             (:repl-port project)
                             0))
                       (-> @lein-repl-server deref :ss .getLocalPort)))))
   (reply/launch-nrepl
     (merge
       {:attach (str (nrepl/wait-for-ack (or (:repl-timeout project)
                                             (:repl-timeout (user/settings))
                                             30000)))}
       (:reply-options (user/settings))
       (:reply-options project)))))

(ns leiningen.repl
  "Start a repl session either with the current project or standalone."
  (:require clojure.main
            [reply.main :as reply]
            [clojure.java.io :as io]
            [leiningen.core.eval :as eval]
            [leiningen.core.project :as project]
            [clojure.tools.nrepl.ack :as nrepl.ack]
            [clojure.tools.nrepl.server :as nrepl.server]
            [leiningen.core.user :as user]
            [leiningen.core.classpath :as classpath]))

(def profile {:dependencies '[[org.clojure/tools.nrepl "0.2.0-beta1"
                               :exclusions [org.clojure/clojure]]
                              [clojure-complete "0.2.1"
                               :exclusions [org.clojure/clojure]]
                              [org.thnetos/cd-client "0.3.3"
                               :exclusions [org.clojure/clojure]]]})

(defn- start-server [project port ack-port]
  (if project
    (eval/eval-in-project (project/merge-profile project profile)
                          `(do (clojure.tools.nrepl.server/start-server
                                 :port ~port :ack-port ~ack-port))
                          '(do (require 'clojure.tools.nrepl.server)
                               (require 'complete.core)))
    (nrepl.server/start-server :port port :ack-port ack-port)))

(def lein-repl-server
  (delay (nrepl.server/start-server
           :handler (nrepl.ack/handle-ack nrepl.server/unknown-op))))

<<<<<<< A.clj
(defn ^:no-project-needed repl
||||||| O.clj
(def retry-limit 200)

(defn repl-options [project options]
  (let [options (apply hash-map options)
        init `#(let [is# ~(:repl-init-script project)
                     in# '~(:repl-init project)
                     mn# '~(:main project)]
                 ~(:init options)
                 (when (and is# (.exists (File. (str is#))))
                   (println (str "Warning: :repl-init-script is "
                                 "deprecated; use :repl-init."))
                   (load-file is#))
                 (when in#
                   (require in#))
                 (when mn#
                   (require mn#))
                 (in-ns (or in# mn# '~'user)))
        ;; Suppress socket closed since it's part of normal operation
        caught `(fn [t#]
                  (when-not (instance? SocketException t#)
                    (~(:caught options 'clojure.main/repl-caught) t#)))
        ;; clojure.main/repl has no way to exit without signalling EOF,
        ;; which we can't do with a socket. We can't rebind skip-whitespace
        ;; in Clojure 1.3, so we have to duplicate the function
        read `(fn [request-prompt# request-exit#]
                (or ({:line-start request-prompt# :stream-end request-exit#}
                     (try (clojure.main/skip-whitespace *in*)
                          (catch java.io.IOException _#
                            :stream-end)))
                    (let [input# (read)]
                      (clojure.main/skip-if-eol *in*)
                      input#)))]
    (apply concat [:init init :caught caught :read read]
           (dissoc options :caught :init :read))))

(defn repl-server [project host port & options]
  `(do (try ;; transitive requires don't work for stuff on bootclasspath
         (require '~'clojure.java.shell)
         (require '~'clojure.java.browse)
         ;; these are new in clojure 1.2, so swallow exceptions for 1.1
         (catch Exception _#))
       (set! *warn-on-reflection* false)
       (let [server# (ServerSocket. ~port 0 (InetAddress/getByName ~host))
             acc# (fn [s#]
                    (let [ins# (.getInputStream s#)
                          outs# (.getOutputStream s#)
                          out-writer# (OutputStreamWriter. outs#)]
                      (doto (Thread.
                             #(binding [*in* (-> ins# InputStreamReader.
                                                 LineNumberingPushbackReader.)
                                        *out* out-writer#
                                        *err* (PrintWriter. out-writer#)
                                        *warn-on-reflection*
                                        ~(:warn-on-reflection project)]
                                (clojure.main/repl
                                 ~@(repl-options project options))))
                        .start)))]
         (doto (Thread. #(when-not (.isClosed server#)
                           (try
                             (acc# (.accept server#))
                             (catch SocketException e#
                               (.printStackTrace e#)))
                           (recur)))
           .start)
         (if ~*trampoline?*
           (clojure.main/repl ~@options)
           (do (when-not ~*interactive?*
                 (println "REPL started; server listening on"
                          ~host "port" ~port))
               ;; block to avoid shutdown-agents
               @(promise))))))

(defn copy-out-loop [reader]
  (let [buffer (make-array Character/TYPE 1000)]
    (loop []
      (.write *out* buffer 0 (.read reader buffer))
      (flush)
      (Thread/sleep 100)
      (recur))))

(defn repl-client [reader writer & [socket]]
  (.start (Thread. #(copy-out-loop reader)))
  (loop [reader reader, writer writer]
    (let [input (read-line)]
      (when (and input (not= "" input))
        (.write writer (str input "\n"))
        (.flush writer)
        (recur reader writer)))))

(defn- connect-to-server [socket handler]
  (let [reader (InputStreamReader. (.getInputStream socket))
        writer (OutputStreamWriter. (.getOutputStream socket))]
    (handler reader writer socket)))

(defn poll-repl-connection
  ([port retries handler]
     (when (> retries retry-limit)
       (throw (Exception. "Couldn't connect")))
     (Thread/sleep 100)
     (let [val (try (connect-to-server (Socket. "localhost" port) handler)
                    (catch java.net.ConnectException _ ::retry))]
       (if (= ::retry val)
         (recur port (inc retries) handler)
         val)))
  ([port]
     (poll-repl-connection port 0 repl-client)))

(defn repl-socket-on [{:keys [repl-port repl-host]}]
  [(Integer. (or repl-port
                 (System/getenv "LEIN_REPL_PORT")
                 (dec (+ 1024 (rand-int 64512)))))
   (or repl-host
       (System/getenv "LEIN_REPL_HOST")
       "localhost")])

(defn repl
=======
(def retry-limit 200)

(defn repl-options [project options]
  (let [options (apply hash-map options)
        init `#(let [is# ~(:repl-init-script project)
                     in# '~(:repl-init project)
                     mn# '~(:main project)]
                 ~(:init options)
                 (when (and is# (.exists (File. (str is#))))
                   (println (str "Warning: :repl-init-script is "
                                 "deprecated; use :repl-init."))
                   (load-file is#))
                 (when in#
                   (require in#))
                 (when mn#
                   (require mn#))
                 (in-ns (or in# mn# '~'user)))
        ;; Suppress socket closed since it's part of normal operation
        caught `(fn [t#]
                  (when-not (instance? SocketException t#)
                    (~(:caught options 'clojure.main/repl-caught) t#)))
        ;; clojure.main/repl has no way to exit without signalling EOF,
        ;; which we can't do with a socket. We can't rebind skip-whitespace
        ;; in Clojure 1.3, so we have to duplicate the function
        read `(fn [request-prompt# request-exit#]
                (or ({:line-start request-prompt# :stream-end request-exit#}
                     (try (clojure.main/skip-whitespace *in*)
                          (catch java.io.IOException _#
                            :stream-end)))
                    (let [input# (read)]
                      (clojure.main/skip-if-eol *in*)
                      input#)))]
    (apply concat [:init init :caught caught :read read]
           (dissoc options :caught :init :read))))

(defn repl-server [project host port & options]
  `(do (try ;; transitive requires don't work for stuff on bootclasspath
         (require '~'clojure.java.shell)
         (require '~'clojure.java.browse)
         ;; these are new in clojure 1.2, so swallow exceptions for 1.1
         (catch Exception _#))
       (set! *warn-on-reflection* false)
       (let [server# (ServerSocket. ~port 0 (InetAddress/getByName ~host))
             acc# (fn [s#]
                    (let [ins# (.getInputStream s#)
                          outs# (.getOutputStream s#)
                          out-writer# (OutputStreamWriter. outs#)]
                      (doto (Thread.
                             #(binding [*in* (-> ins# InputStreamReader.
                                                 LineNumberingPushbackReader.)
                                        *out* out-writer#
                                        *err* (PrintWriter. out-writer#)
                                        *warn-on-reflection*
                                        ~(:warn-on-reflection project)]
                                (clojure.main/repl
                                 ~@(repl-options project options))))
                        .start)))]
         (doto (Thread. #(when-not (.isClosed server#)
                           (try
                             (acc# (.accept server#))
                             (catch SocketException e#
                               (.printStackTrace e#)))
                           (recur)))
           .start)
         (if ~*trampoline?*
           (clojure.main/repl ~@options)
           (do (when-not ~*interactive?*
                 (println "REPL started; server listening on"
                          ~host "port" ~port))
               ;; block to avoid shutdown-agents
               @(promise))))))

(defn copy-out-loop [reader]
  (let [buffer (make-array Character/TYPE 1000)]
    (loop [length (.read reader buffer)]
      (when-not (neg? length)
        (.write *out* buffer 0 length)
        (flush)
        (Thread/sleep 100)
        (recur (.read reader buffer))))))

(defn repl-client [reader writer & [socket]]
  (.start (Thread. #(do (copy-out-loop reader)
                        (exit 0))))
  (loop []
    (let [input (read-line)]
      (when (and input (not= "" input) (not (.isClosed socket)))
        (.write writer (str input "\n"))
        (.flush writer)
        (recur)))))

(defn- connect-to-server [socket handler]
  (let [reader (InputStreamReader. (.getInputStream socket))
        writer (OutputStreamWriter. (.getOutputStream socket))]
    (handler reader writer socket)))

(defn poll-repl-connection
  ([port retries handler]
     (when (> retries retry-limit)
       (throw (Exception. "Couldn't connect")))
     (Thread/sleep 100)
     (let [val (try (connect-to-server (Socket. "localhost" port) handler)
                    (catch java.net.ConnectException _ ::retry))]
       (if (= ::retry val)
         (recur port (inc retries) handler)
         val)))
  ([port]
     (poll-repl-connection port 0 repl-client)))

(defn repl-socket-on [{:keys [repl-port repl-host]}]
  [(Integer. (or repl-port
                 (System/getenv "LEIN_REPL_PORT")
                 (dec (+ 1024 (rand-int 64512)))))
   (or repl-host
       (System/getenv "LEIN_REPL_HOST")
       "localhost")])

(defn repl
>>>>>>> B.clj
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
   (if-let [repl-port (nrepl.ack/wait-for-ack (:repl-timeout project 30000))]
     (reply/launch-nrepl
       (merge
         {:attach (str repl-port)}
         (:reply-options project)))
     (println "REPL server launch timed out."))))

(ns riemann.logging
  (:import (org.slf4j
             LoggerFactory)
           (ch.qos.logback.classic
             Level
             Logger)
           (ch.qos.logback.core
             ConsoleAppender
             FileAppender)
           (ch.qos.logback.core.encoder
             LayoutWrappingEncoder)
           (ch.qos.logback.core.rolling
             RollingFileAppender
             TimeBasedRollingPolicy
             FixedWindowRollingPolicy
             SizeBasedTriggeringPolicy)
           (ch.qos.logback.classic.encoder
             PatternLayoutEncoder)
           (net.logstash.logback
             JSONEventLayoutV0
             JSONEventLayoutV1)
           (net.logstash.logback.encoder
             LogstashEncoder))
  (:require wall.hack))

(defn get-logger
  ([]
    (LoggerFactory/getLogger Logger/ROOT_LOGGER_NAME))
  ([logger]
    (LoggerFactory/getLogger logger)))

(defn- get-context
  []
  (LoggerFactory/getILoggerFactory))


(defmulti encoder identity)

(defmethod encoder :json
  [type]
  (LogstashEncoder.))

(defmethod encoder :json-event
  [type]
  (encoder :json-event-v0))

(defmethod encoder :json-event-v0
  [type]
  (doto (LayoutWrappingEncoder.)
    (.setLayout (JSONEventLayoutV0.))))

(defmethod encoder :json-event-v1
  [type]
  (doto (LayoutWrappingEncoder.)
    (.setLayout (JSONEventLayoutV1.))))

(defmethod encoder :riemann
  [type]
  (doto (PatternLayoutEncoder.)
    (.setPattern "%p [%d] %t - %c - %m%n%throwable")))

(defmethod encoder :default
  [type]
  (encoder :riemann))


(defn set-level
  "Set the level for the given logger, by string name.

  Example:

  (set-level Level/INFO)
    or
  (set-level \"riemann.client\", Level/DEBUG)"
  ([level]
    (. (get-logger)
      (setLevel level)))
  ([logger level]
    (. (get-logger logger)
      (setLevel level))))

(defmacro suppress
  "Turns off logging for the evaluation of body."
  [loggers & body]
  (let [[logger & more] (flatten [loggers])]
    (if logger
      `(let [old-level# (.getLevel (get-logger ~logger))]
         (try
           (set-level ~logger Level/ERROR)
           (suppress ~more ~@body)
           (finally
             (set-level ~logger old-level#))))
      `(do ~@body))))

<<<<<<< A.clj
||||||| O.clj
(def ^{:doc "available logging patterns"}
  layouts
  {:riemann       (EnhancedPatternLayout. "%p [%d] %t - %c - %m%n%throwable")
   :json-event    (JSONEventLayoutV0.)
   :json-event-v0 (JSONEventLayoutV0.)
   :json-event-v1 (JSONEventLayoutV1.)})

(defn get-layout
  "Fetch a logging layout by name"
  [layout-name]
  (get layouts (or layout-name :riemann)))

=======
(def ^{:doc "available logging patterns"}
  layouts
  {:riemann       (EnhancedPatternLayout. "%p [%d] %t - %c - %m%n%throwable")
   :json-event    (JSONEventLayoutV0.)
   :json-event-v0 (JSONEventLayoutV0.)
   :json-event-v1 (JSONEventLayoutV1.)})

(defn get-layout
  "Fetch a logging layout by name"
  [layout-name]
  (let [layout (get layouts (or layout-name :riemann))]
    (when (nil? layout)
      (binding [*out* *err*] (println "invalid logging layout specified: " layout-name)))
    (or layout (:riemann layouts))))

>>>>>>> B.clj
(defn init
  "Initialize logging. You will probably call this from the config file. You can
  call init more than once; its changes are destructive. Options:

  :console          Determine if logging should happen on the console.
  :console-layout   Specifying console layout.
  :file             The file to log to. If omitted, log to console only.
  :file-layout      Specifying file layout.
  :files            A list of files to log to. If provided, a seq or vector is
                    expected containing maps with a :file and an :file-layout
  :logsize-rotate   If size (in bytes) is specified use size based rotation
                    otherwise use default time based rotation.
  :rotate-count     Specifying the number of rotated files to keep. If omitted,
                    keep last 10 rotated files.

  Layout can be :riemann or :json. If layout omitted, default layout :riemann will be used.

  Example:

  (init {:console true :file \"/var/log/riemann.log\"})
    or
  (init {:console false :file \"/var/log/riemann.log\" :rotate-count 10})
    or
  (init {:console false :file \"/var/log/riemann.log\" :logsize-rotate 1000000000})
    or
  (init {:console false :files [{:file \"/var/log/riemann.log\"}, {:file \"/var/log/riemann.json.log\" :file-layout :json}] :logsize-rotate 100 :rotate-count 5})"
  ([]
   (set-level                    Level/INFO)
   (set-level "riemann.client"   Level/DEBUG)
   (set-level "riemann.server"   Level/DEBUG)
   (set-level "riemann.streams"  Level/DEBUG)
   (set-level "riemann.graphite" Level/DEBUG))
  ([opts]
    (let [{:keys [console
                  console-layout
                  file
                  file-layout
                  files
                  rotate-count
                  logsize-rotate]} opts
         logger   (get-logger)
         context  (get-context)]

      (.detachAndStopAllAppenders logger)

      (when console
        (let [encoder             (doto (encoder console-layout)
                                    (.setContext context)
                                    (.start))
              console-appender    (doto (ConsoleAppender.)
                                    (.setContext context)
                                    (.setEncoder encoder)
                                    (.start))]
          (.addAppender logger console-appender)))

      (doseq [{:keys [file file-layout]} (conj files {:file file :file-layout file-layout})
               :when file]
        (if logsize-rotate
          (let [encoder           (doto (encoder file-layout)
                                    (.setContext context)
                                    (.start))
                log-appender      (doto (RollingFileAppender.)
                                    (.setFile file)
                                    (.setContext context)
                                    (.setEncoder encoder))
                rolling-policy    (doto (FixedWindowRollingPolicy.)
                                    (.setMinIndex 1)
                                    (.setMaxIndex (or rotate-count 10))
                                    (.setFileNamePattern
                                     (str file ".%i"))
                                    (.setParent log-appender)
                                    (.setContext context)
                                    (.start))
                triggering-policy (doto (SizeBasedTriggeringPolicy.)
                                    (.setMaxFileSize (str logsize-rotate))
                                    (.setContext context)
                                    (.start))
                log-appender      (doto log-appender
                                    (.setRollingPolicy rolling-policy)
                                    (.setTriggeringPolicy triggering-policy)
                                    (.start))]
            (.addAppender logger log-appender))
          (let [encoder           (doto (encoder file-layout)
                                    (.setContext context)
                                    (.start))
                log-appender      (doto (RollingFileAppender.)
                                    (.setFile file)
                                    (.setContext context)
                                    (.setEncoder encoder))
                rolling-policy    (doto (TimeBasedRollingPolicy.)
                                    (.setMaxHistory (or rotate-count 10))
                                    (.setFileNamePattern
                                     (str file ".%d{yyyy-MM-dd}"))
                                    (.setParent log-appender)
                                    (.setContext context)
                                    (.start))
                log-appender      (doto log-appender
                                    (.setRollingPolicy rolling-policy)
                                    (.start))]
            (.addAppender logger log-appender)))))

    (set-level                    Level/INFO)
    (set-level "riemann.client"   Level/DEBUG)
    (set-level "riemann.server"   Level/DEBUG)
    (set-level "riemann.streams"  Level/DEBUG)
    (set-level "riemann.graphite" Level/DEBUG)))

(defn nice-syntax-error
  "Rewrites clojure.lang.LispReader$ReaderException to have error messages that
  might actually help someone."
  ([e] (nice-syntax-error e "(no file)"))
  ([e file]
   ; Lord help me.
   (let [line (wall.hack/field (class e) :line e)
         msg (.getMessage (or (.getCause e) e))]
    (RuntimeException. (str "Syntax error (" file ":" line ") " msg)))))

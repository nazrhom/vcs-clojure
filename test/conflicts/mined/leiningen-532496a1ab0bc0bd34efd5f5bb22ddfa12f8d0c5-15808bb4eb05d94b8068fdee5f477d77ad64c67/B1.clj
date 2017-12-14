 ;; eval-in-project
(defdeprecated get-os paths/get-os)

(defdeprecated get-os paths/get-arch)

(defn platform-nullsink []
  (file (if (= :windows (paths/get-os))
          "NUL"
          "/dev/null")))

(defn- as-str [x]
  (if (instance? clojure.lang.Named x)
    (name x)
    (str x)))

(defn- d-property [[k v]]
  (format "-D%s=%s" (as-str k) v))

(defn ^{:internal true} get-jvm-args [project]
  (let [native-arch-path (paths/native-arch-path project)]
    `(~@(let [opts (System/getenv "JVM_OPTS")]
          (when (seq opts) [opts]))
      ~@(:jvm-opts project)
      ~@(:jvm-opts (user-settings))
      ~@(map d-property {:clojure.compile.path (:compile-path project)
                         (str (:name project) ".version") (:version project)
                         :clojure.debug (boolean (or (System/getenv "DEBUG")
                                                     (:debug project)))})
      ~@(when (and native-arch-path (.exists native-arch-path))
          [(d-property [:java.library.path native-arch-path])]))))

(defn- injected-forms []
  (with-open [rdr (-> "robert/hooke.clj" resource reader PushbackReader.)]
    `(do (ns ~'leiningen.util.injected)
         ~@(doall (take 6 (rest (repeatedly #(read rdr)))))
         (ns ~'user))))

(defn get-readable-form [java project form init]
  (let [form `(do ~init
                  ~(:project-init project)
                  ~@(let [user-clj (file (paths/leiningen-home) "user.clj")]
                      (if (.exists user-clj)
                        [(list 'load-file (str user-clj))]))
                  ~(injected-forms)
                  (set! ~'*warn-on-reflection*
                        ~(:warn-on-reflection project))
                  (try ~form
                       ;; non-daemon threads will prevent process from exiting;
                       ;; see http://tinyurl.com/2ueqjkl
                       (finally
                        (when (and ~(:shutdown-agents project false)
                                   (not= "1.5" (System/getProperty
                                                "java.specification.version"))
                                   ~(not *interactive?*))
                          (shutdown-agents)))))]
    ;; work around java's command line handling on windows
    ;; http://bit.ly/9c6biv This isn't perfect, but works for what's
    ;; currently being passed; see
    ;; http://www.perlmonks.org/?node_id=300286 for some of the
    ;; landmines involved in doing it properly
    (if (and (= (paths/get-os) :windows) (not (:eval-in-leiningen project)))
      (pr-str (pr-str form))
      (pr-str form))))

(defn prep [{:keys [compile-path checksum-deps] :as project} skip-auto-compile]
  (when (and (not (or *skip-auto-compile* skip-auto-compile)) compile-path
             (empty? (.list (file compile-path))))
    (binding [*silently* true]
      (compile project)))
  (when (or (empty? (find-deps-files project)) checksum-deps)
    (deps project))
  (when compile-path
    (.mkdirs (file compile-path))))

(defn eval-in-leiningen [project form-string]
  ;; bootclasspath workaround: http://dev.clojure.org/jira/browse/CLJ-673
  (require '[clojure walk repl])
  (require '[clojure.java io shell browse])
  (when (:debug project)
    (System/setProperty "clojure.debug" "true"))
  ;; need to at least pretend to return an exit code
  (try (binding [*warn-on-reflection* (:warn-on-reflection project), *ns* *ns*]
         (eval (read-string form-string)))
       0
       (catch Exception e
         (.printStackTrace e)
         1)))

(defn- pump [reader out]
  (let [buffer (make-array Character/TYPE 1000)]
    (loop [len (.read reader buffer)]
      (when-not (neg? len)
        (.write out buffer 0 len)
        (.flush out)
        (Thread/sleep 100)
        (recur (.read reader buffer))))))

;; clojure.java.shell/sh doesn't let you stream out/err
(defn sh [& cmd]
  (let [proc (.exec (Runtime/getRuntime) (into-array cmd))]
    (with-open [out (reader (.getInputStream proc))
                err (reader (.getErrorStream proc))]
      (let [pump-out (doto (Thread. #(pump out *out*)) .start)
            pump-err (doto (Thread. #(pump err *err*)) .start)]
        (.join pump-out)
        (.join pump-err))
      (.waitFor proc))))

(defn eval-in-subprocess [project form-string]
  (apply sh `(~(or (System/getenv "JAVA_CMD") "java")
              "-cp" ~(get-classpath-string project)
              ~@(get-jvm-args project)
              "clojure.main" "-e" ~form-string)))

(defn eval-in-project
  "Executes form in an isolated classloader with the classpath and compile path
  set correctly for the project. If the form depends on any requires, put them
  in the init arg to avoid the Gilardi Scenario: http://technomancy.us/143"
  [project form & [handler skip-auto-compile init]]
  (when skip-auto-compile
    (println "WARNING: eval-in-project's skip-auto-compile arg is deprecated."))
  (prep project skip-auto-compile)
  (let [form-string (get-readable-form nil project form init)]
    (if (:eval-in-leiningen project)
      (eval-in-leiningen project form-string)
      (eval-in-subprocess project form-string))))


(ns leiningen.core.project
  "Read project.clj files."
  (:refer-clojure :exclude [read])
  (:require [clojure.walk :as walk]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [cemerick.pomegranate :as pomegranate]
            [cemerick.pomegranate.aether :as aether]
            [leiningen.core.utils :as utils]
            [leiningen.core.ssl :as ssl]
            [leiningen.core.user :as user]
            [leiningen.core.logger :as log]
            [leiningen.core.classpath :as classpath]
            [useful.fn :refer [fix]]
            [useful.seq :refer [update-first find-first]]
            [useful.map :refer [update update-each map-vals]])
  (:import (clojure.lang DynamicClassLoader)
           (java.io PushbackReader)))

;; # Project definition and normalization

(defn artifact-map
  [id]
  {:artifact-id (name id)
   :group-id (or (namespace id) (name id))})

(defn exclusion-map
  "Transform an exclusion vector into a map that is easier to combine with
  meta-merge. This allows a profile to override specific exclusion options."
  [spec]
  (if-let [[id & {:as opts}] (fix spec symbol? vector)]
    (-> opts
        (merge (artifact-map id))
        (with-meta (meta spec)))))

(defn exclusion-vec
  "Transform an exclusion map back into a vector of the form:
  [name/group & opts]"
  [exclusion]
  (if-let [{:keys [artifact-id group-id]} exclusion]
    (into [(symbol group-id artifact-id)]
          (apply concat (dissoc exclusion :artifact-id :group-id)))))

(defn dependency-map
  "Transform a dependency vector into a map that is easier to combine with
  meta-merge. This allows a profile to override specific dependency options."
  [dep]
  (if-let [[id version & {:as opts}] dep]
    (-> opts
        (merge (artifact-map id))
        (assoc :version version)
        (update :exclusions #(if % (map exclusion-map %)))
        (with-meta (meta dep)))))

(defn dependency-vec
  "Transform a dependency map back into a vector of the form:
  [name/group \"version\" & opts]"
  [dep]
  (if-let [{:keys [artifact-id group-id version]} dep]
    (-> dep
        (update :exclusions #(if % (map exclusion-vec %)))
        (dissoc :artifact-id :group-id :version)
        (->> (apply concat)
             (into [(symbol group-id artifact-id) version]))
        (with-meta (meta dep)))))

(defn- displace?
  "Returns true if the object is marked as displaceable"
  [obj]
  (-> obj meta :displace))

(defn- replace?
  "Returns true if the object is marked as replaceable"
  [obj]
  (-> obj meta :replace))

(defn- different-priority?
  "Returns true if either left has a higher priority than right or vice versa."
  [left right]
  (boolean
   (some (some-fn nil? displace? replace?) [left right])))

(defn- pick-prioritized
  "Picks the highest prioritized element of left and right and merge their
  metadata."
  [left right]
  (cond (nil? left) right
        (nil? right) left

        (and (displace? left)   ;; Pick the rightmost
             (displace? right)) ;; if both are marked as displaceable
        (with-meta right
          (merge (meta left) (meta right)))

        (and (replace? left)    ;; Pick the rightmost
             (replace? right))  ;; if both are marked as replaceable
        (with-meta right
          (merge (meta left) (meta right)))

        (or (displace? left)
            (replace? right))
        (with-meta right
          (merge (-> left meta (dissoc :displace))
                 (-> right meta (dissoc :replace))))

        (or (replace? left)
            (displace? right))
        (with-meta left
          (merge (-> right meta (dissoc :displace))
                 (-> left meta (dissoc :replace))))))

(declare meta-merge)

;; TODO: drop this and use read-eval syntax in 3.0
(defn- unquote-project
  "Inside defproject forms, unquoting (~) allows for arbitrary evaluation."
  [args]
  (walk/walk (fn [item]
               (cond (and (seq? item) (= `unquote (first item))) (second item)
                     ;; needed if we want fn literals preserved
                     (or (seq? item) (symbol? item)) (list 'quote item)
                     :else (let [result (unquote-project item)]
                             ;; clojure.walk strips metadata
                             (if-let [m (meta item)]
                               (with-meta result m)
                               result))))
             identity
             args))

(def defaults
  ;; TODO: why isn't :repositories in here?
  {:source-paths ["src"]
   :resource-paths ["resources"]
   :test-paths ["test"]
   :native-path "target/native"
   :compile-path "target/classes"
   :target-path "target"
   :prep-tasks ["javac" "compile"]
   :jar-exclusions [#"^\."]
   :jvm-opts ["-XX:+TieredCompilation"]
   :certificates ["clojars.pem"]
   :uberjar-exclusions [#"(?i)^META-INF/[^/]*\.(SF|RSA|DSA)$"]
   :test-selectors {:default '(constantly true)}})

(defn- dep-key
  "The unique key used to dedupe dependencies."
  [[id version & opts]]
  (-> (apply hash-map opts)
      (select-keys [:classifier :extension])
      (assoc :id id)))

(defn- add-dep [deps dep]
  (let [k (dep-key dep)]
    (update-first deps #(= k (dep-key %))
                  (fn [existing]
                    (dependency-vec
                     (meta-merge (dependency-map existing)
                                 (dependency-map dep)))))))

(defn- normalize-repo
  "Normalizes a repository to the canonical repository form."
  [[id opts :as repo]]
  (with-meta
    [id (fix opts string? (partial hash-map :url))]
    (meta repo)))

(defn- normalize-repos
  "Normalizes a vector of repositories to the canonical repository form."
  [repos]
  (with-meta
    (mapv normalize-repo repos)
    (meta repos)))

(defn- add-repo [repos [id opts :as repo]]
  (update-first repos #(= id (first %))
                (fn [[_ existing :as original]]
                  (let [opts (if (keyword? opts)
                               (-> (find-first #(= (first %)
                                                   (name opts))
                                               repos)
                                   second)
                               opts)
                        repo (with-meta [id opts] (meta repo))]
                    (if (different-priority? repo original)
                      (pick-prioritized repo original)
                      (with-meta [id (meta-merge existing opts)]
                        (merge (meta original) (meta repo))))))))

(def empty-dependencies
  (with-meta [] {:reduce add-dep}))

(def empty-repositories
  (with-meta [] {:reduce add-repo}))

(def empty-paths
  (with-meta [] {:prepend true}))

(def default-repositories
  (with-meta
    [["central" {:url "http://repo1.maven.org/maven2/" :snapshots false}]
     ["clojars" {:url "https://clojars.org/repo/"}]]
    {:reduce add-repo}))

(def deploy-repositories
  (with-meta
    [["clojars" {:url "https://clojars.org/repo/", :password :gpg, :username :gpg}]]
    {:reduce add-repo}))

(defn update-if-in-map
  "Like update-each, but will only update if the key is within the map."
  [m ks f & args]
  (apply update-each m (filter (partial contains? m) ks) f args))

(defn normalize-values
  "Transform values within a project or profile map to normalized values, such that
  internal functions can assume that the values are already normalized."
  [map]
  (-> map
      (update-if-in-map [:repositories :deploy-repositories
                         :plugin-repositories] normalize-repos)
      (update-if-in-map [:profiles] map-vals normalize-values)))

(defn make
  ([project project-name version root]
     (make (assoc project
             :name (name project-name)
             :group (or (namespace project-name)
                        (name project-name))
             :version version
             :root root)))
  ([project]
     (let [repos (if (:omit-default-repositories project)
                   (do (log/warn
                          ":omit-default-repositories is deprecated.\n"
                          "use :repositories ^:replace [...] instead.")
                       empty-repositories)
                   default-repositories)]
       (meta-merge
        {:repositories repos
         :plugin-repositories repos
         :deploy-repositories deploy-repositories
         :plugins empty-dependencies
         :dependencies empty-dependencies
         :source-paths empty-paths
         :resource-paths empty-paths
         :test-paths empty-paths}
        (-> (merge defaults project)
            (assoc :jvm-opts (or (:jvm-opts project) (:java-opts project)
                                 (:jvm-opts defaults)))
            (dissoc :eval-in-leiningen :omit-default-repositories :java-opts)
            (assoc :eval-in (or (:eval-in project)
                                (if (:eval-in-leiningen project)
                                  :leiningen, :subprocess))
                   :offline? (not (nil? (System/getenv "LEIN_OFFLINE"))))
            (normalize-values))))))

(defmacro defproject
  "The project.clj file must either def a project map or call this macro.
  See `lein help sample` to see what arguments it accepts."
  [project-name version & {:as args}]
  `(let [args# ~(unquote-project args)
         root# ~(.getParent (io/file *file*))]
     (def ~'project
       (make args# '~project-name ~version root#))))

(defn- add-exclusions [exclusions dep]
  (dependency-vec
   (update (dependency-map dep) :exclusions
           into (map exclusion-map exclusions))))

(defn- add-global-exclusions [project]
  (let [{:keys [dependencies exclusions]} project]
    (if-let [exclusions (and (seq dependencies) (seq exclusions))]
      (assoc project
        :dependencies (with-meta
                        (mapv (partial add-exclusions exclusions)
                              dependencies)
                        (meta dependencies)))
      project)))

(defn- absolutize [root path]
  (str (if (.isAbsolute (io/file path))
         path
         (io/file root path))))

(defn- absolutize-path [{:keys [root] :as project} key]
  (cond (re-find #"-path$" (name key))
        (update-in project [key] (partial absolutize root))

        (re-find #"-paths$" (name key))
        (update-in project [key] (partial map (partial absolutize root)))

        :else project))

(defn absolutize-paths [project]
  (reduce absolutize-path project (keys project)))

;; # Profiles: basic merge logic

(def ^:private hooke-injection
  (with-open [rdr (-> "robert/hooke.clj" io/resource io/reader PushbackReader.)]
    `(do (ns ~'leiningen.core.injected)
         ~@(doall (take-while #(not= % ::eof)
                              (rest (repeatedly #(clojure.core/read
                                                  rdr false ::eof)))))
         (ns ~'user))))

(def default-profiles
  "Profiles get merged into the project map. The :dev, :provided, and :user
  profiles are active by default."
  (atom {:default [:base :user :provided :dev]
         :base {:resource-paths ["dev-resources"]
                :checkout-deps-shares [:source-paths
                                       :resource-paths
                                       :compile-path]}
         :leiningen/test {:injections [hooke-injection]}
         :update {:update :always}
         :offline {:offline? true}
         :debug {:debug true}}))



(defn- meta-merge
  "Recursively merge values based on the information in their metadata."
  [left right]
  (cond (different-priority? left right)
        (pick-prioritized left right)

        (-> left meta :reduce)
        (-> left meta :reduce
            (reduce left right)
            (with-meta (meta left)))

        (and (map? left) (map? right))
        (merge-with meta-merge left right)

        (and (set? left) (set? right))
        (set/union right left)

        (and (coll? left) (coll? right))
        (if (or (-> left meta :prepend)
                (-> right meta :prepend))
          (-> (concat right left)
              (with-meta (merge (meta left)
                                (select-keys (meta right) [:displace]))))
          (concat left right))

        (= (class left) (class right)) right

        :else
        (do (println left "and" right "have a type mismatch merging profiles.")
            right)))

(defn- apply-profiles [project profiles]
  (reduce (fn [project profile]
            (with-meta
              (meta-merge project profile)
              (meta-merge (meta project) (meta profile))))
          project
          profiles))

(defn- lookup-profile
  "Lookup a profile in the given profiles map, warning when the profile doesn't
  exist. Recurse whenever a keyword or vector is found, combining all profiles
  in the vector."
  [profiles profile]
  (cond (keyword? profile)
        (let [result (get profiles profile)]
          (when-not (or result (#{:provided :dev :user :test :production} profile))
            (log/warn "profile" profile "not found."))
          (vary-meta (lookup-profile profiles result)
                     update-in [:active-profiles] (fnil conj []) profile))

        ;; composite profile
        (vector? profile)
        (apply-profiles {} (map (partial lookup-profile profiles) profile))

        :else (or profile {})))

(defn- warn-user-repos []
  (when (->> (vals (user/profiles))
             (map (comp second :repositories))
             (apply concat) (some :url))
    (log/warn 
      ":repositories detected in user-level profile!\n"
      "See https://github.com/technomancy/leiningen/wiki/Repeatability")))

(alter-var-root #'warn-user-repos memoize)

(defn- warn-user-profile [profiles]
  (when (contains? profiles :user)
    (log/warn "user-level profile defined in project files.")))

(alter-var-root #'warn-user-profile memoize)

(defn- project-profiles [project]
  (let [profiles (utils/read-file (io/file (:root project) "profiles.clj"))]
    (warn-user-profile profiles)
    profiles))

(defn- read-profiles
  "Read profiles from a variety of sources.

  We check Leiningen's defaults, the profiles.clj file in ~/.lein/profiles.clj,
  the profiles.clj file in the project root, and the :profiles key from the
  project map."
  [project]
  (warn-user-repos)
  (warn-user-profile (:profiles project))
  (merge @default-profiles (user/profiles)
         (:profiles project) (project-profiles project)))

;; # Lower-level profile plumbing: loading plugins, hooks, middleware, certs

(defn ensure-dynamic-classloader []
  (let [thread (Thread/currentThread)
        cl (.getContextClassLoader thread)]
    (when-not (pomegranate/modifiable-classloader? cl)
      (.setContextClassLoader thread (DynamicClassLoader. cl)))))

(defn load-plugins
  ([project key]
     (when (seq (get project key))
       (ensure-dynamic-classloader)
       (classpath/resolve-dependencies
        key project
        :add-classpath? true))
     (doseq [wagon-file (-> (.getContextClassLoader (Thread/currentThread))
                            (.getResources "leiningen/wagons.clj")
                            (enumeration-seq))
             [hint factory] (read-string (slurp wagon-file))]
       (aether/register-wagon-factory! hint (eval factory)))
     project)
  ([project] (load-plugins project :plugins)))

(defn plugin-vars [project type]
  (for [[plugin _ & {:as opts}] (:plugins project)
        :when (get opts type true)]
    (-> (symbol (str (name plugin) ".plugin") (name type))
        (with-meta {:optional true}))))

(defn- plugin-hooks [project]
  (plugin-vars project :hooks))

(defn- plugin-middleware [project]
  (plugin-vars project :middleware))

(defn- load-hook [hook-name]
  (if-let [hook (try (utils/require-resolve hook-name)
                     (catch Throwable e
                       (utils/error "problem requiring" hook-name "hook")
                       (throw e)))]
    (try (hook)
         (catch Throwable e
           (utils/error "problem activating" hook-name "hook")
           (throw e)))
    (when-not (:optional (meta hook-name))
      (utils/error "cannot resolve" hook-name "hook"))))

(defn load-hooks [project & [ignore-missing?]]
  (doseq [hook-name (concat (plugin-hooks project) (:hooks project))]
    ;; if hook-name is just a namespace assume hook fn is called activate
    (let [hook-name (if (namespace hook-name)
                       hook-name
                       (symbol (name hook-name) "activate"))]
      (load-hook hook-name)))
  project)

(defn apply-middleware
  ([project]
     (reduce apply-middleware project
             (concat (plugin-middleware project)
                     (:middleware project))))
  ([project middleware-name]
     (if-let [middleware (utils/require-resolve middleware-name)]
       (middleware project)
       (do (when-not (:optional (meta middleware-name))
             (utils/error "cannot resolve" middleware-name "middleware"))
           project))))

(defn load-certificates
  "Load the SSL certificates specified by the project and register
   them for use by Aether."
  [project]
  (let [certs (mapcat ssl/read-certs (:certificates project))
        context (ssl/make-sslcontext (into (ssl/default-trusted-certs) certs))]
    (ssl/register-scheme (ssl/https-scheme context))
    project))

(defn activate-middleware
  "A helper funtction to apply middleware and then load certificates and hooks,
  since we always do these three things together, at least so far."
  [project]
  (doto (apply-middleware project)
    (load-certificates)
    (load-hooks)))

(defn ^:internal init-profiles
  "Compute a fresh version of the project map, including and excluding the
  specified profiles."
  [project include-profiles & [exclude-profiles]]
  (let [project (:without-profiles (meta project) project)
        profile-map (apply dissoc (read-profiles project) exclude-profiles)
        profiles (map (partial lookup-profile profile-map) include-profiles)
        normalized-profiles (map normalize-values profiles)]
    (-> project
        (apply-profiles normalized-profiles)
        (absolutize-paths)
        (add-global-exclusions)
        (vary-meta merge {:without-profiles project
                          :included-profiles include-profiles
                          :excluded-profiles exclude-profiles}))))

;; # High-level profile operations

(defn set-profiles
  "Compute a fresh version of the project map, with middleware applied, including
   and excluding the specified profiles."
  [project include-profiles & [exclude-profiles]]
  (-> project
      (init-profiles include-profiles exclude-profiles)
      (load-plugins)
      (activate-middleware)))

(defn merge-profiles
  "Compute a fresh version of the project map with the given profiles merged
   into list of active profiles and the appropriate middleware applied."
  [project profiles]
  (let [{:keys [included-profiles excluded-profiles]} (meta project)]
    (set-profiles project
      (concat included-profiles profiles)
      (remove (set profiles) excluded-profiles))))

(defn unmerge-profiles
  "Compute a fresh version of the project map with the given profiles unmerged
   from list of active profiles and the appropriate middleware applied."
  [project profiles]
  (let [{:keys [included-profiles excluded-profiles]} (meta project)]
    (set-profiles project
      (remove (set profiles) included-profiles)
      (concat excluded-profiles profiles))))

(defn- init-lein-classpath
  "Adds dependencies to Leiningen's classpath if required."
  [project]
  (when (= :leiningen (:eval-in project))
    (doseq [path (classpath/get-classpath project)]
      (pomegranate/add-classpath path))))

(defn init-project
  "Initializes a project. This is called at startup with the default profiles."
  [project]
  (-> project
      (doto
        (load-certificates)
        (init-lein-classpath)
        (load-plugins))
      (activate-middleware)))

(defn add-profiles
  "Add the profiles in the given profiles map to the project map, taking care
   to preserve project map metadata. Note that these profiles are not merged,
   merely made available to merge by name."
  [project profiles-map]
  ;; Merge new profiles into both the project and without-profiles meta
  (vary-meta (update-in project [:profiles] merge profiles-map)
             merge
             {:without-profiles (update-in (:without-profiles (meta project)
                                                              project)
                                           [:profiles] merge
                                           profiles-map)}))

(defn read
  "Read project map out of file, which defaults to project.clj."
  ([file profiles]
     (locking read
       (binding [*ns* (find-ns 'leiningen.core.project)]
         (try (load-file file)
              (catch Exception e
                (throw (Exception. "Error loading project.clj" e)))))
       (let [project (resolve 'leiningen.core.project/project)]
         (when-not project
           (throw (Exception. "project.clj must define project map.")))
         ;; return it to original state
         (ns-unmap 'leiningen.core.project 'project)
         (init-profiles @project profiles))))
  ([file] (read file [:default]))
  ([] (read "project.clj")))

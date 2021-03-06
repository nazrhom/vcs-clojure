(ns leiningen.change
  "Rewrite project.clj by applying a function."
  (:require [clojure.string :as str]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [net.cgrand.sjacket :as sj]
            [net.cgrand.sjacket.parser :as parser]))

;;; Helpers

(defn- fail-argument! [msg]
  (throw (IllegalArgumentException. msg)))

(defn- clj->sjacket [value]
  (if (string? value)
    (str "\"" value "\"")
    (-> value print-str parser/parser :content first)))

;; NOTE: this destroy comments, formatting, etc.
(defn- sjacket->clj [value]
  (if-not (#{:comment :whitespace :newline} (:tag value))
    (-> value sj/str-pt read-string)))

(defn ^:internal normalize-path [value]
  (if (coll? value)
    value
    (map keyword (remove empty? (str/split value #":")))))

(defn ^:internal collapse-fn [f args]
  (let [f (cond (ifn? f) f
                (= "set" f) (constantly (first args))
                (string? f) (resolve (symbol f)))]
    #(apply f % args)))

;;; Maven convention helpers

(defn- split-name [name]
  (str/split name #"/" 2))

(defn- get-group-id [name]
  (let [[group artifact] (split-name name)]
    group))

(defn- get-artifact-id [name]
  (let [[group artifact] (split-name name)]
    (or artifact group)))

(defn- set-group-id [new-id name]
  (let [[group artifact] (split-name name)]
    (str new-id "/" (or artifact group))))

(defn- set-artifact-id [new-id name]
  (let [[group artifact] (split-name name)]
    (if artifact
      (str group "/" new-id)
      new-id)))

;;; Traversal

(defn- defproject? [loc]
  (let [{:keys [tag content]} (zip/node loc)]
    (and (= :name tag)
         (= ["defproject"] content))))

(defn- find-defproject [loc]
  (->> loc
       (iterate zip/next)
       (take-while (comp not zip/end?))
       (filter defproject?)
       first))

(defn- find-right [loc pred]
  (->> loc
      (iterate zip/right)
      (take-while (comp not nil?))
      (filter (comp pred zip/node))
      first))

(defn- find-key [loc key]
  (->> loc
       (iterate zip/right)
       (take-while (comp not nil?))
       (partition 2)
       (map first)
       (filter (comp #{key} sjacket->clj zip/node))
       first))

(defn- next-value [loc]
  (->> loc
       (iterate zip/right)
       (take-while (comp not nil?))
       (drop 1)
       (remove (comp #{:whitespace :comment} :tag zip/node))
       first))

(defn- parse-project [project-str]
  (-> (parser/parser project-str)
      zip/xml-zip
      find-defproject
      (or (fail-argument! "Project definition not found"))
      zip/up))

;;; Modifiers

(defn- insert-entry [loc val]
  (-> (if-not (= "{" (zip/node (zip/left loc)))
        (zip/insert-left loc " ")
        loc)
      (zip/insert-left (clj->sjacket val))))

(defn- update-version [proj fn]
  (-> proj
      (find-right (comp #{:string} :tag))
      (or (fail-argument! "Project version not found"))
      (zip/edit (comp clj->sjacket fn sjacket->clj))
      zip/root))

(defn- update-name [proj fn]
  (-> proj
      zip/right
      (find-right (comp #{:symbol} :tag))
      (or (fail-argument! "Project name not found"))
      (zip/edit (comp clj->sjacket symbol fn str sjacket->clj ))
      zip/root))

<<<<<<< A.clj
(defn- update-setting [proj [p & ath] fn]
  (let [loc (or (-> proj (find-key p) next-value)
                (-> proj
                    zip/rightmost
                    (insert-key-val p {})
                    zip/left))]
    (if (empty? ath)
      (zip/root (zip/edit loc fn))
      (recur (-> loc zip/down zip/right) ath fn))))
||||||| O.clj
(defn- update-setting [loc [p & ath] fn]
  (let [loc'  (-> loc (find-key p) next-value)
        loc'' (or loc' (-> loc
                           zip/rightmost
                           (insert-key-val p {})
                           zip/left))]
    (if (empty? ath)
      (zip/root (zip/edit loc'' fn ))
      (recur (-> loc'' zip/down zip/right) ath fn))))
=======
(defn- update-setting [loc [p & ath] fn]
  (let [loc'  (-> loc (find-key p) next-value)
        loc'' (or loc' (-> loc
                           zip/rightmost
                           (insert-entry p)
                           (insert-entry {})
                           zip/left))]
    (if-not (empty? ath)
      (recur (-> loc'' zip/down zip/right) ath fn)
      (zip/root
       (zip/edit loc''
                 (comp clj->sjacket fn sjacket->clj))))))
>>>>>>> B.clj

;;; Public API

(defn change-string
  [project-str key-or-path f & args]
  (let [f    (collapse-fn f args)
        path (normalize-path key-or-path)
        proj (parse-project project-str)]
    (sj/str-pt
     ;; TODO: support 'magic' keys within nested scope also
     (condp = path
       [:version]
       (update-version proj f)

       [:name]
       (update-name proj f)

       [:group-id]
       (update-name proj #(set-group-id (f (get-group-id %)) %))

       [:artifact-id]
       (update-name proj #(set-artifact-id (f (get-artifact-id %)) %))

       (update-setting proj path f)))))

(defn change
  "Rewrite project.clj with f applied to the value at key-or-path.
<<<<<<< A.clj

The first argument should be a keyword (or mashed-together keywords for
nested values indicating which value to change). The second argument
should name a function var which will be called with the current value
as its first argument and the remaining task aruments as the rest.

This will append \"-SNAPSHOT\" to the current version:

    $ lein change version str \"-SNAPSHOT\"

When called programmatically, you may pass a coll of keywords for the
first arg or an actual function for the second.

Note that this task reads the project.clj file from disk rather than
honoring the project map, so profile merging or `update-in` invocations
will not effect it."
||||||| O.clj

TODO: document accepted args."
=======
  TODO: document accepted args."
>>>>>>> B.clj
  [project key-or-path f & args]
  ;; cannot work with project map, want to preserve formatting, comments, etc
  (let [source (slurp (io/file (:root project) "project.clj"))]
    (spit "project.clj" (apply change-string source key-or-path f args))))

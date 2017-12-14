(ns leiningen.trampoline
  (:refer-clojure :exclude [trampoline])
  (:use [leiningen.core :only [apply-task task-not-found abort]]
        [leiningen.compile :only [get-readable-form prep eval-in-project]]
        [leiningen.classpath :only [get-classpath-string]])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [leiningen.util.paths :as paths]))

(def *trampoline?* false)

(defn get-jvm-opts [project]
  (let [legacy-native (paths/legacy-native-path project)]
    (filter identity [(if (not (empty? (System/getenv "JVM_OPTS")))
                        (System/getenv "JVM_OPTS"))
                      (if (:debug project)
                        "-Dclojure.debug=true")
                      (if (.exists (io/file (:native-path project)))
                        (str "-Djava.library.path=" (:native-path project))
                        (if (.exists (io/file legacy-native))
                          (str "-Djava.library.path=" legacy-native)))])))


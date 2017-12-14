(ns leiningen.trampoline
  (:refer-clojure :exclude [trampoline])
  (:use [leiningen.core :only [apply-task task-not-found abort]]
        [leiningen.compile :only [get-input-args get-readable-form
                                  prep eval-in-project]]
        [leiningen.classpath :only [get-classpath-string]])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [leiningen.util.paths :as paths]))

(def ^{:dynamic true} *trampoline?* false)


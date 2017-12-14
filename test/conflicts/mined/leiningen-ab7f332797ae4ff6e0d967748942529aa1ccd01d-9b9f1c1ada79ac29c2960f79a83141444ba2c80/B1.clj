(ns leiningen.test.run
  (:require [leiningen.core.project :as project]
                    [leiningen.test.helper :as helper
                     :refer [bad-require-project tmp-dir tricky-name-project]])
  (:use [clojure.test]
        [clojure.java.io :only [delete-file]]
        ;; [leiningen.javac :only [javac]]
        [leiningen.run]
        [leiningen.test.helper :only [bad-require-project
                                      tmp-dir
                                      tricky-name-project]]))


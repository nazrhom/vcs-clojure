(ns leiningen.test.run
  (:require [leiningen.core.project :as project])
  (:use [clojure.test]
        [clojure.java.io :only [delete-file]]
        ;; [leiningen.javac :only [javac]]
        [leiningen.run]))


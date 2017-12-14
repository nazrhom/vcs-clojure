(ns leiningen.test.run
  (:require [leiningen.core.project :as project]
            [leiningen.javac]
            [clojure.java.io :as io]
            [leiningen.test.helper :as helper
             :refer [bad-require-project tmp-dir tricky-name-project
                     java-main-project]])
  (:use [clojure.test]
        [leiningen.run]))


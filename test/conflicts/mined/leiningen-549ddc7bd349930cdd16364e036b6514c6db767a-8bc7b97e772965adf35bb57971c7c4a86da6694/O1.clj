(ns leiningen.uberjar
  "Create a jar containing the compiled code, source, and all dependencies."
  (:require [clojure.xml :as xml])
  (:use [clojure.zip :only [xml-zip]]
        [clojure.java.io :only [file copy]]
        [clojure.contrib.zip-filter.xml :only [xml-> tag=]]
        [leiningen.clean :only [clean]]
        [leiningen.jar :only [get-default-jar-name get-jar-filename jar]])
  (:import [java.util.zip ZipFile ZipOutputStream ZipEntry]
           [java.io File FileOutputStream PrintWriter]))


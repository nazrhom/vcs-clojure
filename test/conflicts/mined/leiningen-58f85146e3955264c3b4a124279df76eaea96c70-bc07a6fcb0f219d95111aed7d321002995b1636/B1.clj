(ns leiningen.uberjar
  "Create a jar containing the compiled code, source, and all dependencies."
  (:require [clojure.xml :as xml])
  (:use [clojure.zip :only [xml-zip]]
        [clojure.java.io :only [file copy]]
        [clojure.contrib.zip-filter.xml :only [xml-> tag=]]
        [leiningen.jar :only [get-default-jar-name get-jar-filename jar]])
  (:import [java.util.zip ZipFile ZipOutputStream ZipEntry]
           [java.io File FileOutputStream PrintWriter]))

(defn uberjar
  "Create a jar like the jar task, but including the contents of each of
the dependency jars. Suitable for standalone distribution."
  [project]
  (jar project)
  (let [jarname-base  (str (:name project) \- (:version project))
        standalone-base (str jarname-base "-standalone.jar")
        standalone-filename (get-jar-filename project standalone-base)]
    (with-open [out (-> (file standalone-filename)
                        (FileOutputStream.) (ZipOutputStream.))]
      (let [deps (->> (.listFiles (file (:library-path project)))
                      (filter #(.endsWith (.getName %) ".jar"))
                      (cons (file (get-jar-filename
                                   project (str jarname-base ".jar")))))
            [_ components] (reduce (partial include-dep out)
                                   [#{"META-INF/plexus/components.xml"} nil]
                                   deps)]
        (when-not (empty? components)
          (.putNextEntry out (ZipEntry. "META-INF/plexus/components.xml"))
          (binding [*out* (PrintWriter. out)]
            (xml/emit {:tag :component-set
                       :content
                       [{:tag :components
                         :content
                         components}]})
            (.flush *out*))
          (.closeEntry out))))
    (println "Created" standalone-filename)))

(ns leiningen.uberjar
  "Create a jar containing the compiled code, source, and all dependencies."
  (:require [clojure.xml :as xml])
  (:use [clojure.zip :only [xml-zip]]
        [clojure.java.io :only [file copy]]
        [clojure.contrib.zip-filter.xml :only [xml-> tag=]]
        [leiningen.jar :only [get-jar-filename jar]])
  (:import [java.util.zip ZipFile ZipOutputStream ZipEntry]
           [java.io File FileOutputStream PrintWriter]))

(defn uberjar
  "Create a jar like the jar task, but including the contents of each of
the dependency jars. Suitable for standalone distribution."
  ([project uberjar-name]
     (jar project)
     (let [standalone-filename (get-jar-filename project uberjar-name)]
       (with-open [out (-> (file standalone-filename)
                           (FileOutputStream.) (ZipOutputStream.))]
         (let [deps (->> (.listFiles (file (:library-path project)))
                         (filter #(.endsWith (.getName %) ".jar"))
                         (cons (file (get-jar-filename
                                      project (get-default-jar-name project)))))
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
  ([project] (uberjar project (get-default-uberjar-name project))))

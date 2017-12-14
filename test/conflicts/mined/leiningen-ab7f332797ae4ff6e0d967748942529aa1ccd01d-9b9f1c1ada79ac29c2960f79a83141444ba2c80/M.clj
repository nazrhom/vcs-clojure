(ns leiningen.test.run
<<<<<<< A.clj
  (:require [leiningen.core.project :as project]
            [leiningen.javac]
            [clojure.java.io :as io]
            [leiningen.test.helper :as helper
             :refer [bad-require-project tmp-dir tricky-name-project
                     java-main-project]])
||||||| O.clj
  (:require [leiningen.core.project :as project])
=======
  (:require [leiningen.core.project :as project]
                    [leiningen.test.helper :as helper
                     :refer [bad-require-project tmp-dir tricky-name-project]])
>>>>>>> B.clj
  (:use [clojure.test]
<<<<<<< A.clj
        [leiningen.run]))
||||||| O.clj
        [clojure.java.io :only [delete-file]]
        ;; [leiningen.javac :only [javac]]
        [leiningen.run]
        [leiningen.test.helper :only [bad-require-project
                                      tmp-dir
                                      tricky-name-project]]))
=======
        [clojure.java.io :only [delete-file]]
        ;; [leiningen.javac :only [javac]]
        [leiningen.run]))
>>>>>>> B.clj

(def out-file (format "%s/lein-test" tmp-dir))

(use-fixtures :each (fn [f]
                      (f)
                      (io/delete-file out-file :silently)))

(deftest test-basic
  (run tricky-name-project "/unreadable")
  (is (= "nom:/unreadable" (slurp out-file))))

(deftest test-alt-main
  (run tricky-name-project "-m" "org.domain.tricky-name.munch" "/unreadable")
  (is (= ":munched (\"/unreadable\")" (slurp out-file))))

(deftest test-valid-namespace-argument
  (is (re-find #"Option -m requires a valid namespace argument, not -1\."
               (helper/abort-msg run tricky-name-project "-m" "-1"))))

(deftest test-nonexistant-ns-error-message
  (is (re-find #"Can't find 'nonexistant.ns' as \.class or \.clj for lein run"
               (with-out-str
                 (binding [*err* *out*]
                   (try (run tricky-name-project "-m" "nonexistant.ns")
                        (catch Exception _)))))))

(deftest test-escape-args
  (run tricky-name-project "--" ":bbb")
  (is (= "nom::bbb" (slurp out-file)))
  (run tricky-name-project "--" "-m")
  (is (= "nom:-m" (slurp out-file))))

(deftest test-bad-require-error-msg
  (let [sw (java.io.StringWriter.)]
    (binding [*err* sw]
      (try (run bad-require-project)
           (catch clojure.lang.ExceptionInfo e nil)))
    (let [e-msg (str sw)]
      ;; Don't throw the old ClassNotFoundException
      (is (not (re-find #"ClassNotFoundException: bad-require.core" e-msg)))
      ;; Do show a relevant error message
      (is (re-find #"FileNotFoundException" e-msg))
      (is (re-find #"this/namespace/does/not/exist.clj" e-msg)))))

(deftest test-run-java-main
  (leiningen.javac/javac java-main-project)
  (let [out-result (with-out-str (run java-main-project))]
    (is (= (.trim out-result) ;; To avoid os-specific newline handling
            "Hello from Java!"))))

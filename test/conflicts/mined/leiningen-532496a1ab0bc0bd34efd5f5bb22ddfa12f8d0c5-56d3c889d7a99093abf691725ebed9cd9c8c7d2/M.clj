(ns leiningen.test.help
  (:use [leiningen.help]
        [clojure.test]))

(def formatted-docstring @#'leiningen.help/formatted-docstring)
(def get-subtasks-and-docstrings-for
  @#'leiningen.help/get-subtasks-and-docstrings-for)
(def formatted-help @#'leiningen.help/formatted-help)
(def resolve-task @#'leiningen.help/resolve-task)

(deftest blank-subtask-help-for-new
  (let [subtask-help (apply subtask-help-for (resolve-task "new"))]
    (is (= nil subtask-help))))

(deftest test-docstring-formatting
  (is (= "This is an
              AWESOME command
            For real!"
      (formatted-docstring
        "install"
        "This is an\n  AWESOME command\nFor real!" 5))))

(deftest test-formatted-help
  (is (= "install           This is an
                  AWESOME command
                  For real!"
      (formatted-help "install" "This is an\nAWESOME command\nFor real!" 15))))

<<<<<<< A.clj
;; (deftest test-get-subtasks
;;   (let [m (get-subtasks-and-docstrings-for (second (resolve-task "plugin")))]
;;     (is (= ["install" "uninstall"]
;;            (sort (keys m))))))
;; (doseq [[_ var] (ns-publics *ns*)] (alter-meta! var assoc :busted true))
||||||| O.clj
(deftest test-get-subtasks
  (let [m (get-subtasks-and-docstrings-for (second (resolve-task "plugin")))]
    (is (= ["install" "uninstall"]
           (sort (keys m))))))

=======
(deftest test-get-subtasks
  (let [m (get-subtasks-and-docstrings-for (second (resolve-task "plugin")))]
    (is (= ["install" "uninstall"] (map first m)))))

>>>>>>> B.clj

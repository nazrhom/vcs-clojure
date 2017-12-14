(deftest test-get-subtasks
  (let [m (get-subtasks-and-docstrings-for (second (resolve-task "plugin")))]
    (is (= ["install" "uninstall"] (map first m)))))


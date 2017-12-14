(deftest test-new-with-just-project-name
  (leiningen.new/new nil "test-new-proj")
  (is (= #{"README.md" "project.clj" "resources" "src" "core.clj" "test"
           "doc" "intro.md" "test_new_proj" "core_test.clj" ".gitignore"
           ".hgignore" "LICENSE"}
         (set (map (memfn getName) (rest (file-seq (file "test-new-proj")))))))
  (delete-file-recursively (file "test-new-proj") :silently))


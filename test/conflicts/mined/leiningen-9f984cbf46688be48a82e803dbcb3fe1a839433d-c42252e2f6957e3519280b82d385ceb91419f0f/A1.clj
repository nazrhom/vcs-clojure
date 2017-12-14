(deftest test-checkout-deps
  (let [d1 (io/file (:root project) "checkouts" "d1")]
    (try
      (.mkdirs d1)
      (spit (io/file d1 "project.clj")
            (pr-str '(defproject hello "1.0")))
      (is (= (for [path ["src" "dev-resources" "resources" "target/classes" "foo"]]
               (lthelper/pathify (format "/tmp/lein-sample-project/checkouts/d1/%s" path)))
             (#'leiningen.core.classpath/checkout-deps-paths project)))
      (finally
       ;; can't recur from finally
       (dorun (map #(.delete %) (reverse (file-seq d1))))))))


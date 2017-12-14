(deftest ^{:online true} test-deps
  (delete-file-recursively (:library-path sample-project) true)
  (deps sample-project)
  (let [jars (set (map #(.getName %)
                       (.listFiles (file (:library-path sample-project)))))]
    (doseq [j ["jdom-1.0.jar" "tagsoup-1.2.jar" "rome-0.9.jar"]]
      (is (jars j)))))

(deftest ^{:online true} test-dev-deps-only
  (delete-file-recursively (:library-path dev-deps-project) true)
  (deps dev-deps-project)
  (let [jars (set (map #(.getName %)
                       (.listFiles (file (:root dev-deps-project)
                                         "lib" "dev"))))]
    (is (contains? jars "clojure-1.2.0.jar"))))

(deftest test-snapshots-releases
  (try
    (let [pr (assoc sample-project :omit-default-repositories true
                    :repositories {"clojars" {:url "http://clojars.org/repo/"
                                              :snapshots false}})
          ps (assoc sample-project :omit-default-repositories true
                    :repositories {"clojars" {:url "http://clojars.org/repo/"
                                              :releases false}})
          slamhound ['slamhound "1.1.0-SNAPSHOT"]
          hooke ['robert/hooke "1.0.1"]
          deps (fn [project]
                 (delete-file-recursively (apply m2-dir slamhound) :quiet)
                 (delete-file-recursively (apply m2-dir hooke) :quiet)
                 (leiningen.deps/deps project))]
      (deps (assoc pr :dependencies [hooke]))
      (is (lib-populated? ps #"hooke"))
      (deps (assoc ps :dependencies [slamhound]))
      (is (lib-populated? ps #"slamhound"))
      (let [snaps-repo-rel-dep (assoc ps :dependencies [hooke])]
        (is (thrown? Exception (with-no-log (deps snaps-repo-rel-dep)))))
      (let [rel-repo-snaps-dep (assoc pr :dependencies [slamhound])]
        (is (thrown? Exception (with-no-log (deps rel-repo-snaps-dep))))))
    (finally
     (delete-file-recursively (:library-path sample-project) :silently))))

(deftest ^{:online true} test-native-deps
  (delete-file-recursively (:library-path native-project) true)
  (delete-file-recursively (:native-path native-project) true)
  (deps native-project)
  (is (= (conj (get-in native-lib-files-map [(eval/get-os) (eval/get-arch)])
               ".gitkeep")
         (set (for [f (rest (file-seq (io/file (eval/native-arch-path
                                                native-project))))]
                (.getName f))))))

(deftest test-checksum-deps
  (delete-file-recursively (:library-path sample-project) true)
  (deps (assoc sample-project :checksum-deps true))
  (let [deps-ran (atom false)]
    (binding [do-deps (fn [& _] (reset! deps-ran true))]
      (deps (assoc sample-project :checksum-deps true))
      (is (not @deps-ran)))))

(deftest test-explicit-checksum-deps
  (delete-file-recursively (:library-path sample-project) true)
  (deps (assoc sample-project :checksum-deps true))
  (let [deps-ran (atom false)]
    (binding [do-deps (fn [& _] (reset! deps-ran true))]
      (apply-task "deps" (assoc sample-project :checksum-deps true) [] #())
      (is @deps-ran))))

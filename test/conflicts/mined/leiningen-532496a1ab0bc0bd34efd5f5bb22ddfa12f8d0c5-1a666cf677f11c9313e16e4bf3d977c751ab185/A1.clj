(deftest test-deps
  (let [sample-deps [["ring" "1.0.0"] ["rome" "0.9"] ["jdom" "1.0"]]]
    (doseq [[n v] sample-deps]
      (delete-file-recursively (m2-dir n v) :silently))
    (deps sample-project)
    (doseq [[n v] sample-deps]
      (is (.exists (m2-dir n v)) (str n " was not downloaded.")))))

(deftest test-snapshots-releases
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
    (is (.exists (m2-dir :robert/hooke "1.0.1")))
    (deps (assoc ps :dependencies [slamhound]))
    (is (.exists (m2-dir "slamhound" "1.1.0-SNAPSHOT")))
    (let [snaps-repo-rel-dep (assoc ps :dependencies [hooke])]
      (is (thrown? Exception (deps snaps-repo-rel-dep)))
      (is (not (.exists (m2-dir :robert/hooke "1.0.1")))))
    (let [rel-repo-snaps-dep (assoc pr :dependencies [slamhound])]
      (is (thrown? Exception (deps rel-repo-snaps-dep)))
      (is (not (.exists (m2-dir "slamhound" "1.1.0-SNAPSHOT"))))) ))

(deftest ^:busted test-native-deps
  (delete-file-recursively (:native-path native-project) true)
  (deps native-project)
  (is (= (conj (get-in native-lib-files-map [(eval/get-os) (eval/get-arch)])
               ".gitkeep")
         (set (for [f (rest (file-seq (io/file (eval/native-arch-path
                                                native-project))))]
                (.getName f))))))

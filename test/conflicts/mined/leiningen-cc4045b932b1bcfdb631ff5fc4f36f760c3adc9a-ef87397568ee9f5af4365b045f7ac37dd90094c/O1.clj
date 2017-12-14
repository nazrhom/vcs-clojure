(deftest test-merge-profile-repos
  (with-redefs [default-profiles test-profiles]
    (let [project
          (make
           {:profiles {:clojars {:repositories ^:replace
                                [["clojars.org" "https://clojars.org/repo/"]]}
                       :clj-2 {:repositories
                               [["clojars.org" "https://new-link.org/"]]}
                       :blue {:repositories
                              [["my-repo" "https://my-repo.org/"]]}
                       :empty {:repositories ^:replace []}}})]
      (is (= default-repositories
             (:repositories project)))
      (is (= []
             (-> (merge-profiles project [:empty])
                 :repositories)))
      (is (= [["my-repo" {:url "https://my-repo.org/"}]]
             (-> (merge-profiles project [:empty :blue])
                 :repositories)))
      (is (= [["clojars.org" {:url "https://clojars.org/repo/"}]]
             (-> (merge-profiles project [:clojars])
                 :repositories)))
      (is (= [["clojars.org" {:url "https://clojars.org/repo/"}]
              ["my-repo" {:url "https://my-repo.org/"}]]
             (-> (merge-profiles project [:clojars :blue])
                 :repositories)))
      (is (= [["clojars.org" {:url "https://new-link.org/"}]
              ["my-repo" {:url "https://my-repo.org/"}]]
             (-> (merge-profiles project [:clojars :blue :clj-2])
                 :repositories)))
      (is (= [["clojars.org" {:url "https://clojars.org/repo/"}]
              ["my-repo" {:url "https://my-repo.org/"}]]
             (-> (merge-profiles project [:clojars :blue :green])
                 :repositories)))
      (is (= [["clojars.org" {:url "https://clojars.org/repo/"}]
              ["my-repo" {:url "https://my-repo.org/red"}]]
             (-> (merge-profiles project [:blue :clojars :red])
                 :repositories)))
      (is (= [["my-repo" {:url "https://my-repo.org/red"}]
              ["clojars.org" {:url "https://new-link.org/"}]]
             (-> (merge-profiles project [:empty :red :clj-2 :green])
                 :repositories))))))


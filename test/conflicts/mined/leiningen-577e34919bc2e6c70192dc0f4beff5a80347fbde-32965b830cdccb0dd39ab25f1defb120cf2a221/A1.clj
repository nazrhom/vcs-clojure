(defmethod xml-tags ::project
  ([_ project]
     (let [reprofile #(relativize (project/merge-profiles project %))
           test-project (reprofile [:base :downstream :provided :dev :test])
           profiles (merge @project/default-profiles (:profiles project)
                           (project/project-profiles project))
           raw-deps (set (map dep-key (:dependencies project)))
           deps (concat (:dependencies project)
                        (for [dep (:dependencies (:provided profiles))]
                          (make-scope "provided" dep))
                        (for [profile (concat
                                       [:dev :test :base]
                                       (remove #{:provided :dev :test :base}
                                               test-profile-kws))
                              dep (:dependencies (profile profiles))
                              :when (not (and (= profile :base)
                                              (raw-deps (dep-key dep))))]
                          (make-scope "test" dep)))]
       (list
        [:project {:xsi:schemaLocation
                   (str "http://maven.apache.org/POM/4.0.0"
                        " http://maven.apache.org/xsd/maven-4.0.0.xsd")
                   :xmlns "http://maven.apache.org/POM/4.0.0"
                   :xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance"}
         [:modelVersion "4.0.0"]
         (and (:parent project) (xml-tags :parent (:parent project)))
         [:groupId (:group project)]
         [:artifactId (:name project)]
         [:packaging (:packaging project "jar")]
         [:version (:version project)]
         (and (:classifier project) [:classifier (:classifier project)])
         [:name (:name project)]
         [:description (:description project)]
         (xml-tags :url (:url project))
         (if-let [licenses (license-tags project)]
           [:licenses licenses])
         (xml-tags :mailing-list (:mailing-list project))
         (write-scm-tag (guess-scm project) project)
         ;; TODO: this results in lots of duplicate entries
         (xml-tags :build [project test-project])
         (xml-tags :repositories (:repositories project))
         (xml-tags :dependencies (distinct-key dep-key deps))
         (and (:pom-addition project) (:pom-addition project))]))))


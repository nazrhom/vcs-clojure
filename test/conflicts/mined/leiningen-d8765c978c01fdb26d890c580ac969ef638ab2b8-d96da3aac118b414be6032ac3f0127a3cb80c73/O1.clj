(deftest version-map->string-valid
  (doseq [[string parsed bumps] valid-semver-version-values]
    (is (= string (version-map->string parsed)))
    (doseq [[level string] bumps]
      (is (= (merge {:qualifier nil} (bump-version level parsed))
             (parse-semantic-version string))))))

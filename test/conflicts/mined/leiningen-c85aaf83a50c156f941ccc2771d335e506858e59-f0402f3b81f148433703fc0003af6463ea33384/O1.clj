(deftest test-tricky-name
  (let [jar-file (JarFile. (jar tricky-name))
        manifest (manifest-map (.getManifest jar-file))
        bin (slurp (.getInputStream
                    jar-file (.getEntry jar-file "bin/tricky-name")))
        bat (slurp (.getInputStream
                    jar-file (.getEntry jar-file "bin/tricky-name.bat")))]
    (is (= "bin/tricky-name" (manifest "Leiningen-shell-wrapper")))
    (is (re-find #"org/domain/tricky-name/1.0/tricky-name-1\.0\.jar" bin))))

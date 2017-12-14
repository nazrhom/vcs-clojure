(defn- update-setting [loc [p & ath] fn]
  (let [loc'  (-> loc (find-key p) next-value)
        loc'' (or loc' (-> loc
                           zip/rightmost
                           (insert-key-val p {})
                           zip/left))]
    (if (empty? ath)
      (zip/root (zip/edit loc'' fn ))
      (recur (-> loc'' zip/down zip/right) ath fn))))

(defn change
  "Rewrite project.clj with f applied to the value at key-or-path.
  TODO: document accepted args."
  [project key-or-path f & args]
  ;; cannot work with project map, want to preserve formatting, comments, etc
  (let [source (slurp (io/file (:root project) "project.clj"))]
    (spit "project.clj" (apply change-string source key-or-path f args))))

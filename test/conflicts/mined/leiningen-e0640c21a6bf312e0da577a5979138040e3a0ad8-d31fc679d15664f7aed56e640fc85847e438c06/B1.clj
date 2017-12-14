(defn- update-setting [loc [p & ath] fn]
  (let [loc'  (-> loc (find-key p) next-value)
        loc'' (or loc' (-> loc
                           zip/rightmost
                           (insert-entry p)
                           (insert-entry {})
                           zip/left))]
    (if-not (empty? ath)
      (recur (-> loc'' zip/down zip/right) ath fn)
      (zip/root
       (zip/edit loc''
                 (comp clj->sjacket fn sjacket->clj))))))

(defn change
  "Rewrite project.clj with f applied to the value at key-or-path.

TODO: document accepted args."
  [project key-or-path f & args]
  ;; cannot work with project map, want to preserve formatting, comments, etc
  (let [source (slurp (io/file (:root project) "project.clj"))]
    (spit "project.clj" (apply change-string source key-or-path f args))))

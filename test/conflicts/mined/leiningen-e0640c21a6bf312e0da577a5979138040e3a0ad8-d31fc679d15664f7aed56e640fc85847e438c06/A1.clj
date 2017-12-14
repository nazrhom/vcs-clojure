(defn- update-setting [proj [p & ath] fn]
  (let [loc (or (-> proj (find-key p) next-value)
                (-> proj
                    zip/rightmost
                    (insert-key-val p {})
                    zip/left))]
    (if (empty? ath)
      (zip/root (zip/edit loc fn))
      (recur (-> loc zip/down zip/right) ath fn))))

(defn change
  "Rewrite project.clj with f applied to the value at key-or-path.

The first argument should be a keyword (or mashed-together keywords for
nested values indicating which value to change). The second argument
should name a function var which will be called with the current value
as its first argument and the remaining task aruments as the rest.

This will append \"-SNAPSHOT\" to the current version:

    $ lein change version str \"-SNAPSHOT\"

When called programmatically, you may pass a coll of keywords for the
first arg or an actual function for the second.

Note that this task reads the project.clj file from disk rather than
honoring the project map, so profile merging or `update-in` invocations
will not effect it."
  [project key-or-path f & args]
  ;; cannot work with project map, want to preserve formatting, comments, etc
  (let [source (slurp (io/file (:root project) "project.clj"))]
    (spit "project.clj" (apply change-string source key-or-path f args))))

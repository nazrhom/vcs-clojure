(defn sign [file]
  (let [{:keys [err exit]} (user/gpg "--yes" "-ab" "--" file)]
    (when-not (zero? exit)
      (main/abort "Could not sign" (str file "\n" err)))
    (str file ".asc")))


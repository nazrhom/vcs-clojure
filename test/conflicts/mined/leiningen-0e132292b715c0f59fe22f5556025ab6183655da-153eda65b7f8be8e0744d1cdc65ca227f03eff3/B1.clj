(defn sign [file]
  (let [exit (binding [*out* (java.io.StringWriter.)
                       eval/*pump-in* false] ; gpg handles reading by itself
               (eval/sh (user/gpg-program) "--yes" "-ab" "--" file))]
    (when-not (zero? exit)
      (main/abort "Could not sign" (str file "\n" err)))
    (str file ".asc")))


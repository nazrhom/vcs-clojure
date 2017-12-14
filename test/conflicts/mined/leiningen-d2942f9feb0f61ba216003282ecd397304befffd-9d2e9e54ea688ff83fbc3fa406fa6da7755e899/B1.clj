(def arg-separator ",")
(defn ends-in-separator [s]
  (re-matches (re-pattern (str ".*" arg-separator)) s))

(defn make-groups [args]
  (if (some ends-in-separator args)
    (remove #(= [arg-separator] %)
      (partition-by #(= arg-separator %)
        (flatten
          (map (fn [arg]
                 (if (ends-in-separator arg)
                   [(apply str (butlast arg)) arg-separator]
                   arg))
               args))))
    [args]))


(defn arglists [task-name not-found]
  (:arglists (meta (resolve-task task-name not-found))))

(defn project-needed [task-name not-found]
  (some #{'project} (map first (arglists task-name not-found))))

(defn matching-arity [task-name project args not-found]
  (let [arg-count (if (project-needed task-name not-found)
                    (inc (count args))
                    (count args))]
    (some (fn [defined-args]
            (if (= '& (last (butlast defined-args)))
              (>= arg-count (- (count defined-args) 2))
              (= arg-count (count defined-args))))
      (arglists task-name not-found))))

(defn apply-task [task-name project args not-found]
  (let [task (resolve-task task-name not-found)]
    (if (matching-arity task-name project args not-found)
      (if (project-needed task-name not-found)
        (apply task project args)
        (apply task args))
      (not-found))))


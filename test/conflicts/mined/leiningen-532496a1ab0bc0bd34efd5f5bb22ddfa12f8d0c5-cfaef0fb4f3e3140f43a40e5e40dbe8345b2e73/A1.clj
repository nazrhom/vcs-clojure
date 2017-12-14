(defn subtask-help-for
  [task-ns task]
  (if-let [subtasks (seq (get-subtasks-and-docstrings-for task))]
    (let [longest-key-length (apply max (map count (keys subtasks)))]
      (string/join "\n"
                   (concat ["\n\nSubtasks available:"]
                           (for [[subtask doc] subtasks]
                             (formatted-help subtask doc
                                             longest-key-length)))))))


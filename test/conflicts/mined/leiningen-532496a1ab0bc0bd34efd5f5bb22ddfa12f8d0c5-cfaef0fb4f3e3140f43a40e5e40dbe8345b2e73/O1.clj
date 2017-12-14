(defn subtask-help-for
  [task-ns task]
  (let [subtasks (get-subtasks-and-docstrings-for task)]
    (if (empty? subtasks)
      nil
      (let [longest-key-length (apply max (map count (keys subtasks)))
            help-fn (ns-resolve task-ns 'help)]
        (string/join "\n"
          (concat ["\n\nSubtasks available:"]
                  (for [[subtask doc] subtasks]
                    (formatted-help subtask doc longest-key-length))))))))


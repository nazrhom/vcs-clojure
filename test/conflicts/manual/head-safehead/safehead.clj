(defn shead
  "safe head"
  [l d]
  (if (nil? l)
    (d)
    (first l)))

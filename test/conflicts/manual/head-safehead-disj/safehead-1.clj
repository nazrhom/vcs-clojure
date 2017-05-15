(defn shead
  "unsafe head"
  [l d]
  (if (nil? l)
    (d)
    (first l)))

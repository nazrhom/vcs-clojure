(defn head
  "unsafe head"
  [l d]
  (if (nil? l)
    (d)
    (first l)))

(defn ^{:doc "Read an Excel file into a dataset. Note: cells containing formulas will be
empty upon import.
Options are:
:sheet either a String for the tab name or an int for the sheet index -- defaults to 0

 Examples:
   (use '(incanter core io excel))
   (view (read-xls \"http://incanter.org/data/aus-airline-passengers.xls\"))

   (use '(incanter core charts excel))
   ;; read .xls file of Australian airline passenger data from the 1950s.
   (with-data (read-xls \"http://incanter.org/data/aus-airline-passengers.xls\")
   (view $data)
   ;; time-series-plot needs time in millisecs
   ;; create a function, to-millis, to convert a sequence of Date objects
   ;; to a sequence of milliseconds
   (let [to-millis (fn [dates] (map #(.getTime %) dates))] 
     (view (time-series-plot (to-millis ($ :date)) ($ :passengers)))))

"}
  read-xls
  [^String filename  & {:keys [sheet] :or {sheet 0}}]
  (with-open [in-fs (get-input-stream filename)]
      (let [workbook  (HSSFWorkbook. in-fs)
            wsheet     (get-workbook-sheet workbook sheet)
            rows-it   (iterator-seq (. wsheet iterator))
            rowi      (. (first rows-it) iterator)
            colnames  (doall (map get-cell-value (iterator-seq rowi)))
            data      (map #(iterator-seq (. % iterator)) (rest rows-it))]
        (dataset colnames
                 (map (fn [d] (map get-cell-value d)) data)))))


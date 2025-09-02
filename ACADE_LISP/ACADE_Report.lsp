(defun c:ConsolidateBOM (/ doc oldTable newTable items locations rows cols row col total qty srcRow itemCol qtyCol locCol headers modelSpace oldColWidths oldRowHeights oldStyle)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq modelSpace (vla-get-ModelSpace doc))

  ;; Get the selected table
  (setq oldTable (vlax-ename->vla-object (car (entsel "\nSelect the original table: "))))

  ;; Retrieve the table style from the old table
  (setq oldStyle (vla-get-StyleName oldTable))

  ;; Get the number of rows and columns in the original table
  (setq rows (vla-get-Rows oldTable))
  (setq cols (vla-get-Columns oldTable))
  (princ (strcat "\nOriginal table has " (itoa rows) " rows and " (itoa cols) " columns."))

  ;; Read all column headers
  (setq headers '())
  (setq col 0)
  (repeat cols
    (setq headers (cons (vla-GetText oldTable 1 col) headers))
    (setq col (1+ col))
  )
  (setq headers (reverse headers))
  (princ (strcat "\nHeaders: " (vl-princ-to-string headers)))

  ;; Find the ITEM, QTY, and TEMP_LOC columns
  (setq itemCol (vl-position "ITEM" headers))
  (setq qtyCol (vl-position "QTY" headers))
  (setq locCol (vl-position "TEMP_LOC" headers))

  ;; Check if all required columns were found
  (if (or (null itemCol) (null qtyCol) (null locCol))
    (progn
      (princ "\nError: Could not find ITEM, QTY, or TEMP_LOC columns.")
      (exit)
    )
  )
  (princ (strcat "\nITEM column: " (itoa itemCol) ", QTY column: " (itoa qtyCol) ", TEMP_LOC column: " (itoa locCol)))

  ;; Extract unique locations and items
  (setq locations '())
  (setq items '())
  (setq row 2)
  (repeat (- rows 2)
    (setq loc (vla-GetText oldTable row locCol))
    (setq item (vla-GetText oldTable row itemCol))
    (if (and (/= loc "") (not (member loc locations)))
      (setq locations (cons loc locations)))
    (if (not (member item items))
      (setq items (cons item items)))
    (setq row (1+ row)))
  (setq locations (vl-sort locations '<))
  (setq items (vl-sort items '<))
  (princ (strcat "\nUnique locations: " (vl-princ-to-string locations)))
  (princ (strcat "\nUnique items: " (vl-princ-to-string items)))

  ;; Attempt to create a new table with minimal parameters
  (setq newTable nil)
  (setq error-msg
    (vl-catch-all-apply
      '(lambda ()
        (setq newTable (vla-AddTable modelSpace 
                                     (vlax-3d-point 0 0 0) 
                                     5  ; Rows
                                     5  ; Columns
                                     10 ; Row height
                                     20)) ; Column width
        (princ "\nNew table created successfully with minimal parameters.")
      )
    )
  )
  
  (if (vl-catch-all-error-p error-msg)
    (progn
      (princ "\nError: Failed to create new table with minimal parameters.")
      (princ (strcat "\nError details: " (vl-catch-all-error-message error-msg)))
      (exit)
    )
  )

  ;; If we've made it this far, the table was created successfully
  (princ (strcat "\nNew table created with " (itoa (vla-get-Rows newTable)) " rows and " (itoa (vla-get-Columns newTable)) " columns."))
  
  ;; Attempt to modify the new table
  (setq error-msg
    (vl-catch-all-apply
      '(lambda ()
        (vla-put-StyleName newTable oldStyle)
        (vla-SetText newTable 0 0 "Test")
        (princ "\nSuccessfully modified the new table.")
      )
    )
  )
  
  (if (vl-catch-all-error-p error-msg)
    (progn
      (princ "\nError: Failed to modify the new table.")
      (princ (strcat "\nError details: " (vl-catch-all-error-message error-msg)))
      (exit)
    )
  )

  (princ "\nScript completed successfully.")
  (princ)
)

(defun c:ShowReportFilename ()
  (setq filename (vl-bb-ref 'FNAM))
  (if filename
    (progn
      (princ "\nLast report filename: ")
      (princ filename)
      (princ)
    )
    (princ "\nNo report file found.")
  )
  (princ)
)

(defun c:ReadLastReport (/ filename data)
  (setq filename (vl-bb-ref 'FNAM))
  (if filename
    (if (is-xls-file filename)
      (progn
        (princ "\nReading last report file: ")
        (princ filename)
        (princ "\n\nFile contents summary:\n")
        
        (setq data (read-excel-file filename))
        
        (if data
          (display-full-data data)
          (princ "\nUnable to read file data. See error message above.")
        )
      )
      (princ "\nThe file is not an .xls file. Please generate an Excel report.")
    )
    (princ "\nNo report file found.")
  )
  (princ)
)

(defun is-xls-file (filename)
  (= (strcase (vl-filename-extension filename)) ".XLS")
)

(defun read-excel-file (filename / excel workbook sheet usedrange data error-msg)
  (setq excel nil)
  (setq error-msg
    (vl-catch-all-apply
      '(lambda ()
        (setq excel (vlax-create-object "Excel.Application"))
        (vlax-put-property excel 'Visible :vlax-false)
        (setq workbook (vlax-invoke-method (vlax-get-property excel 'Workbooks) 'Open filename))
        (setq sheet (vlax-get-property workbook 'ActiveSheet))
        (setq usedrange (vlax-get-property sheet 'UsedRange))
        (setq data (read-range-data excel usedrange))
        (vlax-invoke-method workbook 'Close :vlax-false)
        nil  ; Return nil if no error occurred
      )
    )
  )
  (if excel (progn
    (vlax-invoke-method excel 'Quit)
    (vlax-release-object excel)
  ))
  (if error-msg
    (progn
      (princ "\nError reading Excel file: ")
      (princ (vl-catch-all-error-message error-msg))
      nil
    )
    data
  )
)

(defun read-range-data (excel range / rows cols data row col cell-value)
  (setq rows (vlax-get-property range 'Rows))
  (setq cols (vlax-get-property range 'Columns))
  (setq data '())
  (setq row 1)
  (while (<= row (vlax-get-property rows 'Count))
    (setq row-data '())
    (setq col 1)
    (while (<= col (vlax-get-property cols 'Count))
      (setq cell-value 
        (vlax-variant-value 
          (vlax-invoke-method 
            (vlax-get-property excel 'WorksheetFunction)
            'Index
            (vlax-get-property range 'Value)
            row
            col
          )
        )
      )
      (setq row-data (append row-data (list cell-value)))
      (setq col (1+ col))
    )
    (setq data (append data (list row-data)))
    (setq row (1+ row))
  )
  data
)

(defun display-full-data (data)
  (princ (strcat "\nTotal number of rows: " (itoa (length data))))
  (princ (strcat "\nNumber of columns: " (itoa (length (car data)))))
  (princ "\n\nColumn Headers:")
  (foreach header (car data)
    (princ (strcat "\n - " (vl-princ-to-string header)))
  )
  (princ (strcat "\n\nNumber of data rows: " (itoa (1- (length data)))))
  (princ "\n\nFull Data:")
  (foreach row (cdr data)
    (princ "\n")
    (foreach cell row
      (princ (strcat (vl-princ-to-string cell) " | "))
    )
  )
)
(defun c:ChangeTableFont (/ ent tbl row col val startRow endRow startCol endCol textStyle tableStyle)
  (initget 1 "ACAD_TABLE")
  (setq ent (entsel "\nSelect a table: "))
  (if ent
    (progn
      (setq tbl (vlax-ename->vla-object (car ent)))
      (setq startRow (getint "\nEnter start row: "))
      (setq endRow (getint "\nEnter end row: "))
      (setq startCol (getint "\nEnter start column: "))
      (setq endCol (getint "\nEnter end column: "))
      (setq textStyle (getstring "\nEnter text style: ")) ; Ask the user for a text style
      (setq tableStyle (vla-Add (vla-get-TableStyles (vla-get-ActiveDocument (vlax-get-acad-object))) "NewTableStyle")) ; Create a new table style
      (vla-put-TextStyle tableStyle textStyle) ; Set the text style of the table style
      (setq row startRow) ; Initialize row before using it
      (repeat (+ (- endRow startRow) 1)
        (setq col startCol)
        (repeat (+ (- endCol startCol) 1)
          (vla-SetCellFormat tbl row col tableStyle) ; Set the format of the cell
        )
        (setq row (+ row 1))
      )
    )
    (alert "No table selected.")
  )
  (princ)
)
(defun c:HighlightNonStraightLines (/ ss count i ent calAng convAng pt1 pt2)
  (setq ss (ssget "X" '((0 . "LINE")))) ; Get all LINE entities
  (if ss
    (progn
      (setq count 0)
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)) ; Get entity name from selection set
        (setq pt1 (cdr (assoc 10 (entget ent)))) ; Get start point of line
        (setq pt2 (cdr (assoc 11 (entget ent)))) ; Get end point of line
        (setq calAng (angle pt1 pt2)) ; Calculate angle of line
        (setq convAng (atof (angtos calAng 0 2))) ; Convert angle to radians
        (if (and (/= convAng 0.0) (/= convAng 90.00) (/= convAng 180.00) (/= conVang 270.00)) ; Check if angle is not 0, 90, 180 or 270 degrees
          (progn
            ;(entmod (append (entget ent) '((62 . 1)))) ; Highlight line
            (grdraw pt1 pt2 1 1) ; Draw a temporary line in red
            (setq count (1+ count))
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\nNumber of non-straight lines: " (itoa count)))
    )
    (princ "\nNo line entities found.")
  )
  (princ)
)



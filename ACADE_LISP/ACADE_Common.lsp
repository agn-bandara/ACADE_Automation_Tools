(defun c:DrawSineWave (/ pt1 length amplitude wavelength numPoints i x y)
  (setq pt1 (getpoint "\nSelect start point: "))
  (setq length (getreal "\nEnter length: "))
  (setq amplitude (getreal "\nEnter amplitude: "))
  (setq wavelength (getreal "\nEnter wavelength: "))
  (setq numPoints 100)
  
  (entmake (list (cons 0 "POLYLINE") (cons 70 0)))
  
  (setq i 0)
  (repeat (1+ numPoints)
    (setq x (* (/ length numPoints) i))
    (setq y (* amplitude (sin (/ (* 2 pi x) wavelength))))
    (setq pt (list (+ (car pt1) x) (+ (cadr pt1) y) 0))
    (entmake (list (cons 0 "VERTEX") (cons 10 pt)))
    (setq i (1+ i))
  )
  
  (entmake (list (cons 0 "SEQEND")))
  (princ)
)

(defun c:AddColumnToRight ( / tbl colCount)
  (setq tbl (vlax-ename->vla-object (car (entsel "\nSelect table: "))))
  (setq colCount (vla-get-Columns tbl))
  (vla-InsertColumns tbl colCount 1 1.0)
  (princ)
)

;;------------------------------------------------------------------;
;; Function to delete all layouts except the "Model" layout.      --;
;; Command: DELALLAYOUTS                                          --;
;;------------------------------------------------------------------;
(defun c:DelAllLayouts ( / acadObj doc layouts modelLayout layoutName errobj layoutNameToDelete layoutsToDelete)
  (vl-load-com) ; Ensure Visual LISP extensions are loaded

  (princ "\nInitializing layout deletion process...")

  ;; Get the AutoCAD application object and the active document
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-activedocument acadObj))

  ;; Get the collection of layouts
  (setq layouts (vla-get-layouts doc))

  ;; Attempt to set the "Model" tab as the active layout
  ;; This is crucial because the active layout cannot be deleted.
  (princ "\nAttempting to switch to 'Model' tab...")
  (if (setq modelLayoutObject (vl-catch-all-apply 'vla-item (list layouts "Model")))
    (if (vl-catch-all-error-p modelLayoutObject)
      (princ (strcat "\nError finding 'Model' layout: " (vl-catch-all-error-message modelLayoutObject)
                     ". Cannot ensure safe deletion of other layouts."))
      (progn
        (setq errobj (vl-catch-all-apply 'vla-put-activelayout (list doc modelLayoutObject)))
        (if (vl-catch-all-error-p errobj)
          (princ (strcat "\nWARNING: Could not switch to 'Model' tab. "
                         "The currently active layout may not be deletable. Error: "
                         (vl-catch-all-error-message errobj)))
          (princ "\nSuccessfully switched to 'Model' tab.")
        )
      )
    )
    (princ "\nCRITICAL: Could not retrieve 'Model' layout object. This is highly unusual.")
  )

  (princ "\n\nIdentifying layouts for deletion (excluding 'Model'):")

  ;; Collect names of layouts to delete.
  ;; This is safer than deleting from a collection while iterating over it directly with vlax-for in some complex scenarios,
  ;; though for layouts, direct deletion in vlax-for often works. This approach is more robust.
  (setq layoutsToDelete '())
  (setq layouts (vla-get-layouts doc)) ; Re-fetch the layouts collection

  (vlax-for layout layouts
    (setq layoutName (vla-get-name layout))
    (if (not (eq (strcase layoutName) "MODEL"))
      (progn
        (princ (strcat "\n- Marked for deletion: '" layoutName "'"))
        (setq layoutsToDelete (cons layoutName layoutsToDelete))
      )
      (princ (strcat "\n- Skipping: '" layoutName "' (Model Space)"))
    )
  )
  
  (setq layoutsToDelete (reverse layoutsToDelete)) ; Process in original order if preferred, though order of deletion doesn't usually matter.

  (if layoutsToDelete
    (progn
      (princ "\n\nStarting deletion of marked layouts:")
      (foreach layoutNameToDelete layoutsToDelete
        (princ (strcat "\n- Attempting to delete layout: '" layoutNameToDelete "'..."))
        ;; Retrieve the layout object again by name before deleting
        (setq layoutObj (vl-catch-all-apply 'vla-item (list layouts layoutNameToDelete)))
        (if (vl-catch-all-error-p layoutObj)
          (princ (strcat " FAILED. Error finding layout '" layoutNameToDelete "' for deletion: " (vl-catch-all-error-message layoutObj)))
          (progn
            (setq errobj (vl-catch-all-apply 'vla-delete (list layoutObj)))
            (if (vl-catch-all-error-p errobj)
              (princ (strcat " FAILED. Error: " (vl-catch-all-error-message errobj)))
              (princ " DELETED.")
            )
          )
        )
      )
    )
    (princ "\n\nNo layouts (other than 'Model') found to delete.")
  )

  (princ "\n\nLayout deletion process finished.")
  (princ "\nAny layouts that could not be deleted (e.g., due to errors or protections) will remain.")
  (princ "\nTo use, load this .LSP file and type DELALLAYOUTS at the AutoCAD command prompt.")
  (princ) ; Cleanly exits the function without returning the last evaluated expression
)


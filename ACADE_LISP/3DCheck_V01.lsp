
(defun get-bounding-box (ename / obj bbox)
    (if (setq obj (vlax-ename->vla-object ename))
        (progn
            (vla-getboundingbox obj 'minpt 'maxpt)
            (list (vlax-safearray->list minpt) (vlax-safearray->list maxpt))
        )
    )
)

(defun modify-object-properties (ename / obj)
    (if (setq obj (vlax-ename->vla-object ename))
        (progn
            (vla-put-layer obj "0")
            (vla-put-color obj acByBlock)
            T
        )
        nil
    )
)

;; Function 1: Get Bounding box, Calculate width, Height and Depth
(defun bbox3D (ename / obj bbox minpt maxpt width height depth)
    "Returns a list with bounding box dimensions (width height depth)"
    (if (setq obj (vlax-ename->vla-object ename))
        (progn
            (vla-getboundingbox obj 'minpt 'maxpt)
            (setq minpt (vlax-safearray->list minpt)
                  maxpt (vlax-safearray->list maxpt)
                  width (- (car maxpt) (car minpt))
                  height (- (cadr maxpt) (cadr minpt))
                  depth (- (caddr maxpt) (caddr minpt)))
            (list width height depth)
        )
    
    )
  nil
)

;; Function: Interactive bounding box calculation with user selection
(defun c:bbox3DSel (/ ent obj bbox minpt maxpt width height depth dimensions result)
    "Allow user to select an object and calculate its bounding box dimensions"
    (princ "\nSelect an object to calculate bounding box dimensions: ")
    (if (setq ent (car (entsel)))
        (progn
            (if (setq obj (vlax-ename->vla-object ent))
                (progn
                    (vla-getboundingbox obj 'minpt 'maxpt)
                    (setq minpt (vlax-safearray->list minpt)
                          maxpt (vlax-safearray->list maxpt)
                          width (- (car maxpt) (car minpt))
                          height (- (cadr maxpt) (cadr minpt))
                          depth (- (caddr maxpt) (caddr minpt))
                          dimensions (list width height depth))
                    
                    (princ "\n--- Bounding Box Dimensions ---")
                    (princ (strcat "\nWidth:  " (rtos width 2 3)))
                    (princ (strcat "\nHeight: " (rtos height 2 3)))
                    (princ (strcat "\nDepth:  " (rtos depth 2 3)))
                    (princ "\n--- Coordinate Points ---")
                    (princ (strcat "\nMin Point: " (vl-princ-to-string minpt)))
                    (princ (strcat "\nMax Point: " (vl-princ-to-string maxpt)))
                    (princ (strcat "\n--- Dimensions List ---"))
                    (princ (strcat "\n(W H D): " (vl-princ-to-string dimensions)))
                    
                    ;; Return the dimensions list
                    dimensions
                )
                (progn
                    (princ "\nError: Could not process the selected object.")
                    nil
                )
            )
        )
        (progn
            (princ "\nNo object selected.")
            nil
        )
    )
    (princ)
    nil
)

;; Function 2: Update objects like bbox but skip objects with specified dimensions
(defun c:upd3DSkip (skip-dimensions / ss obj bbox minpt maxpt width height depth count modified i should-skip result)
    "Update objects like bbox but skip objects matching dimensions (width height depth)"
    "Use -1 or nil for dimensions to ignore. Example: (100 -1 50) skips objects with width=100 AND depth=50"
    (if (not skip-dimensions)
        (progn
            (princ "\nError: Skip dimensions must be specified as (width height depth)")
            (princ "\nExample: (c:upd3DSkip '(100 -1 50))")
            (princ)
            (exit)
        )
    )
    (princ (strcat "\nProcessing all objects, skipping dimensions: " (vl-princ-to-string skip-dimensions)))
    (if (setq ss (ssget "X"))
        (progn
            (setq count (sslength ss)
                  modified 0
                  i 0)
            (princ (strcat "\nChecking " (itoa count) " objects..."))
            (while (< i count)
                (setq obj (ssname ss i))
                (setq bbox (get-bounding-box obj))
                (if bbox
                    (progn
                        (setq minpt (car bbox)
                              maxpt (cadr bbox)
                              width (- (car maxpt) (car minpt))
                              height (- (cadr maxpt) (cadr minpt))
                              depth (- (caddr maxpt) (caddr minpt)))
                        (princ (strcat "\nObject " (itoa (1+ i)) 
                                     " - W:" (rtos width 2 3)
                                     " H:" (rtos height 2 3)
                                     " D:" (rtos depth 2 3)))
                        
                        ;; Check if object should be skipped using AND logic
                        (setq should-skip 
                            (and 
                                (or (< (nth 0 skip-dimensions) 0) (equal width (nth 0 skip-dimensions) 0.001))
                                (or (< (nth 1 skip-dimensions) 0) (equal height (nth 1 skip-dimensions) 0.001))
                                (or (< (nth 2 skip-dimensions) 0) (equal depth (nth 2 skip-dimensions) 0.001))
                            )
                        )
                        
                        (if (not should-skip)
                            (progn
                                (modify-object-properties obj)
                                (setq modified (1+ modified))
                                (princ " - Modified (Layer: 0, Color: ByBlock)")
                            )
                            (princ " - Skipped (Matches skip criteria)")
                        )
                    )
                    (princ (strcat "\nObject " (itoa (1+ i)) " - Error: Could not get bounding box."))
                )
                (setq i (1+ i))
            )
            (progn
            (princ (strcat "\n\nSummary: " (itoa modified) " of " (itoa count) " objects modified."))
            (setq result (strcat "MODIFIED " (itoa modified)))
            )
        )
        (princ "\nNo objects found in the drawing.")
    )
    (princ)
    result
)

;; Function 3: Update objects like bbox but only consider objects with specified dimensions
(defun c:upd3DSel (select-dimensions / ss obj bbox minpt maxpt width height depth count modified i should-select)
    "Update objects like bbox but only consider objects matching dimensions (width height depth)"
    "Use -1 or nil for dimensions to ignore. Example: (100 -1 50) selects objects with width=100 AND depth=50"
    (if (not select-dimensions)
        (progn
            (princ "\nError: Select dimensions must be specified as (width height depth)")
            (princ "\nExample: (c:upd3DSel '(100 -1 50))")
            (princ)
            (exit)
        )
    )
    (princ (strcat "\nProcessing objects with dimensions: " (vl-princ-to-string select-dimensions)))
    (if (setq ss (ssget "X"))
        (progn
            (setq count (sslength ss)
                  modified 0
                  i 0)
            (princ (strcat "\nChecking " (itoa count) " objects..."))
            (while (< i count)
                (setq obj (ssname ss i))
                (setq bbox (get-bounding-box obj))
                (if bbox
                    (progn
                        (setq minpt (car bbox)
                              maxpt (cadr bbox)
                              width (- (car maxpt) (car minpt))
                              height (- (cadr maxpt) (cadr minpt))
                              depth (- (caddr maxpt) (caddr minpt)))
                        (princ (strcat "\nObject " (itoa (1+ i)) 
                                     " - W:" (rtos width 2 3)
                                     " H:" (rtos height 2 3)
                                     " D:" (rtos depth 2 3)))
                        
                        ;; Check if object should be selected using AND logic
                        (setq should-select 
                            (and 
                                (or (< (nth 0 select-dimensions) 0) (equal width (nth 0 select-dimensions) 0.001))
                                (or (< (nth 1 select-dimensions) 0) (equal height (nth 1 select-dimensions) 0.001))
                                (or (< (nth 2 select-dimensions) 0) (equal depth (nth 2 select-dimensions) 0.001))
                            )
                        )
                        
                        (if should-select
                            (progn
                                (modify-object-properties obj)
                                (setq modified (1+ modified))
                                (princ " - Modified (Layer: 0, Color: ByBlock)")
                            )
                            (princ " - Skipped (Does not match selection criteria)")
                        )
                    )
                    (princ (strcat "\nObject " (itoa (1+ i)) " - Error: Could not get bounding box."))
                )
                (setq i (1+ i))
            )
            (progn
              (setq result (strcat "MODIFIED " (itoa modified)))
              (princ (strcat "\n\nSummary: " (itoa modified) " of " (itoa count) " objects modified."))
            )
        )
        (princ "\nNo objects found in the drawing.")
    )
    (princ)
    result
)

;; Function 4: Check and count objects with specified bounding box dimensions
(defun c:bbox3DCheck (check-dimensions / ss obj bbox minpt maxpt width height depth count matched i should-match match-list result)
    "Count objects with matching bounding box dimensions (width height depth)"
    "Use -1 for dimensions to ignore. Example: (100 -1 50) counts objects with width=100 AND depth=50"
    (if (not check-dimensions)
        (progn
            (princ "\nError: Check dimensions must be specified as (width height depth)")
            (princ "\nExample: (c:bbox3DCheck '(100 -1 50))")
            (princ "\nExample: (c:bbox3DCheck '(100 200 50)) - checks all three dimensions")
            (princ "\nExample: (c:bbox3DCheck '(-1 200 -1)) - checks only height dimension")
            (princ)
            (exit)
        )
    )
    (princ (strcat "\nChecking objects with dimensions: " (vl-princ-to-string check-dimensions)))
    (princ "\nDimension criteria:")
    (princ (strcat "\n  Width:  " (if (< (nth 0 check-dimensions) 0) "Any" (rtos (nth 0 check-dimensions) 2 3))))
    (princ (strcat "\n  Height: " (if (< (nth 1 check-dimensions) 0) "Any" (rtos (nth 1 check-dimensions) 2 3))))
    (princ (strcat "\n  Depth:  " (if (< (nth 2 check-dimensions) 0) "Any" (rtos (nth 2 check-dimensions) 2 3))))
    
    (if (setq ss (ssget "X"))
        (progn
            (setq count (sslength ss)
                  matched 0
                  i 0
                  match-list '())
            (princ (strcat "\n\nAnalyzing " (itoa count) " objects..."))
            (while (< i count)
                (setq obj (ssname ss i))
                (setq bbox (get-bounding-box obj))
                (if bbox
                    (progn
                        (setq minpt (car bbox)
                              maxpt (cadr bbox)
                              width (- (car maxpt) (car minpt))
                              height (- (cadr maxpt) (cadr minpt))
                              depth (- (caddr maxpt) (caddr minpt)))
                        
                        ;; Check if object matches the criteria using AND logic
                        (setq should-match 
                            (and 
                                (or (< (nth 0 check-dimensions) 0) (equal width (nth 0 check-dimensions) 0.001))
                                (or (< (nth 1 check-dimensions) 0) (equal height (nth 1 check-dimensions) 0.001))
                                (or (< (nth 2 check-dimensions) 0) (equal depth (nth 2 check-dimensions) 0.001))
                            )
                        )
                        
                        (if should-match
                            (progn
                                (setq matched (1+ matched))
                                (setq match-list (cons (list obj width height depth) match-list))
                                (princ (strcat "\nMatch " (itoa matched) 
                                             " - W:" (rtos width 2 3)
                                             " H:" (rtos height 2 3)
                                             " D:" (rtos depth 2 3)))
                            )
                        )
                    )
                )
                (setq i (1+ i))
            )
            (princ "\n")
            (princ "\n--- RESULTS ---")
            (princ (strcat "\nTotal objects checked: " (itoa count)))
            (princ (strcat "\nObjects matching criteria: " (itoa matched)))
            (if (> matched 0)
                (progn
                    (princ "\n\nMatched objects summary:")
                    (setq i 1)
                    (foreach match-item (reverse match-list)
                        (princ (strcat "\n  " (itoa i) ". W:" 
                                     (rtos (nth 1 match-item) 2 3) 
                                     " H:" (rtos (nth 2 match-item) 2 3)
                                     " D:" (rtos (nth 3 match-item) 2 3)))
                        (setq i (1+ i))
                    )
                    (setq result (strcat "FOUND " (itoa matched)))
                )
                (progn
                  (setq result "NOT_FOUND")
                  (princ "\nNo objects found matching the specified criteria.")
                )
            )
        )
        (progn
            (setq result "NO_OBJECTS")
            (princ "\nNo objects found in the drawing.")
            0
        )
    )
    (princ)
    ;; Return the count of matched objects
    result
)

;; Helper function: bbox3DCheck without the c: prefix for programmatic use
(defun bbox3DCheck (check-dimensions / ss obj bbox minpt maxpt width height depth count matched i should-match)
    "Count objects with matching bounding box dimensions - returns count only"
    "Use -1 for dimensions to ignore. Example: (bbox3DCheck '(100 -1 50))"
    (if (setq ss (ssget "X"))
        (progn
            (setq count (sslength ss)
                  matched 0
                  i 0)
            (while (< i count)
                (setq obj (ssname ss i))
                (setq bbox (get-bounding-box obj))
                (if bbox
                    (progn
                        (setq minpt (car bbox)
                              maxpt (cadr bbox)
                              width (- (car maxpt) (car minpt))
                              height (- (cadr maxpt) (cadr minpt))
                              depth (- (caddr maxpt) (caddr minpt)))
                        
                        ;; Check if object matches the criteria using AND logic
                        (setq should-match 
                            (and 
                                (or (< (nth 0 check-dimensions) 0) (equal width (nth 0 check-dimensions) 0.001))
                                (or (< (nth 1 check-dimensions) 0) (equal height (nth 1 check-dimensions) 0.001))
                                (or (< (nth 2 check-dimensions) 0) (equal depth (nth 2 check-dimensions) 0.001))
                            )
                        )
                        
                        (if should-match
                            (setq matched (1+ matched))
                        )
                    )
                )
                (setq i (1+ i))
            )
            matched
        )
        0
    )
)

;; Function 5: Create 3D surfaces in the middle of bounding boxes
(defun c:bbox3DSurface (filter-dimensions axis / ss obj bbox minpt maxpt width height depth count created i should-match center-pt pt1 pt2 pt3 pt4)
    "Create rectangular 3D surfaces in the middle of objects matching dimensions"
    "filter-dimensions: (width height depth) - Use -1 to ignore dimension"
    "axis: 'X', 'Y', or 'Z' - perpendicular axis for the surface"
    "Example: (c:bbox3DSurface '(100 -1 50) 'Z) - creates XY plane surfaces for objects with W=100, D=50"
    
    (if (not filter-dimensions)
        (progn
            (princ "\nError: Filter dimensions must be specified as (width height depth)")
            (princ "\nExample: (c:bbox3DSurface '(100 -1 50) 'Z)")
            (princ)
            (exit)
        )
    )
    
    (if (not axis)
        (progn
            (princ "\nError: Perpendicular axis must be specified ('X', 'Y', or 'Z')")
            (princ "\nExample: (c:bbox3DSurface '(100 -1 50) 'Z)")
            (princ)
            (exit)
        )
    )
    
    ;; Validate axis input
    (if (not (member (strcase (vl-princ-to-string axis)) '("X" "Y" "Z")))
        (progn
            (princ "\nError: Axis must be 'X', 'Y', or 'Z'")
            (princ)
            (exit)
        )
    )
    
    (setq axis (strcase (vl-princ-to-string axis)))
    
    (princ (strcat "\nCreating 3D surfaces for objects with dimensions: " (vl-princ-to-string filter-dimensions)))
    (princ (strcat "\nSurface perpendicular to " axis "-axis"))
    (princ "\nDimension criteria:")
    (princ (strcat "\n  Width:  " (if (< (nth 0 filter-dimensions) 0) "Any" (rtos (nth 0 filter-dimensions) 2 3))))
    (princ (strcat "\n  Height: " (if (< (nth 1 filter-dimensions) 0) "Any" (rtos (nth 1 filter-dimensions) 2 3))))
    (princ (strcat "\n  Depth:  " (if (< (nth 2 filter-dimensions) 0) "Any" (rtos (nth 2 filter-dimensions) 2 3))))
    
    (if (setq ss (ssget "X"))
        (progn
            (setq count (sslength ss)
                  created 0
                  i 0)
            (princ (strcat "\n\nProcessing " (itoa count) " objects..."))
            (while (< i count)
                (setq obj (ssname ss i))
                (setq bbox (get-bounding-box obj))
                (if bbox
                    (progn
                        (setq minpt (car bbox)
                              maxpt (cadr bbox)
                              width (- (car maxpt) (car minpt))
                              height (- (cadr maxpt) (cadr minpt))
                              depth (- (caddr maxpt) (caddr minpt)))
                        
                        ;; Check if object matches the criteria using AND logic
                        (setq should-match 
                            (and 
                                (or (< (nth 0 filter-dimensions) 0) (equal width (nth 0 filter-dimensions) 0.001))
                                (or (< (nth 1 filter-dimensions) 0) (equal height (nth 1 filter-dimensions) 0.001))
                                (or (< (nth 2 filter-dimensions) 0) (equal depth (nth 2 filter-dimensions) 0.001))
                            )
                        )
                        
                        (if should-match
                            (progn
                                ;; Calculate center point of bounding box
                                (setq center-pt (list 
                                    (/ (+ (car minpt) (car maxpt)) 2.0)
                                    (/ (+ (cadr minpt) (cadr maxpt)) 2.0)
                                    (/ (+ (caddr minpt) (caddr maxpt)) 2.0)
                                ))
                                
                                ;; Create surface based on perpendicular axis
                                (cond
                                    ;; Surface perpendicular to X-axis (YZ plane)
                                    ((equal axis "X")
                                        (setq pt1 (list (car center-pt) (cadr minpt) (caddr minpt))
                                              pt2 (list (car center-pt) (cadr maxpt) (caddr minpt))
                                              pt3 (list (car center-pt) (cadr maxpt) (caddr maxpt))
                                              pt4 (list (car center-pt) (cadr minpt) (caddr maxpt)))
                                    )
                                    ;; Surface perpendicular to Y-axis (XZ plane)
                                    ((equal axis "Y")
                                        (setq pt1 (list (car minpt) (cadr center-pt) (caddr minpt))
                                              pt2 (list (car maxpt) (cadr center-pt) (caddr minpt))
                                              pt3 (list (car maxpt) (cadr center-pt) (caddr maxpt))
                                              pt4 (list (car minpt) (cadr center-pt) (caddr maxpt)))
                                    )
                                    ;; Surface perpendicular to Z-axis (XY plane)
                                    ((equal axis "Z")
                                        (setq pt1 (list (car minpt) (cadr minpt) (caddr center-pt))
                                              pt2 (list (car maxpt) (cadr minpt) (caddr center-pt))
                                              pt3 (list (car maxpt) (cadr maxpt) (caddr center-pt))
                                              pt4 (list (car minpt) (cadr maxpt) (caddr center-pt)))
                                    )
                                )
                                
                                ;; Create 3D face using entmake
                                (entmake (list 
                                    '(0 . "3DFACE")
                                    '(8 . "BBOX_SURFACES")
                                    '(62 . 1) ; Red color
                                    (cons 10 pt1)
                                    (cons 11 pt2)
                                    (cons 12 pt3)
                                    (cons 13 pt4)
                                ))
                                
                                (setq created (1+ created))
                                (princ (strcat "\nSurface " (itoa created) 
                                             " created - W:" (rtos width 2 3)
                                             " H:" (rtos height 2 3)
                                             " D:" (rtos depth 2 3)
                                             " Center:" (vl-princ-to-string center-pt)))
                            )
                        )
                    )
                    (princ (strcat "\nObject " (itoa (1+ i)) " - Error: Could not get bounding box."))
                )
                (setq i (1+ i))
            )
            (princ "\n")
            (princ "\n--- RESULTS ---")
            (princ (strcat "\nTotal objects processed: " (itoa count)))
            (princ (strcat "\n3D surfaces created: " (itoa created)))
            (if (> created 0)
                (princ (strcat "\nSurfaces created on layer: BBOX_SURFACES (Red color)"))
                (princ "\nNo surfaces created - no objects matched the criteria.")
            )
            created
        )
        (progn
            (princ "\nNo objects found in the drawing.")
            0
        )
    )
    (princ)
)
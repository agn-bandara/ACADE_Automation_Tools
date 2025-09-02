; Reuse the get-din-length function from before
(defun get-din-length ( ben / blkname minpt maxpt box delta )
  (if (and ben 
           (= (c:wd_is_it_pnl ben) "FP")
           (setq blkname (cdr (assoc 2 (entget ben)))))
    (progn
      (vla-getboundingbox (vlax-ename->vla-object ben) 'minpt 'maxpt)
      (setq box (list (vlax-safearray->list minpt) 
                     (vlax-safearray->list maxpt))
            delta (mapcar '- (cadr box) (car box)))
      (cond 
        ((wcmatch (strcase blkname) "HDIN*") (car delta))
        ((wcmatch (strcase blkname) "VDIN*") (cadr delta))
        (t nil)
      )
    )
  )
)

(defun c:ListDINAll ( / ss ix count ben blkname mfg cat wdblknam len ins_pt tbl_obj row_data)
  ; Initialize counter and row data
  (setq count 0
        row_data nil)
  
  ; Get all block references
  (setq ss (ssget "X" '((0 . "INSERT"))))
  
  (if ss
    (progn
      ; Loop through each entity
      (setq ix 0)
      (while (< ix (sslength ss))
        (setq ben (ssname ss ix))
        
        ; Check if it's a panel footprint and get block name
        (if (and (= (c:wd_is_it_pnl ben) "FP")
                 (setq blkname (cdr (assoc 2 (entget ben))))
                 (or (wcmatch (strcase blkname) "HDIN*")
                     (wcmatch (strcase blkname) "VDIN*")))
          (progn
            ; Increment counter
            (setq count (1+ count))
            
            ; Get attributes using wd_get_pnlval
            (setq mfg (c:wd_get_pnlval ben "MFG")
                  cat (c:wd_get_pnlval ben "CAT")
                  wdblknam (c:wd_get_pnlval ben "WDBLKNAM")
                  len (get-din-length ben))
            
            ; Add row data to list
            (setq row_data (append row_data 
              (list (list (rtos count 2 0)      ; No.
                         blkname                 ; Type
                         (if mfg mfg "")         ; MFG
                         (if cat cat "")         ; CAT
                         (rtos len 2 2)          ; Length
                         (if wdblknam wdblknam "")  ; WDBLKNAM
                   ))))
          )
        )
        (setq ix (1+ ix))
      )
      
      ; Create table if we found any DIN rails
      (if (> count 0)
        (progn
          ; Get insertion point from user
          (setq ins_pt (getpoint "\nSpecify table insertion point: "))
          
          ; Create table object
          (setq tbl_obj (vla-addtable 
                         (vla-get-modelspace 
                           (vla-get-activedocument (vlax-get-acad-object)))
                         (vlax-3d-point ins_pt)
                         (+ count 2)  ; Add 2 for title and header rows
                         6            ; columns
                         0.7          ; row height
                         8.0))        ; column width
          
          ; Set title
          (vla-settext tbl_obj 0 0 "DIN RAIL LIST")
       
          ; Set headers
          (setq headers '("No." "TYPE" "MFG" "CAT" "LENGTH" "WDBLKNAM"))
          (setq ix 0)
          (foreach header headers 
                (vla-settext tbl_obj 1 ix header)
                (setq ix (1+ ix))
            )
          ; Fill data rows
          (setq row_num 2)  ; Start after title and header rows
          (foreach row row_data
            (setq col_num 0)  ; Reset column counter for each row
            (foreach cell row
              (vla-settext tbl_obj row_num col_num cell)
              (setq col_num (1+ col_num))
            )
            (setq row_num (1+ row_num))
          )
          
          ; Format table
          (vla-put-layer tbl_obj "0")
          
          ; Set row heights
          (vla-setrowheight tbl_obj 0 1.2)  ; Title row
          (vla-setrowheight tbl_obj 1 1.0)  ; Header row
          
          ; Set cell margins
          (vla-put-vertcellmargin tbl_obj 1)
          (vla-put-horzcellmargin tbl_obj 5)
          
          ; Set column widths
          (vla-setcolumnwidth tbl_obj 0 30)   ; No.
          (vla-setcolumnwidth tbl_obj 1 60)   ; Type
          (vla-setcolumnwidth tbl_obj 2 50)   ; MFG
          (vla-setcolumnwidth tbl_obj 3 50)   ; CAT
          (vla-setcolumnwidth tbl_obj 4 60)   ; Length
          (vla-setcolumnwidth tbl_obj 5 60)   ; WDBLKNAM
          
          ; Set text styles
          (vla-settextheight tbl_obj acTitleRow 7)  ; Title text height
          (vla-settextheight tbl_obj acHeaderRow 5)  ; Header text height
          (vla-settextheight tbl_obj acDataRow 5)  ; Data text height
          
          (vla-SetTextStyle tbl_obj acTitleRow "Standard")
          (vla-SetTextStyle tbl_obj acHeaderRow "Standard")
          (vla-SetTextStyle tbl_obj acDataRow "Standard")
          
                    ; Format title row
          (vla-setcellalignment tbl_obj 0 0 acMiddleCenter)
          (vla-setrowheight tbl_obj 0 20) ;row height
           ;Set alignments for each column's cells
          ; Set alignments for each column's cells
          (setq row_count (+ count 2)) ; Total rows including title and header
          (setq row 1)

           ; For all rows (including header row)
           (while (< row row_count)
             (progn
               (vla-setcellalignment tbl_obj row 0 acMiddleCenter)  ; No.
               (vla-setcellalignment tbl_obj row 1 acMiddleLeft)    ; Type
               (vla-setcellalignment tbl_obj row 2 acMiddleLeft)    ; MFG
               (vla-setcellalignment tbl_obj row 3 acMiddleLeft)    ; CAT
               (vla-setcellalignment tbl_obj row 4 acMiddleRight)   ; Length
               (vla-setcellalignment tbl_obj row 5 acMiddleLeft)    ; WDBLKNAM
               (vla-setrowheight tbl_obj row 10) ;row height
               (setq row (1+ row))
             )
           )
          (vla-setrowheight tbl_obj 1 20) ;row height
          ; Report completion
          (prompt (strcat "\nCreated table with " (rtos count 2 0) " DIN rails."))
        )
        (prompt "\nNo DIN rails found in drawing.")
      )
    )
  )
  (princ)
)
;*******************************************  Drawing Wide  *******************************************************************
(defun c:ListDINAdd ( / ss ix count ben mfg cat len key data_table ins_pt tbl_obj headers row_num col_num)
  ; Initialize counter and data table
  (setq count 0
        data_table nil) ; Alist to store MFG/CAT combinations with total lengths
  
  ; Get all block references
  (setq ss (ssget "X" '((0 . "INSERT"))))
  
  (if ss
    (progn
      ; Loop through each entity
      (setq ix 0)
      (while (< ix (sslength ss))
        (setq ben (ssname ss ix))
        
        ; Check if it's a panel footprint and get block name
        (if (and (= (c:wd_is_it_pnl ben) "FP")
                 (or (wcmatch (strcase (cdr (assoc 2 (entget ben)))) "HDIN*")
                     (wcmatch (strcase (cdr (assoc 2 (entget ben)))) "VDIN*")))
          (progn
            ; Increment counter
            (setq count (1+ count))
            
            ; Get attributes using wd_get_pnlval
            (setq mfg (c:wd_get_pnlval ben "MFG")
                  cat (c:wd_get_pnlval ben "CAT")
                  len (get-din-length ben))
            
            ; Create key for MFG/CAT combination
            (setq key (strcat (if mfg mfg "") "," (if cat cat "")))
            
            ; Add length to the data table
            (if (assoc key data_table)
              (setq data_table
                    (subst (cons key (+ len (cdr (assoc key data_table))))
                           (assoc key data_table)
                           data_table))
              (setq data_table (cons (cons key len) data_table)))
          )
        )
        (setq ix (1+ ix))
      )
      
      ; Create table if we found any DIN rails
      (if (> count 0)
        (progn
          ; Get insertion point from user
          (setq ins_pt (getpoint "\nSpecify table insertion point: "))
          
          ; Create table object
          (setq tbl_obj (vla-addtable
                         (vla-get-modelspace
                           (vla-get-activedocument (vlax-get-acad-object)))
                         (vlax-3d-point ins_pt)
                         (+ (length data_table) 2) ; Add 2 for title and header rows
                         4                      ; Columns
                         0.7                    ; Row height
                         8.0))                  ; Column width
          
          ; Set title
          (vla-settext tbl_obj 0 0 "DIN RAIL SUMMARY")
          
          ; Set headers
          (setq headers '("No." "MFG" "CAT" "TOTAL LENGTH"))
          (setq col_num 0)
          (foreach header headers
            (vla-settext tbl_obj 1 col_num header)
            (setq col_num (1+ col_num)))
          
          ; Fill data rows
          (setq row_num 2)  ; Start after title and header rows
          (foreach pair data_table
            (setq col_num 0)
            (vla-settext tbl_obj row_num col_num (rtos (1+ (- row_num 2)) 2 0)) ; No.
            (setq col_num (1+ col_num))
            (setq mfg_cat (str->lst (car pair) ","))
            (vla-settext tbl_obj row_num col_num (car mfg_cat)) ; MFG
            (setq col_num (1+ col_num))
            (vla-settext tbl_obj row_num col_num (cadr mfg_cat)) ; CAT
            (setq col_num (1+ col_num))
            (vla-settext tbl_obj row_num col_num (rtos (cdr pair) 2 2)) ; Total Length
            (setq row_num (1+ row_num)))
          
          ; Format table
          (vla-put-layer tbl_obj "0")
          (vla-setrowheight tbl_obj 0 20)  ; Title row
          (vla-setrowheight tbl_obj 1 20)  ; Header row
          
          ; Set column widths
          (vla-setcolumnwidth tbl_obj 0 30) ; No.
          (vla-setcolumnwidth tbl_obj 1 50) ; MFG
          (vla-setcolumnwidth tbl_obj 2 50) ; CAT
          (vla-setcolumnwidth tbl_obj 3 70) ; Total Length
          
          ; Set text styles
          (vla-settextheight tbl_obj acTitleRow 7)  ; Title text height
          (vla-settextheight tbl_obj acHeaderRow 5)  ; Header text height
          (vla-settextheight tbl_obj acDataRow 5)  ; Data text height
          
          (vla-SetTextStyle tbl_obj acTitleRow "Standard")
          (vla-SetTextStyle tbl_obj acHeaderRow "Standard")
          (vla-SetTextStyle tbl_obj acDataRow "Standard")
          
          ; Set cell margins
          (vla-put-vertcellmargin tbl_obj 1)
          (vla-put-horzcellmargin tbl_obj 5)
          
          ; Format title row
          (vla-setcellalignment tbl_obj 0 0 acMiddleCenter)
          (vla-setrowheight tbl_obj 0 20) ;row height
           ;Set alignments for each column's cells
          ; Set alignments for each column's cells
          (setq row_count (+ count 2)) ; Total rows including title and header
          (setq row 1)

           ; For all rows (including header row)
           (while (< row row_count)
             (progn
               (vla-setcellalignment tbl_obj row 0 acMiddleCenter)  ; No.
               (vla-setcellalignment tbl_obj row 1 acMiddleLeft)    ; MFG
               (vla-setcellalignment tbl_obj row 2 acMiddleLeft)    ; CAT
               (vla-setcellalignment tbl_obj row 3 acMiddleRight)   ; Length
               (vla-setrowheight tbl_obj row 10) ;row height
               (setq row (1+ row))
             )
           )
          (vla-setrowheight tbl_obj 1 20) ;row height
          ; Report completion
          (prompt (strcat "\nCreated summary table with " (rtos (length data_table) 2 0) " unique MFG/CAT combinations."))
        )
        (prompt "\nNo DIN rails found in drawing.")
      )
    )
  )
  (princ)
)

; Helper function to split string into list
(defun str->lst (str delim / pos lst)
  (while (setq pos (vl-string-search delim str))
    (setq lst (cons (substr str 1 pos) lst)
          str (substr str (+ pos 2))))
  (reverse (cons str lst)))

;*********************************************************  Project Wide  ************************************************************************
; Enhanced DIN Rail Data Collection with Current Drawing Addition (V1 Approach)

; Helper function to get project name from current project
(defun get-project-name ( / proj_path proj_name)
  (if (setq proj_path (ace_getactiveproject))
    (progn
      (setq proj_name (cadr (c:wd_split_fnam proj_path))) ; Get filename without extension
      (if proj_name proj_name "UnknownProject")
    )
    "UnknownProject"
  )
)

; Helper function to get file paths with project name
(defun get-project-file-paths ( / temp_dir proj_name)
  (setq temp_dir (getenv "TEMP")
        proj_name (get-project-name))
  (list 
    (vl-string-translate "\\" "/" (strcat temp_dir "/" proj_name "_dinrail_data.csv"))
    (vl-string-translate "\\" "/" (strcat temp_dir "/" proj_name "_dinrail_summary.csv"))
    (vl-string-translate "\\" "/" (strcat temp_dir "/" proj_name "_dinrail_progress.csv"))
    (vl-string-translate "\\" "/" (strcat temp_dir "/" proj_name "_dinrail_script.scr"))
  )
)

; Function to update progress file
(defun update-progress-file (progress_fname total completed status / f)
  (if (setq f (open progress_fname "w"))
    (progn
      (write-line "Total,Completed,Status" f)
      (write-line (strcat (itoa total) "," (itoa completed) "," status) f)
      (close f)
    )
  )
)

; Function to read progress file
(defun read-progress-file (progress_fname / f line data)
  (if (setq f (open progress_fname "r"))
    (progn
      (read-line f) ; Skip header
      (if (setq line (read-line f))
        (progn
          (setq data (c:wd_delim_str_to_lst line ","))
          (close f)
          (list (atoi (nth 0 data)) (atoi (nth 1 data)) (nth 2 data)) ; Total, Completed, Status
        )
        (progn
          (close f)
          nil
        )
      )
    )
    nil
  )
)

; Helper function to get DIN rail length
(defun get-din-length ( ben / blkname minpt maxpt box delta )
  (if (and ben 
           (= (c:wd_is_it_pnl ben) "FP")
           (setq blkname (cdr (assoc 2 (entget ben)))))
    (progn
      (vla-getboundingbox (vlax-ename->vla-object ben) 'minpt 'maxpt)
      (setq box (list (vlax-safearray->list minpt) 
                     (vlax-safearray->list maxpt))
            delta (mapcar '- (cadr box) (car box)))
      (cond 
        ((wcmatch (strcase blkname) "HDIN*") (car delta))
        ((wcmatch (strcase blkname) "VDIN*") (cadr delta))
        (t nil)
      )
    )
  )
)

; Data collection function that checks completion and triggers summary (V1 approach)
(defun c:CollectDINData ( csv_fname progress_fname / ss ix ben blkname mfg cat loc len dwg csv_f prog_data)
  (setq csv_fname (vl-string-translate "\\" "/" csv_fname))
  (setq progress_fname (vl-string-translate "\\" "/" progress_fname))
  
  ; Read current progress
  (setq prog_data (read-progress-file progress_fname))
  (if (not prog_data)
    (setq prog_data (list 0 0 "InProgress")) ; Default if file doesn't exist
  )
  
    ; Update progress - increment completed count
  (update-progress-file progress_fname 
                       (nth 0 prog_data) 
                       (+ (nth 1 prog_data) 1) 
                       "InProgress")
  
  ; Check if all drawings are completed
  (setq prog_data (read-progress-file progress_fname))
  (if (and prog_data 
           (= (nth 0 prog_data) (nth 1 prog_data))) ; Total = Completed
    (progn
      ; Mark as completed
      (update-progress-file progress_fname 
                           (nth 0 prog_data) 
                           (nth 1 prog_data) 
                           "Completed")
      
      ; Trigger summary creation as callback
      (c:CreateDINSummaryCallback)
    )
  )
  
  (setq csv_f (open csv_fname "a"))
  
  (setq ss (ssget "X" '((0 . "INSERT"))))
  (if ss
    (progn
      (setq ix 0)
      (while (< ix (sslength ss))
        (setq ben (ssname ss ix))
        (if (and (= (c:wd_is_it_pnl ben) "FP")
                 (or (wcmatch (strcase (cdr (assoc 2 (entget ben)))) "HDIN*")
                     (wcmatch (strcase (cdr (assoc 2 (entget ben)))) "VDIN*")))
          (progn
            (setq blkname (cdr (assoc 2 (entget ben)))
                  mfg (c:wd_get_pnlval ben "MFG")
                  cat (c:wd_get_pnlval ben "CAT")
                  loc (c:wd_get_pnlval ben "LOC")
                  len (get-din-length ben)
                  dwg (getvar "DWGNAME"))
            
            (write-line (strcat dwg "," blkname ","
                              (if mfg mfg "") ","
                              (if cat cat "") ","
                              (if loc loc "") ","
                              (rtos len 2 0))
                      csv_f)
          )
        )
        (setq ix (1+ ix))
      )
    )
  )
  (close csv_f)
  
  (princ)
)

; Cleanup function to delete temporary files, keep only summary
(defun cleanup-temp-files ( / file_paths csv_fname progress_fname script_fname)
  (setq file_paths (get-project-file-paths)
        csv_fname (nth 0 file_paths)
        progress_fname (nth 2 file_paths)
        script_fname (nth 3 file_paths))
  
  ; Delete temporary files (keep summary file)
  (if (findfile csv_fname)
    (progn
      (vl-file-delete csv_fname)
      (prompt "\nDeleted data file: ") (prompt csv_fname)
    )
  )
  
  (if (findfile progress_fname)
    (progn
      (vl-file-delete progress_fname)
      (prompt "\nDeleted progress file: ") (prompt progress_fname)
    )
  )
  
  (if (findfile script_fname)
    (progn
      (vl-file-delete script_fname)
      (prompt "\nDeleted script file: ") (prompt script_fname)
    )
  )
  
  (prompt "\nCleanup completed. Summary file retained.")
)

; Internal callback function for summary creation
(defun c:CreateDINSummaryCallback ( / file_paths csv_fname summary_fname)
  (setq file_paths (get-project-file-paths)
        csv_fname (nth 0 file_paths)
        summary_fname (nth 1 file_paths))
  
  (if (findfile csv_fname)
    (progn
      (prompt "\nProcessing collected data...")
      (create-summary-report csv_fname)
      (prompt "\nCompleted processing DIN rail data.")
      
      ; Cleanup temporary files after summary is created
      (cleanup-temp-files)
    )
    (prompt "\nData file not found.")
  )
  (princ)
)

; Function to ensure current drawing is last in the list
(defun ensure-current-drawing-last (drawing_list / current_dwg current_path found_index new_list)
  (setq current_dwg (getvar "DWGNAME")
        current_path (strcat (getvar "DWGPREFIX") current_dwg)
        current_path (vl-string-translate "\\" "/" current_path)
        found_index nil
        new_list nil)
  
  (setq new_list (append drawing_list (list current_path)))
  new_list
)

; Main command to create DIN summary
(defun c:CreateDINSummary ( / file_paths csv_fname summary_fname progress_fname script_fname dlst proj_dwgs f user_choice enhanced_drawing_list)
  (setq file_paths (get-project-file-paths)
        csv_fname (nth 0 file_paths)
        summary_fname (nth 1 file_paths)
        progress_fname (nth 2 file_paths)
        script_fname (nth 3 file_paths))
  
  ; Check if summary file already exists
  (if (findfile summary_fname)
    (progn
      (prompt (strcat "\nDIN Rail summary already exists: " summary_fname))
      (setq user_choice (show-yesno-dialog "DIN Rail summary already exists, Reload the report from beginning?" "DIN Rail summary"))
      (if user_choice
        (progn
          ; Delete existing files to start fresh
          (vl-file-delete csv_fname)
          (vl-file-delete summary_fname)
          (vl-file-delete progress_fname)
          (setq user_choice "Yes")
        )
        (progn
          ; Load existing summary
          (if (findfile summary_fname)
            (progn
              (c:wd_ins_table summary_fname)
              (prompt "\nLoaded existing DIN rail summary.")
              (princ)
              (exit) ; Exit function early
            )
            (prompt "\nSummary file not found, starting fresh.")
          )
        )
      )
    )
  )
  
  ; Continue with fresh data collection
  (prompt "\nStarting fresh DIN rail data collection...")
  
  ; Get current project's drawing list
  (setq proj_dwgs (nth 6 (c:wd_proj_wdp_data)))
  
  ; Only proceed if we have a project drawing list
  (if proj_dwgs
    (progn
      ; Show drawing selection dialog
      (setq dlst (c:wd_pdwgs proj_dwgs 
                            "Select drawings for DIN Rail Summary" 
                            "" 
                            nil))
      
      ; Only proceed if drawings were selected
      (if (and dlst 
               (car dlst)  ; First element contains drawing list
               (/= 0 (length (car dlst)))) ; List is not empty
        (progn
          (prompt (strcat "\nSelected " (itoa (length (car dlst))) " drawings"))
          
          ; Ensure current drawing is last in the list
          (setq enhanced_drawing_list (ensure-current-drawing-last (car dlst)))
          (if (= (length (car dlst)) (length enhanced_drawing_list))
            (prompt (strcat "\nProcessing " (itoa (length enhanced_drawing_list)) " drawings (current drawing moved to end)"))
            (prompt (strcat "\nProcessing " (itoa (length enhanced_drawing_list)) " drawings (including current drawing)"))
          )
          
          ; Initialize progress file with the actual count
          (update-progress-file progress_fname (length enhanced_drawing_list) 0 "InProgress")
          
          ; Create empty CSV file with headers
          (setq f (open csv_fname "w"))
          (write-line "Drawing,Type,MFG,CAT,LOC,Length" f)
          (close f)
          
          ; Create script to call our function
          (setq f (open script_fname "w"))
          (write-line (strcat "(c:CollectDINData \"" csv_fname "\" \"" progress_fname "\")") f)
          (close f)
          
          ; Run script on enhanced drawing list
          (prompt "\nRunning collection script...")
          (c:ace_projwide_script enhanced_drawing_list script_fname)
          (prompt "\nData collection started. Summary will be created automatically when complete.")
        )
        (prompt "\nNo drawings selected - operation canceled.")
      )
    )
    (prompt "\nNo active project found.")
  )
  (princ)
)

; Read CSV file into nested list
(defun read-csv (fname / f line data)
  (if (setq f (open fname "r"))
    (progn
      (read-line f) ; Skip header line
      (while (setq line (read-line f))
        (setq data (cons (c:wd_delim_str_to_lst line ",") data))
      )
      (close f)
      (reverse data)
    )
    (progn
      (prompt "\nError: Could not open file for reading")
      nil
    )
  )
)

; Get unique MFG/CAT combinations
(defun get-unique-mfg-cat (data / mfg-cat-list)
  (setq mfg-cat-list nil)
  (foreach row data
    (setq key (strcat (nth 2 row) "," (nth 3 row))) ; MFG,CAT
    (if (not (member key mfg-cat-list))
      (setq mfg-cat-list (cons key mfg-cat-list))
    )
  )
  mfg-cat-list
)

; Get unique LOC values - modified to separate blank LOCs
(defun get-unique-locs (data / loc-list)
  (setq loc-list nil)
  (foreach row data
    (if (and (nth 4 row) 
             (/= (nth 4 row) "")  ; Only include non-blank LOCs
             (not (member (nth 4 row) loc-list)))
      (setq loc-list (cons (nth 4 row) loc-list))
    )
  )
  ; Add "BLANK" at the end of the list
  (setq loc-list (append (vl-sort loc-list '<) (list "BLANK")))
)

; Create summary table from data
(defun make-summary-table (data mfg-cat-list loc-list / table)
  (setq table nil)
  (foreach mfg-cat mfg-cat-list
    (setq row (list mfg-cat)) ; Start with MFG,CAT key
    
    ; Add total for each LOC
    (foreach loc loc-list
      (setq total 0.0)
      (foreach d data
        (if (and (= (strcat (nth 2 d) "," (nth 3 d)) mfg-cat)
                 (if (= loc "BLANK")
                     (or (null (nth 4 d)) 
                         (= (nth 4 d) ""))
                     (= (nth 4 d) loc)
                 )
            )
          (setq total (+ total (atof (nth 5 d))))
        )
      )
      (setq row (append row (list total)))
    )
    
    ; Add row to table
    (setq table (cons row table))
  )
  (reverse table)
)

; Process CSV and create summary report
(defun create-summary-report (csv_fname / csv_data unique_mfg_cat unique_locs data_table file_paths summary_fname)
  (prompt (strcat "\nReading data from: " csv_fname))
  
  ; Process data and create summary
  (if (setq csv_data (read-csv csv_fname))
    (progn
      (prompt (strcat "\nFound " (itoa (length csv_data)) " records"))
      (setq unique_mfg_cat (get-unique-mfg-cat csv_data)
            unique_locs (get-unique-locs csv_data)
            data_table (make-summary-table csv_data unique_mfg_cat unique_locs))
      
      ; Get summary file path
      (setq file_paths (get-project-file-paths)
            summary_fname (nth 1 file_paths))
      
      ; Display table
      (display-summary-table data_table unique_locs summary_fname)
    )
    (prompt "\nNo data found to process")
  )
)

; Display summary table and save to file
(defun display-summary-table (data loc-list fname / f line total)
  (setq f (open fname "w"))
  
  ; Write headers
  (write-line (strcat "MFG,CAT," 
                      (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) loc-list)) 
                      "TOTAL") f)
  
  ; Write data rows
  (foreach row data
    (setq line (car row)) ; MFG,CAT
    (foreach val (cdr row)
      (setq line (strcat line "," (format-number val)))
    )
    ; Add total for row
    (setq total (apply '+ (cdr row)))
    (write-line (strcat line "," (format-number total)) f)
  )
  (close f)
  
  ; Display table dialog
  (c:wd_ins_table fname)
)

; Helper function to format numbers (blank for zero)
(defun format-number (num / )
  (if (equal num 0.0)
    ""
    (rtos num 2 2)
  )
)


;########### Enhanced Item Summary QA with Progress Monitoring ####################################################################################

;; Function to get project name from active project
(defun qa_get_project_name (/ proj_path proj_name)
  (setq proj_path (ace_getactiveproject))
  (if proj_path
    (progn
      (setq proj_name (cadr (c:wd_split_fnam proj_path)))
      (if proj_name proj_name "UnknownProject")
    )
    "UnknownProject"
  )
)

;; Function to get project name from active project
(defun c:LoadActivePorjectName (/ proj_path proj_name)
  (setq proj_path (ace_getactiveproject))
  (if proj_path
    (progn
      (setq proj_name (cadr (c:wd_split_fnam proj_path)))
      (if proj_name proj_name "UnknownProject")
    )
  )
  (setvar "USERS5" proj_name)
)

;; Function to format numbers for QA Item display
(defun qa_format_number (num)
  (if (numberp num)
    (rtos num 2 0)
    (if (or (= num "") (= num nil))
      "0"
      num
    )
  )
)

;; Function to read CSV file to a list of lists
(defun qa_read_csv (file-name / file line data result)
  (if (findfile file-name)
    (progn
      (setq result '())
      (setq file (open file-name "r"))
      ;; Skip header
      (read-line file)
      (while (setq line (read-line file))
        (setq data (qa_parse_csv_line line))
        (setq result (append result (list data)))
      )
      
      (close file)
      result
    )
    nil
  )
)

;; Function to parse a CSV line into a list
(defun qa_parse_csv_line (line / result char buffer in-quotes)
  (setq result '()
        buffer ""
        in-quotes nil)
  
  (foreach char (vl-string->list line)
    (cond
      ;; Handle double quotes
      ((= char 34) ; ASCII for "
       (setq in-quotes (not in-quotes))
      )
      
      ;; Handle commas
      ((and (= char 44) (not in-quotes)) ; ASCII for , and not in quotes
       (setq result (append result (list buffer))
             buffer "")
      )
      
      ;; Handle regular characters
      (T
       (setq buffer (strcat buffer (chr char)))
      )
    )
  )
  
  ;; Add the last field
  (setq result (append result (list buffer)))
  
  result
)

;; Function to read progress file
(defun qa_read_progress (progress_fname / file line fields)
  (if (findfile progress_fname)
    (progn
      (setq file (open progress_fname "r"))
      (read-line file) ; skip header
      (if (setq line (read-line file))
        (progn
          (setq fields (c:wd_delim_str_to_lst line ","))
          (close file)
          (list (atoi (nth 0 fields))    ; Total
                (atoi (nth 1 fields))    ; Completed
                (nth 2 fields))          ; Status
        )
        (progn
          (close file)
          nil
        )
      )
    )
    nil
  )
)

;; Function to update progress file
(defun qa_update_progress (progress_fname total completed status / f)
  (setq f (open progress_fname "w"))
  (write-line "Total,Completed,Status" f)
  (write-line (strcat (itoa total) "," (itoa completed) "," status) f)
  (close f)
)

;; Function to run summary calculation
(defun qa_run_summary_calculation (project_name / temp_dir csv_fname final_csv_fname file line csv_data combined_data p_item mfg cat qty item_key keys f)
  (setq temp_dir (getenv "TEMP")
        csv_fname (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-Itemlist-raw.csv"))
        final_csv_fname (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-Itemlist.csv")))

  (prompt (strcat "\nProcessing data from: " csv_fname))

  (if (findfile csv_fname)
    (progn
      ;; Read and parse raw CSV
      (setq csv_data '())
      (setq file (open csv_fname "r"))
      (read-line file)  ; skip header
      (while (setq line (read-line file))
        (setq fields (c:wd_delim_str_to_lst line ","))
        (setq csv_data (append csv_data (list fields))))
      (close file)

      (if csv_data
        (progn
          (prompt (strcat "\nFound " (itoa (length csv_data)) " records"))

          ;; Create an association list to track quantities
          (setq combined_data '())
          
          ;; Process each row
          (foreach row csv_data
            (setq p_item (nth 1 row)
                  mfg (nth 2 row)
                  cat (nth 3 row)
                  mfg (if mfg mfg "")
                  cat (if cat cat "")
                  item_key (strcat p_item "," mfg "," cat))
            
            ;; Check if we've seen this item before
            (setq keys (assoc item_key combined_data))
            
            (if keys
                ;; Increment the quantity
                (setq combined_data 
                      (subst 
                        (list item_key p_item mfg cat (1+ (nth 4 keys)))
                        keys
                        combined_data))
                ;; Add the new item with quantity 1
                (setq combined_data 
                      (cons (list item_key p_item mfg cat 1) combined_data))))
          
          (prompt (strcat "\nFound " (itoa (length combined_data)) " unique items"))

          ;; Sort combined data by P_ITEM number
          (setq combined_data
                (vl-sort combined_data
                         (function
                           (lambda (a b)
                             (< (atoi (nth 1 a))
                                (atoi (nth 1 b)))))))
          
          (prompt "\nItems sorted numerically by P_ITEM")

          ;; Write out final CSV with QTY column
          (vl-file-delete final_csv_fname)
          (setq f (open final_csv_fname "w"))
          (write-line "P_ITEM,MFG,CAT,QTY" f)
          (foreach item combined_data
            (write-line (strcat (nth 1 item) ","
                                (nth 2 item) ","
                                (nth 3 item) ","
                                (itoa (nth 4 item)))
                        f))
          (close f)

          ;; Display in AutoCAD
          (prompt "\nDisplaying QA Item table with quantities...")
          (c:wd_ins_table final_csv_fname))
        (prompt "\nNo data found in the CSV file")))
    (prompt "\nData file not found."))
  
  (c:QA-CleanupTempFiles)
)

;; Function to collect P_ITEM, MFG, and CAT data from one drawing with progress monitoring
(defun c:QA_CollectItemData (csv_fname progress_fname project_name / ss i ent p_item mfg cat j item0j mfg0j cat0j progress_data total completed status)
  ;; Make sure AutoCAD Electrical is loaded
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'c:wd_get_pnlval '(nil nil))))
    (load "wd_load.lsp")
  )
  
  ;; Get current drawing name
  (setq drawing_name (getvar "DWGNAME"))
  
  ;; Read current progress before processing
  (setq progress_data (qa_read_progress progress_fname))
  (if progress_data
    (setq total (nth 0 progress_data)
          completed (nth 1 progress_data)
          status (nth 2 progress_data))
    (setq total 0 completed 0 status "in_progress")
  )
  
    ;; Update progress - increment completed count
  (setq completed (1+ completed))
  (qa_update_progress progress_fname total completed "in_progress")
  
  ;; Read progress again to check if processing is complete
  (setq progress_data (qa_read_progress progress_fname))
  (if progress_data
    (progn
      (setq total (nth 0 progress_data)
            completed (nth 1 progress_data))
      
      ;; Check if all drawings are processed
      (if (>= completed total)
        (progn
          ;; Update status to completed
          (qa_update_progress progress_fname total completed "completed")
          (prompt "\nAll drawings processed. Running summary calculation...")
          
          ;; Run summary calculation
          (qa_run_summary_calculation project_name)
        )
      )
    )
  )
  
  ;; Get all block references in the drawing
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  
  ;; Process each block reference
  (if ss
    (progn
      (setq f (open csv_fname "a"))
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        
        ;; Get P_ITEM value
        (setq p_item (c:wd_get_pnlval ent "P_ITEM"))
        
        ;; If P_ITEM value exists, get MFG and CAT
        (if (and p_item (/= p_item ""))
          (progn
            (setq mfg (c:wd_get_pnlval ent "MFG"))
            (setq cat (c:wd_get_pnlval ent "CAT"))
            
            ;; Write data to CSV file
            (write-line (strcat 
                          drawing_name "," 
                          p_item "," 
                          (if mfg mfg "") "," 
                          (if cat cat ""))
                        f)
          )
        )
        ;; Check for ITEM01 to ITEM04 values
        (setq j 1)
        (while (<= j 4)
          (setq item0j (c:wd_get_pnlval ent (strcat "ITEM0" (itoa j))))
          ;; If ITEM0j value exists, add to file
         (if (and item0j (/= item0j ""))
            (progn
              (setq mfg0j (c:wd_get_pnlval ent (strcat "MFG0" (itoa j))))
              (setq cat0j (c:wd_get_pnlval ent (strcat "CAT0" (itoa j))))
              ;; Write data to CSV file
              (write-line (strcat 
                            drawing_name "," 
                            item0j "," 
                            (if mfg0j mfg0j "") "," 
                            (if cat0j cat0j ""))
                          f)
            )
          )
          (setq j (1+ j))
        )
        
        (setq i (1+ i))
      )
      
      ;; Close file
      (close f)
    )
  )
  
  (princ)
)

;; Main function to collect data from multiple drawings with progress monitoring
(defun c:QA-ItemListProject (/ dlst temp_dir csv_fname final_csv_fname progress_fname scr_fname f project_name user_choice)
  ;; Get project name
  (setq project_name (qa_get_project_name))
  ;; Get temp directory path and create file paths 
  (setq temp_dir (getenv "TEMP")
        csv_fname (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-Itemlist-raw.csv"))
        final_csv_fname (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-Itemlist.csv"))
        progress_fname (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-Itemlist_progress.csv")))
  
  (prompt (strcat "\nProject: " project_name))
  (prompt (strcat "\nTemporary data file: " csv_fname))
  
  ;; Check if final report already exists
  (if (findfile final_csv_fname)
    (progn
      (prompt (strcat "\nExisting report found: " final_csv_fname))
      (setq user_choice (show-yesno-dialog "Existing report found, Reload the report from beginning?" "QA Item List"))
      
      (if user_choice
        (progn
          ;; Delete existing files to start fresh
          (vl-file-delete csv_fname)
          (vl-file-delete final_csv_fname)
          (vl-file-delete progress_fname)
          (prompt "\nStarting fresh report generation...")
        )
        (progn
          ;; Load existing report
          (prompt "\nLoading existing report...")
          (c:wd_ins_table final_csv_fname)
          (princ)
          (exit)
        )
      )
    )
  )
  
  ;; Get current project's drawing list
  (setq proj_dwgs (nth 6 (c:wd_proj_wdp_data)))
  
  ;; Only proceed if we have a project drawing list
  (if proj_dwgs
    (progn
      ;; Show drawing selection dialog
      (setq dlst (c:wd_pdwgs proj_dwgs 
                            "Select drawings for Item List Summary" 
                            "" 
                            nil))
      
      ;; Only proceed if drawings were selected
      (if (and dlst 
               (car dlst)  
               (/= 0 (length (car dlst))))
        (progn
          (prompt (strcat "\nSelected " (itoa (length (car dlst))) " drawings"))
          
          ;; Add current drawing to the end of the list (to handle projwide script behavior)
          (setq current_dwg (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
          (setq drawing_list (append (car dlst) (list current_dwg)))
          
          ;; Initialize progress file
          (qa_update_progress progress_fname (length drawing_list) 0 "in_progress")
          
          ;; Create empty CSV file with headers
          (setq f (open csv_fname "w"))
          (write-line "Drawing,P_ITEM,MFG,CAT" f)
          (close f)
          
          ;; Create script to call our function with progress monitoring
          (setq scr_fname (strcat temp_dir "/item_script.scr"))
          (setq scr_fname (vl-string-translate "\\" "/" scr_fname))
          
          (setq f (open scr_fname "w"))
          (write-line (strcat "(c:QA_CollectItemData \"" csv_fname "\" \"" progress_fname "\" \"" project_name "\")") f)
          (close f)
          
          ;; Run script on selected drawings
          (prompt "\nRunning collection script on selected drawings...")
          (prompt "\nProgress will be monitored automatically...")
          (c:ace_projwide_script drawing_list scr_fname)
          
          (prompt "\nData collection has been initiated.")
          (prompt "\nThe summary report will be generated automatically when all drawings are processed.")
        )
        (prompt "\nNo drawings selected - operation canceled.")
      )
    )
    (prompt "\nNo active project found.")
  )
  (princ)
)

;; Function to cleanup temporary files but keep the final report
(defun c:QA-CleanupTempFiles (/ project_name temp_dir csv_fname progress_fname scr_fname)
  ;; Get project name
  (setq project_name (qa_get_project_name))
  
  ;; Get temp directory path and create file paths 
  (setq temp_dir (getenv "TEMP")
        csv_fname (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-Itemlist-raw.csv"))
        progress_fname (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-Itemlist_progress.csv"))
        scr_fname (vl-string-translate "\\" "/" (strcat temp_dir "/item_script.scr")))
  
  (prompt (strcat "\nCleaning up temporary files for project: " project_name))
  
  ;; Delete temporary files
  (if (findfile csv_fname)
    (progn
      (vl-file-delete csv_fname)
      (prompt (strcat "\nDeleted: " csv_fname))
    )
    (prompt (strcat "\nFile not found: " csv_fname))
  )
  
  (if (findfile progress_fname)
    (progn
      (vl-file-delete progress_fname)
      (prompt (strcat "\nDeleted: " progress_fname))
    )
    (prompt (strcat "\nFile not found: " progress_fname))
  )
  
  (if (findfile scr_fname)
    (progn
      (vl-file-delete scr_fname)
      (prompt (strcat "\nDeleted: " scr_fname))
    )
    (prompt (strcat "\nFile not found: " scr_fname))
  )
  
  ;; Check if final report still exists
  (setq final_csv_fname (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-Itemlist.csv")))
  (if (findfile final_csv_fname)
    (prompt (strcat "\nFinal report preserved: " final_csv_fname))
    (prompt "\nWarning: Final report not found!")
  )
  
  (prompt "\nCleanup completed.")
  (princ)
)

;; Function for single drawing item list with summary
(defun c:QA-ItemList (/ ss i j ent p_item mfg cat temp_dir csv_file f item_data unique_data item0j mfg0j cat0j project_name)
  ;; Make sure AutoCAD Electrical is loaded
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'c:wd_get_pnlval '(nil nil))))
    (load "wd_load.lsp")
  )
  
  ;; Get project name
  (setq project_name (qa_get_project_name))
  
  ;; Get all block references in the drawing
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  
  ;; Create empty result list
  (setq unique_data '())
  
  ;; Get current drawing name
  (setq drawing_name (getvar "DWGNAME"))
  
  ;; Get temp directory path and create file paths 
  (setq temp_dir (getenv "TEMP")
        csv_file (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-Itemlist-single.csv")))
  
  ;; Process each block reference
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        
        ;; Get P_ITEM value
        (setq p_item (c:wd_get_pnlval ent "P_ITEM"))
        
        ;; If P_ITEM value exists, get MFG and CAT
        (if (and p_item (/= p_item ""))
          (progn
            (setq mfg (c:wd_get_pnlval ent "MFG"))
            (setq cat (c:wd_get_pnlval ent "CAT"))
            
            ;; Create data item for unique list
            (setq item_data (list p_item (if mfg mfg "") (if cat cat "")))
            
            ;; Add to result list if not already there
            (if (not (member item_data unique_data))
              (setq unique_data (cons item_data unique_data))
            )
          )
        )
        ;; Check for ITEM01 to ITEM04 values
        (setq j 1)
        (while (<= j 4)
          (setq item0j (c:wd_get_pnlval ent (strcat "ITEM0" (itoa j))))
          ;; If ITEM0j value exists, add to unique list
         (if (and item0j (/= item0j ""))
            (progn
              (setq mfg0j (c:wd_get_pnlval ent (strcat "MFG0" (itoa j))))
             (setq cat0j (c:wd_get_pnlval ent (strcat "CAT0" (itoa j))))
              (setq item_data (list item0j (if mfg0j mfg0j "") (if cat0j cat0j "")))
              (if (not (member item_data unique_data))
          (setq unique_data (cons item_data unique_data))
              )
            )
          )
          (setq j (1+ j))
        )
        
        (setq i (1+ i))
      )
      
      ;; Create output CSV file
      (vl-file-delete csv_file)
      (setq f (open csv_file "w"))
      
      ;; Write header
      (write-line "P_ITEM,MFG,CAT" f)
      
      ;; Write data
      (foreach item unique_data
        (write-line (strcat 
                      (nth 0 item) "," 
                      (nth 1 item) "," 
                      (nth 2 item))
                    f)
      )
      
      ;; Close file
      (close f)
      
      ;; Report results
      (prompt (strcat "\nData collection completed. Found " (itoa (length unique_data)) " unique items."))
      
      ;; Display items table
      (c:wd_ins_table csv_file)
    )
    (prompt "\nNo block references found in the drawing.")
  )
  
  (princ)
)

;########### QA Balloon Diff with Proven Logic from list-pnl-diff ####################################################################################

;; Helper function to remove trailing comma from strings (already available)
(defun remove_trailing_comma (str)
  (if (or (null str) (= str ""))
    ""
    (if (wcmatch str "*,*")
      (substr str 1 (- (strlen str) 1))
      str
    )
  )
)

;; Function to collect balloon difference data from one drawing using proven logic
(defun c:QA_CollectBalloonDiffData (csv_fname progress_fname project_name / ss slen ix ben blknam b_item_val hdl_ptr p_item_val p_tag_val target_en 
                          ballptr_val ballptr01_val ballptr02_val ballptr03_val ballptr04_val
                          clean_hdl target_p_item_attr pnl_hdl diff_count progress_data total completed status drawing_name f)
  ;; Make sure AutoCAD Electrical is loaded
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'c:wd_get_pnlval '(nil nil))))
    (load "wd_load.lsp")
  )
  
  ;; Get current drawing name
  (setq drawing_name (getvar "DWGNAME"))
  
  ;; Read current progress before processing
  (setq progress_data (qa_read_progress progress_fname))
  (if progress_data
    (setq total (nth 0 progress_data)
          completed (nth 1 progress_data)
          status (nth 2 progress_data))
    (setq total 0 completed 0 status "in_progress")
  )
  
  ;; Get all INSERT entities (blocks) in the drawing that start with "PNL" - USING EXACT PROVEN LOGIC
  (setq ss (ssget "_X" '((-4 . "<AND")(0 . "INSERT")(2 . "PNL*")(-4 . "AND>"))))
  (setq diff_count 0)
  
  ;; Process each block reference - USING EXACT PROVEN LOGIC
  (if ss
    (progn
      (setq f (open csv_fname "a"))
      (setq slen (sslength ss))
      (setq ix 0)
      
      ;; Process each block - EXACT LOGIC FROM list-pnl-diff
      (while (< ix slen)
        (setq ben (ssname ss ix))  ; Get entity name of current block
        
        ;; Get block name
        (setq blknam (cdr (assoc 2 (entget ben))))
        
        ;; Get B_ITEM attribute value using wd_get_pnlval - EXACT PROVEN LOGIC
        ;; This function looks for ATTRIB first, if not found, looks for XDATA value
        (setq b_item_val (c:wd_get_pnlval ben "B_ITEM"))
        
        ;; If not found, set to empty string - EXACT PROVEN LOGIC
        (if (not b_item_val)
          (setq b_item_val "")
        )
        
        ;; First, get the handle pointer and PNL handle from the PNL block - EXACT PROVEN LOGIC
        (setq hdl_ptr (c:wd_get_pnlval ben "BALLPTR"))
        (setq pnl_hdl (c:wd_get_pnlval ben "HDL"))
        
        ;; Process the handle pointer to get the referenced block - EXACT PROVEN LOGIC
        (setq p_item_val "")
        (setq target_en nil)
        
        (if (and hdl_ptr (/= hdl_ptr ""))
          (progn
            ;; Remove comma from handle string if present using helper function - EXACT PROVEN LOGIC
            (setq hdl_ptr (remove_trailing_comma hdl_ptr))
            
            ;; Try to get entity from handle - EXACT PROVEN LOGIC
            (if (setq target_en (handent hdl_ptr))
              (progn
                ;; Now get all BALLPTR values from the TARGET block and clean them - EXACT PROVEN LOGIC
                (setq ballptr_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR")))
                (setq ballptr01_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR01")))
                (setq ballptr02_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR02")))
                (setq ballptr03_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR03")))
                (setq ballptr04_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR04")))
                (setq p_tag_val (c:wd_get_pnlval target_en "P_TAG1"))
                ;; Compare the PNL handle with each BALLPTR value to determine which ITEM to read - EXACT PROVEN LOGIC
                (cond
                  ;; If pnl_hdl matches BALLPTR value
                  ((and ballptr_val (= pnl_hdl ballptr_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "P_ITEM"))
                   (if (not p_item_val) (setq p_item_val "<P_ITEM not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR01 value
                  ((and ballptr01_val (= pnl_hdl ballptr01_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM01"))
                   (if (not p_item_val) (setq p_item_val "<ITEM01 not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR02 value
                  ((and ballptr02_val (= pnl_hdl ballptr02_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM02"))
                   (if (not p_item_val) (setq p_item_val "<ITEM02 not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR03 value
                  ((and ballptr03_val (= pnl_hdl ballptr03_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM03"))
                   (if (not p_item_val) (setq p_item_val "<ITEM03 not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR04 value
                  ((and ballptr04_val (= pnl_hdl ballptr04_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM04"))
                   (if (not p_item_val) (setq p_item_val "<ITEM04 not found>"))
                  )
                  ;; No match found
                  (T
                   (setq p_item_val "<Handle not found in any BALLPTR>")
                  )
                )
              )
              ;; Handle not found
              (setq p_item_val "<Referenced block not found>")
            )
          )
          ;; No handle pointer
          (setq p_item_val "<No BALLPTR in PNL block>")
        )
        
        ;; Compare B_ITEM and P_ITEM values - EXACT PROVEN LOGIC
        (if (and b_item_val p_item_val 
                 (/= b_item_val "") 
                 (/= p_item_val "")
                 (not (wcmatch p_item_val "<*>"))  ; Exclude error messages
                 (/= (strcase b_item_val) (strcase p_item_val)))  ; Case-insensitive comparison
          (progn
            ;; Values are different - write to CSV
            (write-line (strcat 
                          drawing_name "," 
                          blknam "," 
                          b_item_val "," 
                          p_item_val "," 
                          p_tag_val "," 
                          (if pnl_hdl pnl_hdl ""))
                        f)
            (setq diff_count (1+ diff_count))
          )
        )
        
        (setq ix (1+ ix))
      )
      
      ;; Close file
      (close f)
      
      (if (> diff_count 0)
        (prompt (strcat "\nFound " (itoa diff_count) " balloon differences in " drawing_name))
        (prompt (strcat "\nNo balloon differences found in " drawing_name))
      )
    )
  )
  
  ;; Update progress - increment completed count AFTER processing the drawing
  (setq completed (1+ completed))
  (qa_update_progress progress_fname total completed "in_progress")
  
  ;; Read progress again to check if processing is complete
  (setq progress_data (qa_read_progress progress_fname))
  (if progress_data
    (progn
      (setq total (nth 0 progress_data)
            completed (nth 1 progress_data))
      
      ;; Check if all drawings are processed
      (if (>= completed total)
        (progn
          ;; Update status to completed
          (qa_update_progress progress_fname total completed "completed")
          (prompt "\nAll drawings processed. Displaying balloon difference report...")
          
          ;; Display the report directly (no summary calculation needed)
          (qa_display_balloon_diff_report project_name)
        )
      )
    )
  )
    
  (princ)
)

;; Function to display balloon difference report
(defun qa_display_balloon_diff_report (project_name / temp_dir csv_fname)
  (setq temp_dir (getenv "TEMP")
        csv_fname (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-BalloonDiff.csv")))

  (prompt (strcat "\nDisplaying Balloon Difference Report from: " csv_fname))

  (if (findfile csv_fname)
    (progn
      ;; Display in AutoCAD
      (prompt "\nDisplaying QA Balloon Difference table...")
      (c:wd_ins_table csv_fname)
      
      ;; Cleanup temporary files after successful completion
      (c:QA-CleanupBalloonDiffTempFiles)
    )
    (prompt "\nBalloon difference report file not found.")
  )
)

;; Function to cleanup balloon diff temporary files
(defun c:QA-CleanupBalloonDiffTempFiles (/ project_name temp_dir progress_fname scr_fname)
  ;; Get project name
  (setq project_name (qa_get_project_name))
  
  ;; Get temp directory path and create file paths 
  (setq temp_dir (getenv "TEMP")
        progress_fname (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-BalloonDiff_progress.csv"))
        scr_fname (vl-string-translate "\\" "/" (strcat temp_dir "/balloon_diff_script.scr")))
  
  (prompt (strcat "\nCleaning up balloon diff temporary files for project: " project_name))
  
  ;; Delete temporary files
  (if (findfile progress_fname)
    (progn
      (vl-file-delete progress_fname)
      (prompt (strcat "\nDeleted: " progress_fname))
    )
  )
  
  (if (findfile scr_fname)
    (progn
      (vl-file-delete scr_fname)
      (prompt (strcat "\nDeleted: " scr_fname))
    )
  )
  
  ;; Check if final report still exists
  (setq final_csv_fname (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-BalloonDiff.csv")))
  (if (findfile final_csv_fname)
    (prompt (strcat "\nBalloon difference report preserved: " final_csv_fname))
    (prompt "\nWarning: Balloon difference report not found!")
  )
  
  (prompt "\nBalloon diff cleanup completed.")
  (princ)
)

;; Main function to collect balloon difference data from multiple drawings
(defun c:QA-BalloonDiffProject (/ dlst temp_dir csv_fname progress_fname scr_fname f project_name user_choice)
  ;; Get project name
  (setq project_name (qa_get_project_name))
  
  ;; Get temp directory path and create file paths 
  (setq temp_dir (getenv "TEMP")
        csv_fname (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-BalloonDiff.csv"))
        progress_fname (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-BalloonDiff_progress.csv")))
  
  (prompt (strcat "\nProject: " project_name))
  (prompt (strcat "\nBalloon difference report file: " csv_fname))
  
  ;; Check if final report already exists
  (if (findfile csv_fname)
    (progn
      (prompt (strcat "\nExisting balloon difference report found: " csv_fname))
      (setq user_choice (show-yesno-dialog "Balloon QA already exists, Reload the report from beginning?" "DIN Rail summary"))
      (if user_choice
        (progn
          ;; Delete existing files to start fresh
          (vl-file-delete csv_fname)
          (vl-file-delete progress_fname)
          (prompt "\nStarting fresh balloon difference report generation...")
        )
        (progn
          ;; Load existing report
          (prompt "\nLoading existing balloon difference report...")
          (c:wd_ins_table csv_fname)
          (princ)
          (exit)
        )
      )
    )
  )
  
  ;; Get current project's drawing list
  (setq proj_dwgs (nth 6 (c:wd_proj_wdp_data)))
  
  ;; Only proceed if we have a project drawing list
  (if proj_dwgs
    (progn
      ;; Show drawing selection dialog
      (setq dlst (c:wd_pdwgs proj_dwgs 
                            "Select drawings for Balloon Difference Analysis" 
                            "" 
                            nil))
      
      ;; Only proceed if drawings were selected
      (if (and dlst 
               (car dlst)  
               (/= 0 (length (car dlst))))
        (progn
          (prompt (strcat "\nSelected " (itoa (length (car dlst))) " drawings"))
          
          ;; Add current drawing to the end of the list (to handle projwide script behavior)
          (setq current_dwg (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
          (setq drawing_list (append (car dlst) (list current_dwg)))
          
          ;; Initialize progress file
          (qa_update_progress progress_fname (length drawing_list) 0 "in_progress")
          
          ;; Create empty CSV file with headers
          (setq f (open csv_fname "w"))
          (write-line "Drawing,Block_Name,B_ITEM,P_ITEM,TAG,Handle" f)
          (close f)
          
          ;; Create script to call our function with progress monitoring
          (setq scr_fname (strcat temp_dir "/balloon_diff_script.scr"))
          (setq scr_fname (vl-string-translate "\\" "/" scr_fname))
          
          (setq f (open scr_fname "w"))
          (write-line (strcat "(c:QA_CollectBalloonDiffData \"" csv_fname "\" \"" progress_fname "\" \"" project_name "\")") f)
          (close f)
          
          ;; Run script on selected drawings
          (prompt "\nRunning balloon difference analysis on selected drawings...")
          (prompt "\nProgress will be monitored automatically...")
          (c:ace_projwide_script drawing_list scr_fname)
          
          (prompt "\nBalloon difference analysis has been initiated.")
          (prompt "\nThe report will be generated automatically when all drawings are processed.")
        )
        (prompt "\nNo drawings selected - operation canceled.")
      )
    )
    (prompt "\nNo active project found.")
  )
  (princ)
)

;; Function for single drawing balloon difference analysis using EXACT PROVEN LOGIC
(defun c:QA-BalloonDiff (/ ss slen ix ben blknam b_item_val hdl_ptr p_item_val target_en 
                          ballptr_val ballptr01_val ballptr02_val ballptr03_val ballptr04_val
                          clean_hdl target_p_item_attr pnl_hdl diff_count temp_dir csv_file f project_name drawing_name)
  ;; Make sure AutoCAD Electrical is loaded
  (if (not (vl-catch-all-error-p (vl-catch-all-apply 'c:wd_get_pnlval '(nil nil))))
    (load "wd_load.lsp")
  )
  
  ;; Get project name and drawing name
  (setq project_name (qa_get_project_name))
  (setq drawing_name (getvar "DWGNAME"))
  
  ;; Get temp directory path and create file paths 
  (setq temp_dir (getenv "TEMP")
        csv_file (vl-string-translate "\\" "/" (strcat temp_dir "/" project_name "_QA-BalloonDiff-single.csv")))
  
  ;; Get all INSERT entities (blocks) in the drawing that start with "PNL" - USING EXACT PROVEN LOGIC
  (setq ss (ssget "_X" '((-4 . "<AND")(0 . "INSERT")(2 . "PNL*")(-4 . "AND>"))))
  (setq diff_count 0)
  
  ;; Process each block reference using EXACT PROVEN LOGIC
  (if ss
    (progn
      ;; Create output CSV file
      (vl-file-delete csv_file)
      (setq f (open csv_file "w"))
      
      ;; Write header
      (write-line "Drawing,Block_Name,B_ITEM,P_ITEM,Handle" f)
      
      (setq slen (sslength ss))
      (setq ix 0)
      
      ;; Process each block - EXACT LOGIC FROM list-pnl-diff
      (while (< ix slen)
        (setq ben (ssname ss ix))  ; Get entity name of current block
        
        ;; Get block name
        (setq blknam (cdr (assoc 2 (entget ben))))
        
        ;; Get B_ITEM attribute value using wd_get_pnlval - EXACT PROVEN LOGIC
        ;; This function looks for ATTRIB first, if not found, looks for XDATA value
        (setq b_item_val (c:wd_get_pnlval ben "B_ITEM"))
        
        ;; If not found, set to empty string - EXACT PROVEN LOGIC
        (if (not b_item_val)
          (setq b_item_val "")
        )
        
        ;; First, get the handle pointer and PNL handle from the PNL block - EXACT PROVEN LOGIC
        (setq hdl_ptr (c:wd_get_pnlval ben "BALLPTR"))
        (setq pnl_hdl (c:wd_get_pnlval ben "HDL"))
        
        ;; Process the handle pointer to get the referenced block - EXACT PROVEN LOGIC
        (setq p_item_val "")
        (setq target_en nil)
        
        (if (and hdl_ptr (/= hdl_ptr ""))
          (progn
            ;; Remove comma from handle string if present using helper function - EXACT PROVEN LOGIC
            (setq hdl_ptr (remove_trailing_comma hdl_ptr))
            
            ;; Try to get entity from handle - EXACT PROVEN LOGIC
            (if (setq target_en (handent hdl_ptr))
              (progn
                ;; Now get all BALLPTR values from the TARGET block and clean them - EXACT PROVEN LOGIC
                (setq ballptr_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR")))
                (setq ballptr01_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR01")))
                (setq ballptr02_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR02")))
                (setq ballptr03_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR03")))
                (setq ballptr04_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR04")))
                
                ;; Compare the PNL handle with each BALLPTR value to determine which ITEM to read - EXACT PROVEN LOGIC
                (cond
                  ;; If pnl_hdl matches BALLPTR value
                  ((and ballptr_val (= pnl_hdl ballptr_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "P_ITEM"))
                   (if (not p_item_val) (setq p_item_val "<P_ITEM not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR01 value
                  ((and ballptr01_val (= pnl_hdl ballptr01_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM01"))
                   (if (not p_item_val) (setq p_item_val "<ITEM01 not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR02 value
                  ((and ballptr02_val (= pnl_hdl ballptr02_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM02"))
                   (if (not p_item_val) (setq p_item_val "<ITEM02 not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR03 value
                  ((and ballptr03_val (= pnl_hdl ballptr03_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM03"))
                   (if (not p_item_val) (setq p_item_val "<ITEM03 not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR04 value
                  ((and ballptr04_val (= pnl_hdl ballptr04_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM04"))
                   (if (not p_item_val) (setq p_item_val "<ITEM04 not found>"))
                  )
                  ;; No match found
                  (T
                   (setq p_item_val "<Handle not found in any BALLPTR>")
                  )
                )
              )
              ;; Handle not found
              (setq p_item_val "<Referenced block not found>")
            )
          )
          ;; No handle pointer
          (setq p_item_val "<No BALLPTR in PNL block>")
        )
        
        ;; Compare B_ITEM and P_ITEM values - EXACT PROVEN LOGIC
        (if (and b_item_val p_item_val 
                 (/= b_item_val "") 
                 (/= p_item_val "")
                 (not (wcmatch p_item_val "<*>"))  ; Exclude error messages
                 (/= (strcase b_item_val) (strcase p_item_val)))  ; Case-insensitive comparison
          (progn
            ;; Values are different - write to CSV
            (write-line (strcat 
                          drawing_name "," 
                          blknam "," 
                          b_item_val "," 
                          p_item_val "," 
                          (if pnl_hdl pnl_hdl ""))
                        f)
            (setq diff_count (1+ diff_count))
          )
        )
        
        (setq ix (1+ ix))
      )
      
      ;; Close file
      (close f)
      
      ;; Report results
      (if (> diff_count 0)
        (progn
          (prompt (strcat "\nBalloon difference analysis completed. Found " (itoa diff_count) " differences."))
          ;; Display balloon differences table
          (c:wd_ins_table csv_file)
        )
        (prompt "\nNo balloon differences found in the drawing.")
      )
    )
    (prompt "\nNo PNL block references found in the drawing.")
  )
  
  (princ)
)
;###############################################################################################################
; Function to count blocks with specific MFG and CAT attributes
(defun c:CountPnlMfgCat (/ ss count slen ix ben mfg_val cat_val search_mfg search_cat)
  
  ;; Prompt for MFG and CAT values to search for
  (setq search_mfg (getstring "\nEnter MFG value to search for: "))
  (setq search_cat (getstring "\nEnter CAT value to search for: "))
  
  ;; Initialize counter
  (setq count 0)
  (setvar "USERI3" count)
  
  ;; Select all INSERT entities
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  
  ;; Check if selection set is not nil
  (if ss
    (progn
      ;; Get selection set length
      (setq slen (sslength ss))
      
      ;; Loop through each entity in the selection set
      (setq ix 0)
      (while (< ix slen)
        ;; Get entity name
        (setq ben (ssname ss ix))
        
        ;; Get MFG and CAT attribute values
        (setq mfg_val (c:wd_get_pnlval ben "MFG"))
        (setq cat_val (c:wd_get_pnlval ben "CAT"))
        
        ;; Check if MFG and CAT match the search values
        (if (and mfg_val cat_val 
                 (/= mfg_val "") (/= cat_val "")
                 (= (strcase mfg_val) (strcase search_mfg))
                 (= (strcase cat_val) (strcase search_cat)))
          ;; Increment counter
          (setq count (1+ count))
        )
        
        ;; Move to next entity
        (setq ix (1+ ix))
      )
      
      ;; Update USERI3 environment variable
      (setvar "USERI3" count)
      
      ;; Output count to console
      (princ (strcat "\nCount of blocks with \"" search_mfg 
                     "\" and \"" search_cat "\" = " (itoa count)))
    )
    ;; No INSERT entities found
    (princ "\nNo block entities found in the drawing.")
  )
  
  (princ)  ;; Clean exit
)

(defun c:RefreshBalloons (/ blk cnt)
  ;(setvar "CMDECHO" 0) ; Turn off command echoing
  (command "_.attsync" "_NAME" "PNL*")
  (princ) ; Exit quietly
)

(defun c:list-pnl-data ( / ss slen ix ben blknam b_item_val hdl_ptr p_item_val target_en 
                         ballptr_val ballptr01_val ballptr02_val ballptr03_val ballptr04_val
                         clean_hdl target_p_item_attr pnl_hdl)
  ;; Function to list all PNL blocks showing B_ITEM and corresponding P_ITEM values
  ;; Checks multiple BALLPTR attributes and maps to corresponding ITEM attributes
  ;; 
  ;; Variable naming convention:
  ;; ben = Block Entity Name (specifically for INSERT entities/blocks)
  ;; en  = Entity Name (generic term for any AutoCAD entity)
  ;;
  ;; wd_get_pnlval: Gets ATTRIB or XDATA (1000) value from panel footprint block "ben" 
  ;; XDATA in form "VIA_WD_attname". Looks for ATTRIB first, if not found, looks for XDATA value.
  
  ;; Helper function to remove trailing comma from strings
  (defun remove_trailing_comma (str)
    (if (or (null str) (= str ""))
      ""
      (if (wcmatch str "*,*")
        (substr str 1 (- (strlen str) 1))
        str
      )
    )
  )
  
  (princ "\nScanning all PNL blocks and comparing B_ITEM with P_ITEM values...\n")
  
  ;; Get all INSERT entities (blocks) in the drawing that start with "PNL"
  (setq ss (ssget "_X" '((-4 . "<AND")(0 . "INSERT")(2 . "PNL*")(-4 . "AND>"))))
  
  (if ss
    (progn
      (setq slen (sslength ss))  ; Number of blocks found
      (setq ix 0)                ; Counter
      
      (princ (strcat "Found " (itoa slen) " PNL blocks:\n"))
      (princ "Block Name\t\tB_ITEM Value\t\tP_ITEM Value\n")
      (princ "================================================================\n")
      
      ;; Process each block
      (while (< ix slen)
        (setq ben (ssname ss ix))  ; Get entity name of current block
        
        ;; Get block name
        (setq blknam (cdr (assoc 2 (entget ben))))
        
        ;; Get B_ITEM attribute value using wd_get_pnlval
        ;; This function looks for ATTRIB first, if not found, looks for XDATA value
        (setq b_item_val (c:wd_get_pnlval ben "B_ITEM"))
        
        ;; If not found, set to empty string
        (if (not b_item_val)
          (setq b_item_val "")
        )
        
        ;; First, get the handle pointer and PNL handle from the PNL block
        (setq hdl_ptr (c:wd_get_pnlval ben "BALLPTR"))
        (setq pnl_hdl (c:wd_get_pnlval ben "HDL"))
        
        ;; Process the handle pointer to get the referenced block
        (setq p_item_val "")
        (setq target_en nil)
        
        (if (and hdl_ptr (/= hdl_ptr ""))
          (progn
            ;; Remove comma from handle string if present using helper function
            (setq hdl_ptr (remove_trailing_comma hdl_ptr))
            
            ;; Try to get entity from handle
            (if (setq target_en (handent hdl_ptr))
              (progn
                ;; Now get all BALLPTR values from the TARGET block and clean them
                (setq ballptr_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR")))
                (setq ballptr01_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR01")))
                (setq ballptr02_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR02")))
                (setq ballptr03_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR03")))
                (setq ballptr04_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR04")))
                
                ;; Compare the PNL handle with each BALLPTR value to determine which ITEM to read
                (cond
                  ;; If pnl_hdl matches BALLPTR value
                  ((and ballptr_val (= pnl_hdl ballptr_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "P_ITEM"))
                   (if (not p_item_val) (setq p_item_val "<P_ITEM not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR01 value
                  ((and ballptr01_val (= pnl_hdl ballptr01_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM01"))
                   (if (not p_item_val) (setq p_item_val "<ITEM01 not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR02 value
                  ((and ballptr02_val (= pnl_hdl ballptr02_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM02"))
                   (if (not p_item_val) (setq p_item_val "<ITEM02 not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR03 value
                  ((and ballptr03_val (= pnl_hdl ballptr03_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM03"))
                   (if (not p_item_val) (setq p_item_val "<ITEM03 not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR04 value
                  ((and ballptr04_val (= pnl_hdl ballptr04_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM04"))
                   (if (not p_item_val) (setq p_item_val "<ITEM04 not found>"))
                  )
                  ;; No match found
                  (T
                   (setq p_item_val "<Handle not found in any BALLPTR>")
                  )
                )
              )
              ;; Handle not found
              (setq p_item_val "<Referenced block not found>")
            )
          )
          ;; No handle pointer
          (setq p_item_val "<No BALLPTR in PNL block>")
        )
        
        ;; Display all blocks (both matching and different)
        (princ (strcat blknam "\t\t" b_item_val "\t\t" p_item_val "\n"))
        
        ;; Increment counter
        (setq ix (1+ ix))
      )
      
      (setq ss nil)  ; Release selection set
    )
    ;; No PNL blocks found
    (princ "No blocks starting with 'PNL' found in the drawing.\n")
  )
  
  (princ)  ; Clean exit
)

(defun c:list-pnl-diff ( / ss slen ix ben blknam b_item_val hdl_ptr p_item_val target_en 
                          ballptr_val ballptr01_val ballptr02_val ballptr03_val ballptr04_val
                          clean_hdl target_p_item_attr pnl_hdl diff_count)
  ;; Function to list only PNL blocks where B_ITEM and P_ITEM values are different
  ;; Checks multiple BALLPTR attributes and maps to corresponding ITEM attributes
  ;; 
  ;; Variable naming convention:
  ;; ben = Block Entity Name (specifically for INSERT entities/blocks)
  ;; en  = Entity Name (generic term for any AutoCAD entity)
  ;;
  ;; wd_get_pnlval: Gets ATTRIB or XDATA (1000) value from panel footprint block "ben" 
  ;; XDATA in form "VIA_WD_attname". Looks for ATTRIB first, if not found, looks for XDATA value.
  
  ;; Helper function to remove trailing comma from strings
  (defun remove_trailing_comma (str)
    (if (or (null str) (= str ""))
      ""
      (if (wcmatch str "*,*")
        (substr str 1 (- (strlen str) 1))
        str
      )
    )
  )
  
  (princ "\nScanning PNL blocks for differences between B_ITEM and P_ITEM values...\n")
  
  ;; Get all INSERT entities (blocks) in the drawing that start with "PNL"
  (setq ss (ssget "_X" '((-4 . "<AND")(0 . "INSERT")(2 . "PNL*")(-4 . "AND>"))))
  (setq diff_count 0)  ; Counter for blocks with differences
  
  (if ss
    (progn
      (setq slen (sslength ss))  ; Number of blocks found
      (setq ix 0)                ; Counter
      
      (princ (strcat "Checking " (itoa slen) " PNL blocks for differences...\n"))
      (princ "Block Name\t\tB_ITEM Value\t\tP_ITEM Value\n")
      (princ "================================================================\n")
      
      ;; Process each block
      (while (< ix slen)
        (setq ben (ssname ss ix))  ; Get entity name of current block
        
        ;; Get block name
        (setq blknam (cdr (assoc 2 (entget ben))))
        
        ;; Get B_ITEM attribute value using wd_get_pnlval
        ;; This function looks for ATTRIB first, if not found, looks for XDATA value
        (setq b_item_val (c:wd_get_pnlval ben "B_ITEM"))
        
        ;; If not found, set to empty string
        (if (not b_item_val)
          (setq b_item_val "")
        )
        
        ;; First, get the handle pointer and PNL handle from the PNL block
        (setq hdl_ptr (c:wd_get_pnlval ben "BALLPTR"))
        (setq pnl_hdl (c:wd_get_pnlval ben "HDL"))
        
        ;; Process the handle pointer to get the referenced block
        (setq p_item_val "")
        (setq target_en nil)
        
        (if (and hdl_ptr (/= hdl_ptr ""))
          (progn
            ;; Remove comma from handle string if present using helper function
            (setq hdl_ptr (remove_trailing_comma hdl_ptr))
            
            ;; Try to get entity from handle
            (if (setq target_en (handent hdl_ptr))
              (progn
                ;; Now get all BALLPTR values from the TARGET block and clean them
                (setq ballptr_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR")))
                (setq ballptr01_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR01")))
                (setq ballptr02_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR02")))
                (setq ballptr03_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR03")))
                (setq ballptr04_val (remove_trailing_comma (c:wd_get_pnlval target_en "BALLPTR04")))
                
                ;; Compare the PNL handle with each BALLPTR value to determine which ITEM to read
                (cond
                  ;; If pnl_hdl matches BALLPTR value
                  ((and ballptr_val (= pnl_hdl ballptr_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "P_ITEM"))
                   (if (not p_item_val) (setq p_item_val "<P_ITEM not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR01 value
                  ((and ballptr01_val (= pnl_hdl ballptr01_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM01"))
                   (if (not p_item_val) (setq p_item_val "<ITEM01 not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR02 value
                  ((and ballptr02_val (= pnl_hdl ballptr02_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM02"))
                   (if (not p_item_val) (setq p_item_val "<ITEM02 not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR03 value
                  ((and ballptr03_val (= pnl_hdl ballptr03_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM03"))
                   (if (not p_item_val) (setq p_item_val "<ITEM03 not found>"))
                  )
                  ;; If pnl_hdl matches BALLPTR04 value
                  ((and ballptr04_val (= pnl_hdl ballptr04_val))
                   (setq p_item_val (c:wd_get_pnlval target_en "ITEM04"))
                   (if (not p_item_val) (setq p_item_val "<ITEM04 not found>"))
                  )
                  ;; No match found
                  (T
                   (setq p_item_val "<Handle not found in any BALLPTR>")
                  )
                )
              )
              ;; Handle not found
              (setq p_item_val "<Referenced block not found>")
            )
          )
          ;; No handle pointer
          (setq p_item_val "<No BALLPTR in PNL block>")
        )
        
        ;; Compare B_ITEM and P_ITEM values and display only if different
        (if (and b_item_val p_item_val 
                 (/= b_item_val "") 
                 (/= p_item_val "")
                 (not (wcmatch p_item_val "<*>"))  ; Exclude error messages
                 (/= (strcase b_item_val) (strcase p_item_val)))  ; Case-insensitive comparison
          (progn
            ;; Values are different - display this block
            (princ (strcat blknam "\t\t" b_item_val "\t\t" p_item_val "\n"))
            (setq diff_count (1+ diff_count))
          )
        )
        
        ;; Increment counter
        (setq ix (1+ ix))
      )
      
      (setq ss nil)  ; Release selection set
      
      ;; Summary
      (princ "================================================================\n")
      (if (> diff_count 0)
        (princ (strcat "Found " (itoa diff_count) " PNL blocks with different B_ITEM and P_ITEM values.\n"))
        (princ "All PNL blocks have matching B_ITEM and P_ITEM values.\n")
      )
    )
    ;; No PNL blocks found
    (princ "No blocks starting with 'PNL' found in the drawing.\n")
  )
  
  (princ)  ; Clean exit
)
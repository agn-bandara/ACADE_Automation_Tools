(defun c:addFileToProject ( / prjPath dwgName x sectionCode subsectionCode subFolder dwgDesc)
    (progn
        ; Make sure AcadE is "awake"
        (if (not (wd_load))  ; If not in memory
            (if (setq x (findfile "wd_load.lsp"))
                (load x)  ; Load AcadE init functions
            )
        )
        (wd_load)  ; Ensure AcadE functions are loaded
        
        (if GBL_wd_prj
            (progn
                ; Assuming c:wd_split_fnam returns a list where the car is the path
                (setq prjPath (car (c:wd_split_fnam GBL_wd_prj)))  ; Correctly assign the directory to prjPath
                (setq dwgName (getstring T "\nEnter Name for New Drawing: "))
                (setq dwgName (strcat prjPath dwgName ".dwg"))  ; Concatenate the path and the new drawing name
                (setq dwgDesc (getstring T "\nEnter Description for New Drawing: "))
                (setq sectionCode (getstring T "\nEnter Section Code: "))
                (setq subsectionCode (getstring T "\nEnter Subsection Code: "))
                (setq subFolder (getstring T "\nEnter Subfolder: "))
                (setq subFolder (if (equal subFolder "") nil subFolder))
                (c:ace_add_dwg_to_project dwgName (list sectionCode subsectionCode dwgDesc nil subFolder))  ; Add the drawing to the project)
            )
            (princ "\nNo Project Loaded")
        )
    )
    (princ)  ; Ensure the function ends with princ to return to the command line cleanly
)

(defun remove-duplicates (lst / result item)
    (setq result '()) ; Initialize result list to empty
    (foreach item lst
        (if (not (member item result)) ; Check if item is not already in result
            (setq result (cons item result)) ; Add item to result if not present
        )
    )
    (reverse result) ; Reverse result to maintain original order
)

(defun c:getSubFolderList ( / x subfList uniqueSubfList)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)
        )
    )
    (wd_load) 

    ; Assuming c:wd_proj_wdp_data returns a list structure properly
    (setq subfList (mapcar 'car (nth 10 (c:wd_proj_wdp_data))))

    ; Remove duplicates from the subfolder list
    (setq uniqueSubfList (remove-duplicates subfList))

    ; Output the unique subfolder list for verification
    uniqueSubfList
)


(defun c:GetCSVFilePath ( / filePath)
  ;; Use getfiled to open the file dialog
  ;; Arguments are: dialog title, default filename, default extension, dialog flags
  (setq filePath (getfiled "Select CSV File" "" "csv" 0))

  ;; Check if a file was selected
  (if filePath
    (progn
      ;; If a file was selected, store the file path in an environment variable
      (setvar "USERS1" filePath)
      (princ (strcat "\nSelected CSV file path: " filePath))
    )
    ;; If no file was selected, notify the user
    (princ "\nNo file selected.")
  )
  ;; Ensure a clean exit
  (princ)
)

(defun c:GetCSVFilePath2 ( / filePath)
  ;; Use getfiled to open the file dialog
  ;; Arguments are: dialog title, default filename, default extension, dialog flags
  (setq filePath (getfiled "Select CSV File" "" "csv" 0))

  ;; Check if a file was selected
  (if filePath
    (progn
      ;; If a file was selected, store the file path in an environment variable
      (setvar "USERS2" filePath)
      (princ (strcat "\nSelected CSV file path: " filePath))
    )
    ;; If no file was selected, notify the user
    (princ "\nNo file selected.")
  )
  ;; Ensure a clean exit
  (princ)
)

;; To make the function easily callable, add a command wrapper
(defun c:SelectCSVFile ( / )
  (c:GetCSVFilePath)
)

(defun c:CreateProjectSubfolderTable ()
  ; Ensure AcadE is "awake" and auxiliary functions are loaded
  (if (not (wd_load))
    (if (setq x (findfile "wd_load.lsp"))
      (load x)
    )
  )
  (wd_load)
  
  (setq proj_data (c:wd_proj_wdp_data))
  (if proj_data
    (progn
      (setq subfolder_data (nth 10 proj_data))
      (if subfolder_data
        (progn
          (setq pt (getpoint "\nSelect insertion point for table: "))
          (if pt
            (progn
              ;; Get the current document and model space
              (setq doc (vla-get-activedocument (vlax-get-acad-object)))
              (setq mspace (vla-get-modelspace doc))

              ;; Create the table
              (setq table (vla-addtable mspace 
                                        (vlax-3d-point pt) 
                                        (+ (length subfolder_data) 1) 
                                        2 
                                        10 
                                        50))

              ;; Set header text
              (vla-settext table 0 0 "Subfolder")
              (vla-settext table 0 1 "Drawing Name")

              ;; Fill the table with the data
              (setq row 1)
              (foreach item subfolder_data
                (if (> (length item) 1)
                  (progn
                    (setq subfolder (nth 0 item))
                    (setq full_path (nth 1 item))
                    (setq file_name (vl-filename-base full_path))
                    
                    (vla-settext table row 0 subfolder)
                    (vla-settext table row 1 file_name)
                    (setq row (1+ row))
                  )
                )
              )
              
              (princ "\nTable of project subfolders and drawings created successfully.")
            )
            (princ "\nInvalid insertion point.")
          )
        )
        (princ "\nNo subfolder data found in project data.")
      )
    )
    (princ "\nFailed to retrieve project data.")
  )
  (princ)
)

(defun strip_quotes (str / )
  "Removes leading and trailing quotes from a string"
  (vl-string-right-trim "\""
    (vl-string-left-trim "\"" str))
)

(defun c:copy_project_drawing ( / old_name new_name desc1 desc2 desc3 sheet sec subsec folder
                                 proj_path source_path dest_path)
  ; Get project path
  (setq proj_path (car (c:wd_split_fnam (ace_getactiveproject))))
  
  ; Prompt for values 
  (princ "\nEnter values (press Enter for empty/default):")
  
  ; Get and clean input values
  (setq old_name (strip_quotes (getstring T "\nSource drawing name (without extension): ")))
  (setq new_name (strip_quotes (getstring T "\nNew drawing name (without extension): ")))
  (setq desc1 (strip_quotes (getstring T "\nDescription Line 1: ")))
  (setq desc2 (strip_quotes (getstring T "\nDescription Line 2: ")))
  (setq desc3 (strip_quotes (getstring T "\nDescription Line 3: ")))
  (setq sheet (strip_quotes (getstring T "\nSheet Number: ")))  ; Keep prompt for VBA compatibility
  (setq sec (strip_quotes (getstring T "\nSection code (e.g. 'Sec'): ")))
  (setq subsec (strip_quotes (getstring T "\nSubsection code (e.g. 'Subsec'): ")))
  (setq folder (strip_quotes (getstring T "\nFolder: ")))
  
  ; Construct full paths
  (setq source_path (strcat proj_path old_name ".dwg"))
  (setq dest_path (strcat proj_path new_name ".dwg"))
  
  ; Convert backslashes to forward slashes
  (setq source_path (vl-string-translate "\\" "/" source_path))
  (setq dest_path (vl-string-translate "\\" "/" dest_path))
  
  (if (findfile source_path)
    (progn
      ; Copy the file
      (vl-file-copy source_path dest_path)
      
      ; Add to project with new properties
      (c:ace_add_dwg_to_project 
        dest_path
        (list 
          sec                 ; Section code
          subsec             ; Subsection code
          (list desc1 desc2 desc3)  ; Description lines
        )
      )
      
      (princ (strcat "\nDrawing " new_name " added to project successfully."))
    )
    (princ (strcat "\nSource drawing \"" old_name "\" not found at: " source_path))
  )
  (princ)
)

(defun c:update_project_drawing ( / dwg_name desc1 desc2 desc3 sheet sec subsec folder
                                  proj_path dwg_path)
  ; Get project path
  (setq proj_path (car (c:wd_split_fnam (ace_getactiveproject))))
  
  ; Prompt for values 
  (princ "\nEnter values (press Enter for empty/default):")
  
  ; Get and clean input values
  (setq dwg_name (strip_quotes (getstring T "\nDrawing name (without extension): ")))
  (setq desc1 (strip_quotes (getstring T "\nDescription Line 1: ")))
  (setq desc2 (strip_quotes (getstring T "\nDescription Line 2: ")))
  (setq desc3 (strip_quotes (getstring T "\nDescription Line 3: ")))
  (setq sheet (strip_quotes (getstring T "\nSheet Number: ")))  ; Keep prompt for VBA compatibility
  (setq sec (strip_quotes (getstring T "\nSection code (e.g. 'Sec'): ")))
  (setq subsec (strip_quotes (getstring T "\nSubsection code (e.g. 'Subsec'): ")))
  (setq folder (strip_quotes (getstring T "\nFolder: ")))
  
  ; Construct full path
  (setq dwg_path (strcat proj_path dwg_name ".dwg"))
  
  ; Convert backslashes to forward slashes
  (setq dwg_path (vl-string-translate "\\" "/" dwg_path))
  
  (if (findfile dwg_path)
    (progn
      ; Update project properties
      (c:ace_add_dwg_to_project 
        dwg_path
        (list 
          sec                 ; Section code
          subsec             ; Subsection code
          (list desc1 desc2 desc3)  ; Description lines
        )
      )
      
      (princ (strcat "\nDrawing " dwg_name " updated in project successfully."))
    )
    (princ (strcat "\nDrawing \"" dwg_name "\" not found at: " dwg_path))
  )
  (princ)
)

(defun convert_path_to_forward (path / )
  ; Convert backslashes to forward slashes
  (vl-string-translate "\\" "/" path)
)

(defun c:wdp_update_desc2 ( / wdpfnam backupNum wdp_data_lst t_lst a_lst p_lst d2_lst new_d2_lst)
  ;; Prompt the user to select the .wdp file to update.
  (setq wdpfnam (getfiled "Select .wdp file to update" "" "wdp" 1))
  (if wdpfnam
    (progn
      ;; Create a backup if the file exists.
      (if (findfile wdpfnam)
        (progn
          (setq backupNum 1)
          (while (findfile (strcat wdpfnam ".back" (itoa backupNum)))
            (setq backupNum (1+ backupNum))
          )
          (vl-file-copy wdpfnam (strcat wdpfnam ".back" (itoa backupNum)))
          (princ (strcat "\nBackup created: " wdpfnam ".back" (itoa backupNum)))
        )
        (princ "\nNo file found to backup.")
      )
      ;; Retrieve current project settings.
      ;; Assumes that c:wd_proj_wdp_data returns a list where:
      ;;   (nth 2 ...) = t_lst,
      ;;   (nth 3 ...) = a_lst,
      ;;   (nth 4 ...) = p_lst,
      ;;   (nth 6 ...) = d2_lst.
      (setq wdp_data_lst (c:wd_proj_wdp_data))
      (setq t_lst (nth 2 wdp_data_lst))
      (setq a_lst (nth 3 wdp_data_lst))
      (setq p_lst (nth 4 wdp_data_lst))
      (setq d2_lst (nth 6 wdp_data_lst))
      ;; Update each entry in d2_lst: copy the final element (index 8)
      ;; into the Description2 field (index 6). (Other elements remain unchanged.)
      (setq new_d2_lst
            (mapcar
              (function
                (lambda (entry)
                  (if (and (listp entry) (>= (length entry) 9))
                    (list (nth 0 entry)  ; id
                          (nth 1 entry)  ; sheet/drawing group
                          (nth 2 entry)  ; often blank
                          (nth 3 entry)  ; drawing path
                          (nth 4 entry)  ; first description/section
                          (nth 5 entry)  ; often blank
                          (nth 8 entry)  ; update Description2 with final element
                          (nth 7 entry)  ; often blank
                          (nth 8 entry)  ; subfolder name remains
                    )
                    entry
                  )
                )
              )
              d2_lst
            )
      )
      ;; Write out the updated .wdp file.
      (c:wd_proj_wdp_write wdpfnam t_lst a_lst p_lst new_d2_lst)
      (princ "\nWDP file updated successfully.")
    )
    (princ "\nNo file selected.")
  )
  (princ)
)

(defun c:getDwgPLCAttr ( / d_lst rtrn first_drawing_path fhand plc_handles 
                         plc_attr_list temp_folder csv_filename csv_file)
  ;; Reset flag to indicate processing started
  (setvar "USERI5" 0)
  
  ;; Make sure AcadE is "awake"
  (if (not wd_load)
      (if (setq x (findfile "wd_load.lsp"))
          (load x)
      )
  )
  (wd_load) ;; wake up AcadE if not already awake
  
  ;; Get the temp folder path
  (setq temp_folder (getenv "TEMP"))
  
  ;; Ask user for the CSV filename
  (setq csv_filename (getstring "\nEnter filename for CSV export (without extension): "))
  
  ;; Add .csv extension if not provided
  (if (not (wcmatch (strcase csv_filename) "*.CSV"))
      (setq csv_filename (strcat csv_filename ".csv"))
  )
  
  ;; Get the list of drawings from the project
  (setq d_lst (nth 6 (c:wd_proj_wdp_data)))
  
  ;; Show dialog to select drawings
  (setq rtrn (c:wd_pdwgs d_lst "Select Drawing" "Choose a drawing to extract PLC attributes" nil))
  
  ;; Check if any drawing was selected
  (if rtrn
      (progn
        ;; Get the first drawing path from the list of selected drawings
        (setq first_drawing_path (car (nth 0 rtrn)))
        
        ;; Print the path to verify
        (princ "\nSelected drawing: ")
        (princ first_drawing_path)
        
        ;; Open the drawing file using WD_DBX_OPEN with performance flag set to T
        (setq fhand (wd_dbx_open first_drawing_path "r" T))
        
        (if fhand
            (progn
              ;; Use WD_DBX_SSGET to find all PLC blocks in the drawing
              ;; Look for blocks with names starting with "PLCIO_"
              (setq plc_handles (wd_dbx_ssget fhand "_X" '((-4 . "<AND")
                                                           (0 . "INSERT")
                                                           (2 . "PLCIO_*")
                                                           (-4 . "AND>"))))
              
              ;; Initialize the list to store all PLC attribute data
              (setq plc_attr_list '())
              
              ;; Check if any PLC blocks were found
              (if plc_handles
                  (progn
                    (princ "\n\nFound ")
                    (princ (length plc_handles))
                    (princ " PLC blocks.")
                    
                    ;; Create the full CSV file path
                    (setq csv_path (strcat temp_folder "\\" csv_filename))
                    
                    ;; Open the CSV file for writing
                    (setq csv_file (open csv_path "w"))
                    
                    ;; Write CSV header
                    (write-line "Block Name,Attribute Name,Attribute Value" csv_file)
                    
                    ;; Process each PLC block
                    (foreach handle plc_handles
                      ;; Get entity data for the PLC block
                      (setq plc_en (wd_dbx_entget handle fhand))
                      
                      ;; Extract block name
                      (setq block_name (cdr (assoc 2 plc_en)))
                      
                      ;; Get all attributes for this PLC block
                      (setq attr_data (wd_dbx_entattr handle fhand nil))
                      
                      ;; Write attribute data to CSV
                      (foreach attr attr_data
                        (write-line (strcat 
                                    "\"" block_name "\","
                                    "\"" (car attr) "\","
                                    "\"" (cadr attr) "\"") 
                                   csv_file)
                      )
                      
                      ;; Convert to just name/value pairs for return value
                      (setq name_value_pairs '())
                      (foreach attr attr_data
                        (setq name_value_pairs 
                          (cons (list (car attr) (cadr attr)) name_value_pairs))
                      )
                      
                      ;; Add the attribute data to our aggregate list
                      (setq plc_attr_list (append plc_attr_list 
                                                 (list (list handle block_name name_value_pairs))))
                    )
                    
                    ;; Close the CSV file
                    (close csv_file)
                    
                    ;; Inform the user that the file has been created
                    (princ "\nCSV file created: ")
                    (princ csv_path)
                    
                    ;; Set flag to indicate successful completion
                    (setvar "USERI5" 1)
                    
                    ;; Return the complete PLC attribute list
                    plc_attr_list
                  )
                  (progn
                    (princ "\nNo PLC blocks found in the drawing.")
                    ;; Set flag to indicate error
                    (setvar "USERI5" -1)
                    nil
                  )
              )
              
              ;; Close the drawing
              (wd_dbx_close fhand)
            )
            (progn
              (princ "\nFailed to open drawing file.")
              ;; Set flag to indicate error
              (setvar "USERI5" -1)
              nil
            )
        )
      )
      (progn
        (princ "\nNo drawing selected.")
        ;; Set flag to indicate error (user canceled)
        (setvar "USERI5" -1)
        nil
      )
  )
  
  (princ) ;; Exit quietly
)

(defun c:wdp_update_descriptions ( / wdpfnam backupNum wdp_data_lst t_lst a_lst p_lst d2_lst 
                                   selected_dwgs selected_indices new_d2_lst i updated_count
                                   desc1 desc2 desc3 user_input)
  
    ;; Prompt user for the description values
  (princ "\nEnter description values to apply to selected drawings (Press ENTER to skip):")
  
  (princ "\nDescription Line 1: ")
  (setq desc1 (strip_quotes (getstring T)))
  (if (= desc1 "") (setq desc1 nil))
  
  (princ "Description Line 2: ")
  (setq desc2 (strip_quotes (getstring T)))
  (if (= desc2 "") (setq desc2 nil))
  
  (princ "Description Line 3: ")
  (setq desc3 (strip_quotes (getstring T)))
  (if (= desc3 "") (setq desc3 nil))
  
  ;; Read existing WDP file data first
  (setq wdp_data_lst (c:wd_proj_wdp_data))
  
  (if wdp_data_lst
    (progn
      ;; Get the WDP filename from the current project data
      (setq wdpfnam (nth 0 wdp_data_lst))
      
      ;; Extract the drawing list for selection dialog
      (setq d_lst (nth 6 wdp_data_lst)) ; drawing list with sec/subsec data
      
      ;; Let user select which drawings to update
      (setq selected_dwgs (c:wd_pdwgs d_lst 
                                     "Select Drawings to Update Descriptions" 
                                     "Choose drawings to update description lines" 
                                     nil)) ; don't exclude ref-only drawings
      
      (if selected_dwgs
        (progn
          ;; Get the selected drawing indices
          (setq selected_indices (nth 1 selected_dwgs))
          
          ;; Create backup of the WDP file
          (if (findfile wdpfnam)
            (progn
              (setq backupNum 1)
              (while (findfile (strcat wdpfnam ".back" (itoa backupNum)))
                (setq backupNum (1+ backupNum))
              )
              (vl-file-copy wdpfnam (strcat wdpfnam ".back" (itoa backupNum)))
              (princ (strcat "\nBackup created: " wdpfnam ".back" (itoa backupNum)))
            )
          )
          
          ;; Extract current project components
          (setq t_lst (nth 2 wdp_data_lst))   ; title lines
          (setq a_lst (nth 3 wdp_data_lst))   ; parameter values  
          (setq p_lst (nth 4 wdp_data_lst))   ; paths and project values
          (setq d2_lst (nth 6 wdp_data_lst))  ; drawing data with sec/subsec
          
          ;; Update only selected drawings
          (setq new_d2_lst '())
          (setq i 0)
          (setq updated_count 0)
          
          (foreach entry d2_lst
            (if (and (member i selected_indices)
                     (listp entry) 
                     (>= (length entry) 9))
              (progn
                ;; Create updated entry with user inputs (only if not nil)
                (setq new_entry
                  (list (nth 0 entry)  ; id - keep original
                        (nth 1 entry)  ; sheet/drawing group - keep original
                        (nth 2 entry)  ; section code - keep original
                        (nth 3 entry)  ; drawing path - keep original
                        (if desc1 desc1 (nth 4 entry))          ; Description Line 1
                        (nth 5 entry)  ; subsection code - keep original
                        (if desc2 desc2 (nth 6 entry))          ; Description Line 2
                        (if desc3 desc3 (nth 7 entry))          ; Description Line 3
                        (nth 8 entry)  ; subfolder name - keep original
                  ))
                
                (setq new_d2_lst (append new_d2_lst (list new_entry)))
                (setq updated_count (1+ updated_count))
                
                ;; Show what was updated
                (princ (strcat "\nUpdated: " (nth 3 entry)))
                (if desc1 (princ (strcat "\n  Description 1: " desc1)))
                (if desc2 (princ (strcat "\n  Description 2: " desc2)))
                (if desc3 (princ (strcat "\n  Description 3: " desc3)))
              )
              ;; Keep original entry unchanged
              (setq new_d2_lst (append new_d2_lst (list entry)))
            )
            (setq i (1+ i))
          )
          
          ;; Only write if something was actually changed
          (if (> updated_count 0)
            (progn
              ;; Write the updated WDP file
              (c:wd_proj_wdp_write wdpfnam t_lst a_lst p_lst new_d2_lst)
              
              (princ (strcat "\nWDP file updated successfully. " 
                            (itoa updated_count) 
                            " drawing descriptions updated."))
            )
            (princ "\nNo changes made to any drawings.")
          )
        )
        (princ "\nNo drawings selected.")
      )
    )
    (progn
      (princ "\nError: Could not read current project data.")
      (princ "\nMake sure an AutoCAD Electrical project is active.")
    )
  )
  (princ)
)


(defun c:wdp_update_sections ( / wdp_data_lst d_lst selected_dwgs selected_files
                               sec subsec updated_count dwg_path desc1 desc2 desc3
                               current_entry desc_list subfolder)
  ;; Prompt for values (press Enter for empty/default)
  (princ "\nEnter values (press Enter for empty/default):")
  
  ;; Get input values
  (setq sec (strip_quotes (getstring T "\nSection code (e.g. 'CTRL'): ")))
  (setq subsec (strip_quotes (getstring T "\nSubsection code (e.g. 'MAIN'): ")))
  
  ;; Read existing WDP file data first
  (setq wdp_data_lst (c:wd_proj_wdp_data))
  
  (if wdp_data_lst
    (progn
      ;; Extract the drawing list for selection dialog
      (setq d_lst (nth 6 wdp_data_lst)) ; drawing list with sec/subsec data
      
      ;; Let user select which drawings to update
      (setq selected_dwgs (c:wd_pdwgs d_lst 
                                     "Select Drawings to Update Section/Subsection" 
                                     "Choose drawings to update section and subsection codes" 
                                     nil)) ; don't exclude ref-only drawings
      
      (if selected_dwgs
        (progn
          ;; Get the selected drawing file names
          (setq selected_files (nth 0 selected_dwgs))
          
          ;; Check if user entered at least one value
          (if (or (/= sec "") (/= subsec ""))
            (progn
              (setq updated_count 0)
              
              ;; Process each selected drawing
              (foreach dwg_path selected_files
                ;; Convert backslashes to forward slashes
                (setq dwg_path (vl-string-translate "\\" "/" dwg_path))
                
                (if (findfile dwg_path)
                  (progn
                    ;; Get current description lines from the drawing entry
                    ;; Find the entry in d_lst that matches this drawing path
                    (setq current_entry nil)
                    (foreach entry d_lst
                      (if (and (listp entry) 
                               (>= (length entry) 9)
                               (= (vl-string-translate "\\" "/" (nth 3 entry)) dwg_path))
                        (setq current_entry entry)
                      )
                    )
                    
                    (if current_entry
                      (progn
                        ;; Extract current descriptions to preserve them
                        (setq desc1 (nth 4 current_entry))
                        (setq desc2 (nth 6 current_entry))
                        (setq desc3 (nth 7 current_entry))
                        
                        ;; Extract current subfolder to preserve it
                        (setq subfolder (nth 8 current_entry))
                        (if (not subfolder) (setq subfolder ""))
                        
                        ;; Create description list (handle nil values)
                        (setq desc_list (list 
                                         (if desc1 desc1 "")
                                         (if desc2 desc2 "")
                                         (if desc3 desc3 "")))
                        
                        ;; Update project properties using the API with subfolder preservation
                        (c:ace_add_dwg_to_project 
                          dwg_path
                          (list 
                            sec                    ; Section code
                            subsec                 ; Subsection code
                            desc_list              ; Description lines (preserved)
                            1                      ; Suppress project file freshen
                            subfolder              ; Preserve subfolder location
                          )
                        )
                        
                        (setq updated_count (1+ updated_count))
                        (princ (strcat "\nDrawing " 
                                      (cadr (c:wd_split_fnam dwg_path)) 
                                      " updated in project successfully."))
                        (if (/= sec "") (princ (strcat "\n  Section: " sec)))
                        (if (/= subsec "") (princ (strcat "\n  Subsection: " subsec)))
                        (if (/= subfolder "") (princ (strcat "\n  Subfolder: " subfolder)))
                      )
                      (princ (strcat "\nWarning: Could not find entry for " dwg_path))
                    )
                  )
                  (princ (strcat "\nDrawing not found at: " dwg_path))
                )
              )
              
              (princ (strcat "\nSection/Subsection update completed. " 
                            (itoa updated_count) 
                            " drawings updated."))
            )
            (princ "\nNo section or subsection codes entered.")
          )
        )
        (princ "\nNo drawings selected.")
      )
    )
    (progn
      (princ "\nError: Could not read current project data.")
      (princ "\nMake sure an AutoCAD Electrical project is active.")
    )
  )
  (princ)
)
(defun c:GetEntityData (/ entName entData)
  ;; Prompt the user to select an entity
  (setq entName (car (entsel "\nSelect an entity: ")))

  ;; Check if an entity was selected
  (if entName
    (progn
      ;; Get the entity data list using entget
      (setq entData (entget entName))

      ;; Display the entity data
      (princ "\nEntity Data:\n")
      (foreach item entData
        (princ (strcat (vl-princ-to-string item) "\n"))
      )
    )
    (princ "\nNo entity selected.")
  )
  (princ)
)

(defun c:ListBlockProperties (/ blkEnt blkName blkObj dynProps attProps attList propName propValue)
  ;; Helper function to get property value, handling VARIANTs
  (defun GetVariantValue (val)
    (cond
      ((= (type val) 'VARIANT) (vlax-variant-value val))  ;; Extract the value if it's a VARIANT
      (T val)  ;; Otherwise, return the value as is
    )
  )

  ;; Prompt the user to select a block
  (setq blkEnt (car (entsel "\nSelect a block: ")))

  ;; Check if the selected entity is a block reference
  (if (and blkEnt
           (= (cdr (assoc 0 (entget blkEnt))) "INSERT"))
    (progn
      ;; Get the block name
      (setq blkName (cdr (assoc 2 (entget blkEnt))))
      (princ (strcat "\nBlock Name: " blkName "\n"))

      ;; Convert the block entity to a VLA object
      (setq blkObj (vlax-ename->vla-object blkEnt))

      ;; Get dynamic properties if available
      (setq dynProps (vlax-invoke blkObj 'GetDynamicBlockProperties))

      ;; Loop through and display dynamic properties
      (if dynProps
        (progn
          (princ "\nDynamic Block Properties:\n")
          (foreach prop dynProps
            (setq propName (vla-get-propertyname prop))
            (setq propValue (GetVariantValue (vla-get-value prop)))
            (princ (strcat propName ": " (vl-princ-to-string propValue) "\n"))
          )
        )
        (princ "\nNo Dynamic Properties Found.\n")
      )

      ;; Get attribute properties if available
      (setq attList (vlax-invoke blkObj 'GetAttributes))
      (if attList
        (progn
          (princ "\nBlock Attribute Properties:\n")
          (foreach attObj attList
            (setq propName (vla-get-tagstring attObj))
            (setq propValue (vla-get-textstring attObj)) ;; No need for variant handling in attributes
            (princ (strcat propName ": " (vl-princ-to-string propValue) "\n"))
          )
        )
        (princ "\nNo Attribute Properties Found.\n")
      )
      
      ;; List basic properties using entget
      (princ "\nBasic Block Properties:\n")
      (setq blkData (entget blkEnt))
      (foreach item blkData
        (princ (strcat (vl-princ-to-string item) "\n"))
      )
    )
    (princ "\nSelected entity is not a block.")
  )
  (princ)
)

(defun c:getPnlCat ( / x xx cat_rec ben blknam mfg cat assycode ix)
  ; Utility to print all catalog values for a picked component
  (if (setq x (entsel "\nPick component:"))  ; Changed while to if
    (progn
      (setq ben (car x))
      ; get "CAT" attribute or xdata value  
      (if (AND (setq cat (c:wd_get_pnlval ben "CAT"))
               (/= cat ""))
        (progn
          (setq mfg (c:wd_get_pnlval ben "MFG"))
          (setq assycode (c:wd_get_pnlval ben "ASSYCODE"))
          (setq blknam (c:wd_get_pnlval ben "WDBLKNAM"))
          (if (OR (not blknam)(= blknam ""))
            (progn ; get actual block name
              (setq blknam (cdr (assoc 2 (entget ben))))
          ))
          
          ; Do catalog lookup on the target MFG/CAT/ASSYCODE combo
          (setq x (c:ace_get_cat_data nil mfg cat assycode blknam))
          
          (if x ; some data returned
            (progn
              (setq cat_rec (car x)) ; get first record
              (princ "\nCatalog record values for component:")
              (princ (strcat "\nMFG: " mfg))
              (princ (strcat "\nCAT: " cat))
              (princ (strcat "\nASSYCODE: " assycode))
              (princ "\n---------------------------------")
              
              ; Print all values with their nth position
              (setq ix 0)
              (while (< ix (length cat_rec))
                (princ (strcat "\nnth " (itoa ix) ": " (vl-princ-to-string (nth ix cat_rec))))
                (setq ix (1+ ix))
              )
              (princ "\n---------------------------------")
            )
            (princ "\nNo catalog data found for this component.")
          )
        )
        (princ "\nNo CAT attribute/value found on selected component.")
      )
    )
  )
  (princ)
)

(defun c:getSchCat ( / x xx cat_rec ben blknam mfg cat assycode ix)
  ; Utility to print all catalog values for a picked schematic component
  (if (setq x (entsel "\nPick schematic component:"))
    (progn
      (setq ben (car x))
      ; get "CAT" attribute value  
      (if (AND (setq cat (c:wd_getattrval ben "CAT"))
               (/= cat ""))
        (progn
          (setq mfg (c:wd_getattrval ben "MFG"))
          (setq assycode (c:wd_getattrval ben "ASSYCODE"))
          (setq blknam (c:wd_getattrval ben "WDBLKNAM"))
          (if (OR (not blknam)(= blknam ""))
            (progn ; get actual block name
              (setq blknam (cdr (assoc 2 (entget ben))))
          ))
          
          ; Do catalog lookup on the target MFG/CAT/ASSYCODE combo
          (setq x (c:ace_get_cat_data nil mfg cat assycode blknam))
          
          (if x ; some data returned
            (progn
              (setq cat_rec (car x)) ; get first record
              (princ "\nCatalog record values for schematic component:")
              (princ (strcat "\nMFG: " mfg))
              (princ (strcat "\nCAT: " cat))
              (princ (strcat "\nASSYCODE: " assycode))
              (princ (strcat "\nTAG: " (c:wd_getattrval ben "TAG1,TAG1F,TAG2")))
              (princ "\n---------------------------------")
              
              ; Print all values with their nth position
              (setq ix 0)
              (while (< ix (length cat_rec))
                (princ (strcat "\nnth " (itoa ix) ": " (vl-princ-to-string (nth ix cat_rec))))
                (setq ix (1+ ix))
              )
              (princ "\n---------------------------------")
            )
            (princ "\nNo catalog data found for this component.")
          )
        )
        (princ "\nNo CAT attribute found on selected component.")
      )
    )
  )
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adjust the CONFIG value using a lookup table.
(defun modAdjustConfig (config / fixTable newConfig)
  (setq fixTable '(("RR" . "LR")
                   ("LL" . "LR")))
  (setq newConfig config)
  (foreach pair fixTable
    (setq newConfig (vl-string-subst (cdr pair) (car pair) newConfig))
  )
  newConfig
)

;; Toggle the letters R and L within a CONFIG string using a lookup list.
(defun modToggleConfigValue (config / i len char result toggleList)
  (setq toggleList '(("R" . "L") ("L" . "R")))
  (setq result "")
  (setq len (strlen config))
  (setq i 1)
  (while (<= i len)
    (setq char (substr config i 1))
    (setq result (strcat result (if (cdr (assoc char toggleList))
                                    (cdr (assoc char toggleList))
                                    char)))
    (setq i (1+ i))
  )
  result
)

;; Retrieve the allowed values for the dynamic property "Visibility1"
(defun modGetVisibilityAllowedValues (ent / blkObj dynProps prop allowedValues)
  (setq blkObj (vlax-ename->vla-object ent))
  (setq dynProps (vlax-invoke blkObj 'GetDynamicBlockProperties))
  (setq allowedValues nil)
  (if dynProps
    (foreach prop dynProps
      (if (equal (vla-get-propertyname prop) "Visibility1")
        (progn
          (setq allowedValues (mapcar 'modGetVariantValue 
                                        (vlax-safearray->list (vlax-variant-value (vla-get-allowedvalues prop)))))
        )
      )
    )
  )
  allowedValues
)


;; Retrieve catalog records from MFG/CAT attribute pairs.
(defun modGetCatalogRecords (ent / mfg1 cat1 mfg2 cat2 mfg3 cat3 rec1 rec2 rec3 blkName)
  (setq mfg1 (c:wd_get_pnlval ent "MFG01"))
  (setq cat1 (c:wd_get_pnlval ent "CAT01"))
  (setq mfg2 (c:wd_get_pnlval ent "MFG02"))
  (setq cat2 (c:wd_get_pnlval ent "CAT02"))
  (setq mfg3 (c:wd_get_pnlval ent "MFG03"))
  (setq cat3 (c:wd_get_pnlval ent "CAT03"))
  (setq blkName (c:wd_get_pnlval ent "WDBLKNAM"))
  (if (or (not blkName) (= blkName ""))
    (setq blkName (cdr (assoc 2 (entget ent))))
  )
  ;; If blkName has more than two characters, take only the first two.
  (if (> (strlen blkName) 2)
    (setq blkName (substr blkName 1 2))
  )
  (if (and mfg1 cat1 (/= mfg1 "") (/= cat1 ""))
      (setq rec1 (car (c:ace_get_cat_data nil mfg1 cat1 "" blkName)))
      (setq rec1 nil)
  )
  (if (and mfg2 cat2 (/= mfg2 "") (/= cat2 ""))
      (setq rec2 (car (c:ace_get_cat_data nil mfg2 cat2 "" blkName)))
      (setq rec2 nil)
  )
  (if (and mfg3 cat3 (/= mfg3 "") (/= cat3 ""))
      (setq rec3 (car (c:ace_get_cat_data nil mfg3 cat3 "" blkName)))
      (setq rec3 nil)
  )
  (list rec1 rec2 rec3)
)

;; Compute the new CONFIG value from catalog records.
;; It extracts the nth element (index 8) from each valid catalog record if it starts with "AUX".
;; If no AUX values are found, "NIL" is returned.
;; If one is found, that value is used.
;; If multiple are found, the AUX parts (after the prefix) are sorted and reassembled.
(defun modComputeAuxConfig (catList / auxList auxValue finalConfig strippedList)
  (setq auxList '())
  (foreach rec catList
    (if rec
      (progn
        (setq auxValue (nth 8 rec))
        (if (and auxValue (/= auxValue "")
                 (= (substr auxValue 1 3) "AUX"))
          (setq auxList (cons auxValue auxList))
        )
      )
    )
  )
  (setq auxList (vl-remove "" auxList))  ;; Remove any empty strings.
  (cond
    ((null auxList)
     (setq finalConfig "NIL"))
    ((= (length auxList) 1)
     (setq finalConfig (car auxList)))
    (T
     (setq strippedList (mapcar '(lambda (s) (substr s 4)) auxList))
     (setq strippedList (vl-sort strippedList '<))
     (setq finalConfig (strcat "AUX" (apply 'strcat strippedList)))
    )
  )
  finalConfig
)

;; Update the CONFIG attribute on the block.
(defun modSetConfigAttribute (ent newConfig)
  (c:wd_modattrval ent "CONFIG" newConfig nil)
)

;; Helper to convert VARIANT values to plain values
(defun modGetVariantValue (val)
  (if (= (type val) 'VARIANT)
      (vlax-variant-value val)
      val))

(defun modSetVisibilityProperty (ent config / blkObj dynProps prop validValues found)
  (setq blkObj (vlax-ename->vla-object ent))
  (setq dynProps (vlax-invoke blkObj 'GetDynamicBlockProperties))
  (setq found nil)
  (if dynProps
    (foreach prop dynProps
      (if (equal (vla-get-propertyname prop) "Visibility1")
        (progn
          (setq validValues (mapcar 'modGetVariantValue 
                                    (vlax-safearray->list (vlax-variant-value (vla-get-allowedvalues prop)))))
          (if (member config validValues)
            (progn
              (vla-put-value prop config)
              (setq found T)
            )
          )
        )
      )
    )
  )
  found
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Single-Block Update Function
(defun c:UpdateBlockConfig ( / blkEnt catRecords newCfg fixedCfg updResult)
  (setq blkEnt (car (entsel "\nSelect a block for update: ")))
  (if (and blkEnt
           (= (cdr (assoc 0 (entget blkEnt))) "INSERT"))
    (progn
      (setq catRecords (modGetCatalogRecords blkEnt))
      (setq newCfg (modComputeAuxConfig catRecords))
      (setq fixedCfg (modAdjustConfig newCfg))
      (if (modSetConfigAttribute blkEnt fixedCfg)
        (princ (strcat "\nCONFIG updated to: " fixedCfg))
        (princ "\nFailed to update CONFIG attribute.")
      )
      (if (modSetVisibilityProperty blkEnt fixedCfg)
        (princ (strcat "\nVisibility1 updated to: " fixedCfg))
        (princ "\nVisibility1 property not found or update failed.")
      )
    )
    (princ "\nSelected entity is not a valid block reference.")
  )
  (princ)
)

;; 2. Drawing-Wide Update Function
(defun c:UpdateAllBlockConfigs ( / ss cnt i blkEnt catRecords newCfg fixedCfg updResult cntNoConfig)
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (if ss
    (progn
      (setq cnt 0)
      (setq cntNoConfig 0)
      (setq i 0)
      (while (< i (sslength ss))
        (setq blkEnt (ssname ss i))
        (setq catRecords (modGetCatalogRecords blkEnt))
        (setq newCfg (modComputeAuxConfig catRecords))
        (setq fixedCfg (modAdjustConfig newCfg))
        (if (modSetConfigAttribute blkEnt fixedCfg)
          (setq cnt (1+ cnt))
          (setq cntNoConfig (1+ cntNoConfig))
        )
        (modSetVisibilityProperty blkEnt fixedCfg)
        (setq i (1+ i))
      )
      (princ (strcat "\nDrawing update complete."
                     "\nBlocks updated: " (itoa cnt)
                     "\nBlocks without CONFIG attribute: " (itoa cntNoConfig)))
    )
    (princ "\nNo block references found in the drawing.")
  )
  (princ)
)

;; 3. Toggle Function (Single-Block Toggle) with equality check
(defun c:ToggleBlockConfig ( / blkEnt curCfg toggledCfg allowedValues)
  (setq blkEnt (car (entsel "\nSelect a block for toggle: ")))
  (if (and blkEnt
           (= (cdr (assoc 0 (entget blkEnt))) "INSERT"))
    (progn
      (setq curCfg (c:wd_get_pnlval blkEnt "CONFIG"))
      (if (and curCfg (/= curCfg ""))
        (progn
          (setq toggledCfg (modToggleConfigValue curCfg))
          
          ;; Check if current and toggled configs are equal
          (if (= (strcase curCfg) (strcase toggledCfg))
            (princ (strcat "\nConfiguration already set to: " curCfg ". No change needed."))
            (progn
              ;; Get allowed values for Visibility1
              (setq allowedValues (modGetVisibilityAllowedValues blkEnt))
              (if (and allowedValues (member toggledCfg allowedValues))
                (progn
                  (if (modSetConfigAttribute blkEnt toggledCfg)
                    (princ (strcat "\nCONFIG toggled to: " toggledCfg))
                    (princ "\nFailed to update CONFIG attribute.")
                  )
                  (if (modSetVisibilityProperty blkEnt toggledCfg)
                    (princ (strcat "\nVisibility1 updated to: " toggledCfg))
                    (princ "\nVisibility1 property update failed.")
                  )
                )
                (princ "\nToggled config value not allowed in Visibility1 property. No update performed.")
              )
            )
          )
        )
        (princ "\nCONFIG attribute not found or empty.")
      )
    )
    (princ "\nSelected entity is not a valid block reference.")
  )
  (princ)
)

(defun c:ResetDrawingConfig ( / ss i blkEnt curConfig updatedCount)
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  (setq updatedCount 0)
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq blkEnt (ssname ss i))
        (setq curConfig (c:wd_get_pnlval blkEnt "CONFIG"))
        (if (and curConfig (/= curConfig ""))
          (progn
            ;; Update CONFIG attribute to "NIL"
            (if (modSetConfigAttribute blkEnt "")
              (setq updatedCount (1+ updatedCount))
            )
            ;; Update the dynamic property Visibility1 to "NIL"
            (modSetVisibilityProperty blkEnt "NIL")
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\nReset complete. Blocks updated: " (itoa updatedCount)))
    )
    (princ "\nNo block references found in the drawing.")
  )
  (princ)
)

(defun c:select-drawings-for-export ( / proj_data d_lst selected_drawings selected_indices partial_lst)
  ;; Function to select drawings and create partial_lst for c:wd_2ss
  
  ;; Make sure AutoCAD Electrical is loaded
  (if (not wd_load)
    (if (setq x (findfile "wd_load.lsp"))
      (load x)
    )
  )
  (if wd_load (wd_load)) ;; Wake up AutoCAD Electrical
  
  ;; Get the current project data
  (setq proj_data (c:wd_proj_wdp_data))
  
  (if proj_data
    (progn
      ;; Extract the drawing list (nth 6 contains dwg names with sec/subsec data)
      (setq d_lst (nth 6 proj_data))
      
      ;; Use wd_pdwgs to let user select drawings
      (setq selected_drawings (c:wd_pdwgs 
                                d_lst 
                                "Select Drawings for Export" 
                                "Choose drawings to include in the data export" 
                                nil)) ;; nil = don't exclude ref-only drawings
      
      (if selected_drawings
        (progn
          ;; selected_drawings returns (list drawing_names index_numbers)
          ;; We need to convert the drawing position indices to DWGIX values
          (setq selected_indices (cadr selected_drawings)) ;; Get the index list
          
          ;; Convert drawing positions to DWGIX values using wd_mdb_wdp2ix_ix
          ;; and convert to strings as required by wd_2ss
          (setq partial_lst nil)
          (foreach pos_index selected_indices
            ;; Convert 0-based position to 1-based, then get DWGIX
            (setq dwgix (wd_mdb_wdp2ix_ix (1+ pos_index)))
            (if (and dwgix (not (member dwgix partial_lst)))
              (setq partial_lst (cons dwgix partial_lst))
            )
          )
          ;; Reverse the list to maintain original order
          (setq partial_lst (reverse partial_lst))
          
          ;; Display what was selected
          (princ "\nSelected drawings indices: ")
          (princ partial_lst)
          (princ "\nSelected drawing count: ")
          (princ (length partial_lst))
          
          ;; Store the partial_lst globally for use
          (setq GBL_export_param_lst partial_lst)
          
          (princ "\nParam list created and stored in partial_lst")
          
          ;; Return the param_lst
          partial_lst
        )
        (progn
          (princ "\nNo drawings selected.")
          nil
        )
      )
    )
    (progn
      (princ "\nNo active project found.")
      nil
    )
  )
)

;; Main export function - select drawings and export schematic components to CSV
(defun c:export-selected-components ( / output_file param_lst partial_lst proj_name temp_folder proj_data)
  ;; Export schematic components from selected drawings to CSV with automatic naming
  
  ;; First select the drawings
  (if (setq partial_lst (c:select-drawings-for-export))
    (progn
      ;; Get current project data
      (setq proj_data (c:wd_proj_wdp_data))
      
      ;; Extract project name from project file path
      (setq proj_name (cadr (c:wd_split_fnam (car proj_data))))
      
      ;; Get temp folder location
      (setq temp_folder (getvar "TEMPPREFIX"))
      
      ;; Create output filename: ProjectName-CompList.csv
      (setq output_file (strcat temp_folder proj_name "-CompList.csv"))
      
      ;; Create param_lst specifically for schematic components CSV export
      (setq param_lst (list 
                        "comp"               ;; dumpwhat - schematic components
                        2                    ;; doall = 2 for partial list
                        partial_lst          ;; Keep the partial_lst we created
                        1                    ;; todolocs - all components
                        nil                  ;; doloc - all locations
                        "CSV"                ;; format - CSV
                        output_file          ;; output filename
                        nil))                ;; no script file
      
      ;; Run the export
      (princ "\nExporting schematic components to CSV...")
      (princ (strcat "\nOutput file: " output_file))
      (c:wd_2ss param_lst)
      (princ "\nSchematic components export completed!")
    )
    (princ "\nFailed to select drawings.")
  )
  (princ)
)

;; New routine - export one-line components only
(defun c:export-selected-oneline ( / output_file temp_file param_lst partial_lst proj_name temp_folder proj_data csv_content filtered_content lines header_line data_lines filtered_lines wdtype_index)
  ;; Export one-line components (WDTYPE = "1-") from selected drawings to CSV
  
  ;; First select the drawings
  (if (setq partial_lst (c:select-drawings-for-export))
    (progn
      ;; Get current project data
      (setq proj_data (c:wd_proj_wdp_data))
      
      ;; Extract project name from project file path
      (setq proj_name (cadr (c:wd_split_fnam (car proj_data))))
      
      ;; Get temp folder location
      (setq temp_folder (getvar "TEMPPREFIX"))
      
      ;; Create temp filename: ProjectName-OneLineList_TEMP.csv
      (setq temp_file (strcat temp_folder proj_name "-OneLineList_TEMP.csv"))
      
      ;; Create final filename: ProjectName-OneLineList.csv
      (setq output_file (strcat temp_folder proj_name "-OneLineList.csv"))
      
      ;; Create param_lst specifically for schematic components CSV export
      (setq param_lst (list 
                        "comp"               ;; dumpwhat - schematic components
                        2                    ;; doall = 2 for partial list
                        partial_lst          ;; Keep the partial_lst we created
                        1                    ;; todolocs - all components
                        nil                  ;; doloc - all locations
                        "CSV"                ;; format - CSV
                        temp_file            ;; temp filename
                        nil))                ;; no script file
      
      ;; Run the export to temp file
      (princ "\nExporting schematic components to temp CSV...")
      (princ (strcat "\nTemp file: " temp_file))
      (c:wd_2ss param_lst)
      
      ;; Now filter the CSV to keep only one-line components (WDTYPE = "1-")
      (princ "\nFiltering for one-line components...")
      
      ;; Read the temp CSV file
      (setq csv_file (open temp_file "r"))
      (if csv_file
        (progn
          ;; Read lines directly into a list
          (setq lines nil)
          (while (setq line (read-line csv_file))
            (setq lines (append lines (list line)))
          )
          (close csv_file)
          
          ;; Get header line (first line)
          (setq header_line (car lines))
          (setq data_lines (cdr lines))
          
          ;; Find WDTYPE column index in header
          (setq header_fields (c:wd_delim_str_to_lst header_line ","))
          (setq wdtype_index nil)
          (setq field_index 0)
          (foreach field header_fields
            (if (= (strcase field) "WDTYPE")
              (setq wdtype_index field_index)
            )
            (setq field_index (1+ field_index))
          )
          
          ;; Filter data lines - keep only rows where WDTYPE = "1-"
          (setq filtered_lines (list header_line))
          (if wdtype_index
            (foreach data_line data_lines
              (if (and data_line (/= data_line ""))
                (progn
                  (setq fields (c:wd_delim_str_to_lst data_line ","))
                  (if (< wdtype_index (length fields))
                    (progn
                      (setq wdtype_value (nth wdtype_index fields))
                      ;; Check for one-line components - WDTYPE = "1-"
                      (if (and wdtype_value (= (strcase wdtype_value) "1-"))
                        (setq filtered_lines (append filtered_lines (list data_line)))
                      )
                    )
                  )
                )
              )
            )
            ;; else clause - WDTYPE column not found
            (princ "\nWarning: WDTYPE column not found in CSV")
          )
          
          ;; Write filtered content to final file
          (setq csv_file (open output_file "w"))
          (if csv_file
            (progn
              (foreach line filtered_lines
                (write-line line csv_file)
              )
              (close csv_file)
              (princ (strcat "\nFiltered " (itoa (1- (length filtered_lines))) " one-line components"))
              (princ (strcat "\nFinal file: " output_file))
              
              ;; Delete temp file
              (vl-file-delete temp_file)
              (princ "\nTemp file deleted")
              (princ "\nOne-line components export completed!")
            )
            (princ "\nError: Could not write filtered CSV file")
          )
        )
        (princ "\nError: Could not read temp CSV file for filtering")
      )
    )
    (princ "\nFailed to select drawings.")
  )
  (princ)
)

;; ============================================================================
;; PANEL LAYOUT INSERT ROUTINES
;; Modular functions to create layout CSV files and insert panel footprints
;; ============================================================================

;; Helper function to convert component CSV to layout CSV format
(defun convert-comp-to-lout-csv (input_file output_file / csv_file lines header_line data_lines 
                                 header_fields tagname_idx inst_idx loc_idx mfg_idx cat_idx 
                                 assycode_idx cnt_idx um_idx desc1_idx desc2_idx desc3_idx block_idx
                                 rating_indices item_idx par1_idx hdl_idx converted_lines)
  "Convert component CSV to layout CSV format with proper column arrangement"
  
  ;; Read the input CSV file
  (setq csv_file (open input_file "r"))
  (if csv_file
    (progn
      ;; Read all lines
      (setq lines nil)
      (while (setq line (read-line csv_file))
        (setq lines (append lines (list line)))
      )
      (close csv_file)
      
      (if lines
        (progn
          ;; Get header and data
          (setq header_line (car lines))
          (setq data_lines (cdr lines))
          (setq header_fields (c:wd_delim_str_to_lst header_line ","))
          
          ;; Find column indices
          (setq tagname_idx (find-column-index header_fields "TAGNAME"))
          (setq inst_idx (find-column-index header_fields "INST"))
          (setq loc_idx (find-column-index header_fields "LOC"))
          (setq mfg_idx (find-column-index header_fields "MFG"))
          (setq cat_idx (find-column-index header_fields "CAT"))
          (setq assycode_idx (find-column-index header_fields "ASSYCODE"))
          (setq cnt_idx (find-column-index header_fields "CNT"))
          (setq um_idx (find-column-index header_fields "UM"))
          (setq desc1_idx (find-column-index header_fields "DESC1"))
          (setq desc2_idx (find-column-index header_fields "DESC2"))
          (setq desc3_idx (find-column-index header_fields "DESC3"))
          (setq block_idx (find-column-index header_fields "BLOCK"))
          (setq item_idx (find-column-index header_fields "ITEM"))
          (setq par1_idx (find-column-index header_fields "(PAR1_CHLD2)"))
          (setq hdl_idx (find-column-index header_fields "(HDL)"))
          
          ;; Find RATING indices (RATING1-RATING12)
          (setq rating_indices (list))
          (setq rating_num 1)
          (while (<= rating_num 12)
            (setq rating_col (strcat "RATING" (itoa rating_num)))
            (setq rating_idx (find-column-index header_fields rating_col))
            (setq rating_indices (append rating_indices (list rating_idx)))
            (setq rating_num (1+ rating_num))
          )
          
          ;; Convert each data line to layout format
          (setq converted_lines nil)
          (foreach data_line data_lines
            (if (and data_line (/= data_line ""))
              (progn
                (setq fields (c:wd_delim_str_to_lst data_line ","))
                (setq converted_line (convert-row-to-lout-format 
                                       fields 
                                       tagname_idx inst_idx loc_idx mfg_idx cat_idx 
                                       assycode_idx cnt_idx um_idx desc1_idx desc2_idx desc3_idx 
                                       block_idx rating_indices item_idx par1_idx hdl_idx))
                (setq converted_lines (append converted_lines (list converted_line)))
              )
            )
          )
          
          ;; Write converted lines to output file (no header for .lout files)
          (setq csv_file (open output_file "w"))
          (if csv_file
            (progn
              (foreach line converted_lines
                (write-line line csv_file)
              )
              (close csv_file)
              (princ (strcat "\nConverted " (itoa (length converted_lines)) " components to layout format"))
              (princ (strcat "\nLayout file: " output_file))
              T ;; Success
            )
            (progn
              (princ "\nError: Could not write layout CSV file")
              nil ;; Failure
            )
          )
        )
        (progn
          (princ "\nError: Input CSV file is empty")
          nil
        )
      )
    )
    (progn
      (princ "\nError: Could not read input CSV file")
      nil
    )
  )
)

;; Helper function to find column index by name
(defun find-column-index (header_fields column_name / index found_index)
  "Find the index of a column by name (case insensitive)"
  (setq index 0)
  (setq found_index nil)
  (foreach field header_fields
    (if (= (strcase field) (strcase column_name))
      (setq found_index index)
    )
    (setq index (1+ index))
  )
  found_index
)

;; Helper function to convert a row to layout format
(defun convert-row-to-lout-format (fields tagname_idx inst_idx loc_idx mfg_idx cat_idx 
                                   assycode_idx cnt_idx um_idx desc1_idx desc2_idx desc3_idx 
                                   block_idx rating_indices item_idx par1_idx hdl_idx / 
                                   tagname inst loc mfg cat assycode cnt um desc1 desc2 desc3 
                                   block item par1 hdl rating_values converted_row)
  "Convert a single row to layout CSV format"
  
  ;; Extract values safely
  (setq tagname (safe-nth tagname_idx fields ""))
  (setq inst (safe-nth inst_idx fields ""))
  (setq loc (safe-nth loc_idx fields ""))
  (setq mfg (safe-nth mfg_idx fields ""))
  (setq cat (safe-nth cat_idx fields ""))
  (setq assycode (safe-nth assycode_idx fields ""))
  (setq cnt (safe-nth cnt_idx fields "1"))
  (setq um (safe-nth um_idx fields ""))
  (setq desc1 (safe-nth desc1_idx fields ""))
  (setq desc2 (safe-nth desc2_idx fields ""))
  (setq desc3 (safe-nth desc3_idx fields ""))
  (setq block (safe-nth block_idx fields ""))
  (setq item (safe-nth item_idx fields ""))
  (setq par1 (safe-nth par1_idx fields ""))
  (setq hdl (safe-nth hdl_idx fields ""))
  
  ;; Extract RATING values
  (setq rating_values nil)
  (foreach rating_idx rating_indices
    (setq rating_val (safe-nth rating_idx fields ""))
    (setq rating_values (append rating_values (list rating_val)))
  )
  
  ;; Build the layout row according to specified format:
  ;; TAGNAME	INST	LOC	BLANK	BLANK	MFG	CAT	ASSYCODE	CNT	UM	DESC1	DESC2	DESC3	BLOCK	RATING1-12	ITEM	BLANK	BLANK	BLANK	(PAR1_CHLD2)	(HDL)
  (setq converted_row (strcat
    tagname ","
    inst ","
    loc ","
    "," ;; BLANK
    "," ;; BLANK
    mfg ","
    cat ","
    assycode ","
    cnt ","
    um ","
    desc1 ","
    desc2 ","
    desc3 ","
    block ","
    (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) rating_values)) ;; RATING1-12
    item ","
    "," ;; BLANK
    "," ;; BLANK
    "," ;; BLANK
    par1 ","
    hdl
  ))
  
  converted_row
)

;; Helper function to safely get nth element
(defun safe-nth (index lst default_val)
  "Safely get nth element from list, return default if index out of bounds"
  (if (and index (< index (length lst)))
    (nth index lst)
    default_val
  )
)

;; Function to check if layout file exists and handle user choice
(defun check-and-handle-lout-file (lout_file / user_choice)
  "Check if layout file exists and handle user choice. Returns: 'create-new', 'use-existing', or 'cancel'"
  
  (if (findfile lout_file)
    (progn
      ;; File exists, ask user
      (prompt (strcat "\nLayout file already exists: " lout_file))
      (setq user_choice (show-yesno-dialog "Existing list found, Reload the list from beginning?" "Item List"))
      (if user_choice
        'create-new      ; Yes chosen
        'use-existing    ; No chosen
      )
    )
    (progn
        ;; File doesn't exist
        'create-new
    )
  )
)

;; Main function to insert selected components as panel footprints
(defun c:insert-selected-components ( / temp_comp_file lout_file proj_data proj_name temp_folder 
                                      file_choice conversion_success)
  "Insert panel footprints for selected schematic components"
  
  ;; Get current project data
  (setq proj_data (c:wd_proj_wdp_data))
  (if proj_data
    (progn
      ;; Extract project name and temp folder
      (setq proj_name (cadr (c:wd_split_fnam (car proj_data))))
      (setq temp_folder (getvar "TEMPPREFIX"))
      (setq lout_file (strcat temp_folder proj_name "-CompLout.csv"))
      
      ;; Check existing layout file and get user choice
      (setq file_choice (check-and-handle-lout-file lout_file))
      
      (cond
        ((= file_choice 'create-new)
         ;; Create new layout file
         (princ "\nCreating new layout file...")
         
         ;; First export components using existing routine
         (if (c:export-selected-components)
           (progn
             ;; Convert the component CSV to layout format
             (setq temp_comp_file (strcat temp_folder proj_name "-CompList.csv"))
             (setq conversion_success (convert-comp-to-lout-csv temp_comp_file lout_file))
             
             (if conversion_success
               (progn
                 ;; Clean up temp file
                 (if (findfile temp_comp_file)
                   (vl-file-delete temp_comp_file)
                 )
                 ;; Check if layout file has content
                 (if (check-lout-file-content lout_file)
                   (progn
                     ;; Insert panel footprints
                     (princ "\nInserting panel footprints...")
                     (c:wd_pnl_lout lout_file)
                     (princ "\nPanel footprint insertion completed!")
                   )
                   (progn
                     (alert "No components exist in the layout file!")
                     (princ "\nNo components to insert.")
                   )
                 )
               )
               (princ "\nError: Failed to convert component data to layout format")
             )
           )
           (princ "\nError: Failed to export component data")
         )
        )
        
        ((= file_choice 'use-existing)
         ;; Use existing layout file
         (princ "\nUsing existing layout file...")
         ;; Check if layout file has content
         (if (check-lout-file-content lout_file)
           (progn
             (c:wd_pnl_lout lout_file)
             (princ "\nPanel footprint insertion completed!")
           )
           (progn
             (alert "No components exist in the layout file!")
             (princ "\nNo components to insert.")
           )
         )
        )
        
        (T
         ;; User cancelled
         (princ "\nOperation cancelled by user")
        )
      )
    )
    (princ "\nError: No active project found")
  )
  (princ)
)

;; Main function to insert selected one-line components as panel footprints
(defun c:insert-selected-oneline ( / temp_comp_file lout_file proj_data proj_name temp_folder 
                                   file_choice conversion_success)
  "Insert panel footprints for selected one-line components"
  
  ;; Get current project data
  (setq proj_data (c:wd_proj_wdp_data))
  (if proj_data
    (progn
      ;; Extract project name and temp folder
      (setq proj_name (cadr (c:wd_split_fnam (car proj_data))))
      (setq temp_folder (getvar "TEMPPREFIX"))
      (setq lout_file (strcat temp_folder proj_name "-OneLineLout.csv"))
      
      ;; Check existing layout file and get user choice
      (setq file_choice (check-and-handle-lout-file lout_file))
      
      (cond
        ((= file_choice 'create-new)
         ;; Create new layout file
         (princ "\nCreating new one-line layout file...")
         
         ;; First export one-line components using existing routine
         (if (c:export-selected-oneline)
           (progn
             ;; Convert the component CSV to layout format
             (setq temp_comp_file (strcat temp_folder proj_name "-OneLineList.csv"))
             (setq conversion_success (convert-comp-to-lout-csv temp_comp_file lout_file))
             
             (if conversion_success
               (progn
                 ;; Clean up temp file
                 (if (findfile temp_comp_file)
                   (vl-file-delete temp_comp_file)
                 )
                 ;; Check if layout file has content
                 (if (check-lout-file-content lout_file)
                   (progn
                     ;; Insert panel footprints
                     (princ "\nInserting one-line panel footprints...")
                     (c:wd_pnl_lout lout_file)
                     (princ "\nOne-line panel footprint insertion completed!")
                   )
                   (progn
                     (alert "No One-Line components exist in the layout file!")
                     (princ "\nNo one-line components to insert.")
                   )
                 )
               )
               (princ "\nError: Failed to convert one-line data to layout format")
             )
           )
           (princ "\nError: Failed to export one-line component data")
         )
        )
        
        ((= file_choice 'use-existing)
         ;; Use existing layout file
         (princ "\nUsing existing one-line layout file...")
         ;; Check if layout file has content
         (if (check-lout-file-content lout_file)
           (progn
             (c:wd_pnl_lout lout_file)
             (princ "\nOne-line panel footprint insertion completed!")
           )
           (progn
             (alert "No One-Line components exist in the layout file!")
             (princ "\nNo one-line components to insert.")
           )
         )
        )
        
        (T
         ;; User cancelled
         (princ "\nOperation cancelled by user")
        )
      )
    )
    (princ "\nError: No active project found")
  )
  (princ)
)

;; Helper function to check if layout file has content
(defun check-lout-file-content (lout_file / csv_file line_count)
  "Check if layout file exists and has content (more than 0 lines). Returns T if has content, nil if empty"
  
  (if (findfile lout_file)
    (progn
      (setq csv_file (open lout_file "r"))
      (if csv_file
        (progn
          (setq line_count 0)
          ;; Count non-empty lines
          (while (setq line (read-line csv_file))
            (if (and line (/= (strcase (vl-string-trim " \t" line)) ""))
              (setq line_count (1+ line_count))
            )
          )
          (close csv_file)
          (> line_count 0) ;; Return T if has content, nil if empty
        )
        nil ;; Could not open file
      )
    )
    nil ;; File doesn't exist
  )
)

(defun c:test-lout-format ( / lout_file proj_data proj_name temp_folder csv_file line)
  "Test function to display layout file format"
  
  (setq proj_data (c:wd_proj_wdp_data))
  (if proj_data
    (progn
      (setq proj_name (cadr (c:wd_split_fnam (car proj_data))))
      (setq temp_folder (getvar "TEMPPREFIX"))
      (setq lout_file (strcat temp_folder proj_name "-CompLout.csv"))
      
      (if (setq csv_file (open lout_file "r"))
        (progn
          (princ "\nLayout file contents:")
          (princ "\n====================")
          (while (setq line (read-line csv_file))
            (princ "\n")
            (princ line)
          )
          (close csv_file)
        )
        (princ "\nLayout file not found")
      )
    )
  )
  (princ)
)
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

(defun c:UpdateConfigDisp (/ blkEnt blkObj dynProps attList propName propValue validValues configValue)
  ;; Helper function to get property value, handling VARIANTs
  (defun GetVariantValue (val)
    (if (= (type val) 'VARIANT)
      (vlax-variant-value val)  ;; If it's a VARIANT, extract the value
      val  ;; Otherwise, return the value as is
    )
  )

  ;; Helper function to convert SAFEARRAY to a list and extract VARIANT values
  (defun SafeArrayToList (sa)
    (mapcar 'GetVariantValue (vlax-safearray->list sa))  ;; Convert SAFEARRAY to a list and extract each VARIANT
  )

  ;; Prompt the user to select a block
  (setq blkEnt (car (entsel "\nSelect a block: ")))

  ;; Check if the selected entity is a block reference
  (if (and blkEnt
           (= (cdr (assoc 0 (entget blkEnt))) "INSERT"))
    (progn
      ;; Convert the block entity to a VLA object
      (setq blkObj (vlax-ename->vla-object blkEnt))

      ;; Initialize variables to track if properties are found
      (setq visibilityFound nil
            configFound nil)

      ;; Get dynamic properties if available
      (setq dynProps (vlax-invoke blkObj 'GetDynamicBlockProperties))

      ;; Loop through dynamic properties to find "Visibility1"
      (if dynProps
        (foreach prop dynProps
          (setq propName (vla-get-propertyname prop))
          (if (and (= propName "Visibility1") (not visibilityFound))
            (progn
              (setq visibilityFound T)
              ;; Get the list of valid values for Visibility1
              (setq validValues (SafeArrayToList (GetVariantValue (vla-get-allowedvalues prop))))
              ;; Store the property object for later use
              (setq visibilityProp prop)
            )
          )
        )
      )

      ;; Get attribute properties if available
      (setq attList (vlax-invoke blkObj 'GetAttributes))
      (if attList
        (foreach attObj attList
          (if (= (strcase (vla-get-tagstring attObj)) "CONFIG")
            (progn
              (setq configFound T)
              ;; Get the CONFIG attribute value
              (setq configValue (vla-get-textstring attObj))
            )
          )
        )
      )

      ;; Apply CONFIG value to Visibility1 if both are found and valid
      (if (and visibilityFound configFound)
        (if (member configValue validValues)
          (progn
            ;; Set the new value for Visibility1
            (vla-put-value visibilityProp configValue)
            (princ (strcat "\nVisibility1 value changed to: " configValue))
          )
          (princ "\nInvalid CONFIG value for Visibility1. No changes made.")
        )
        (princ "\nBlock does not have both Visibility1 and CONFIG or CONFIG is invalid.")
      )
    )
    (princ "\nSelected entity is not a block.")
  )
  (princ)
)


(defun c:AutoUpdateConfigDisp (/ blkEnt blkObj blkList dynProps attList propName propValue validValues configValue)
  ;; Helper function to get property value, handling VARIANTs
  (defun GetVariantValue (val)
    (if (= (type val) 'VARIANT)
      (vlax-variant-value val)  ;; If it's a VARIANT, extract the value
      val  ;; Otherwise, return the value as is
    )
  )

  ;; Helper function to convert SAFEARRAY to a list and extract VARIANT values
  (defun SafeArrayToList (sa)
    (mapcar 'GetVariantValue (vlax-safearray->list sa))  ;; Convert SAFEARRAY to a list and extract each VARIANT
  )

  ;; Loop through all blocks in the drawing
  (setq blkList (ssget "_X" '((0 . "INSERT"))))  ;; Select all block references

  (if blkList
    (progn
      (repeat (setq i (sslength blkList))
        (setq blkEnt (ssname blkList (setq i (1- i))))
        
        ;; Convert the block entity to a VLA object
        (setq blkObj (vlax-ename->vla-object blkEnt))

        ;; Initialize variables to track if properties are found
        (setq visibilityFound nil
              configFound nil)

        ;; Get dynamic properties if available
        (setq dynProps (vlax-invoke blkObj 'GetDynamicBlockProperties))

        ;; Loop through dynamic properties to find "Visibility1"
        (if dynProps
          (foreach prop dynProps
            (setq propName (vla-get-propertyname prop))
            (if (and (= propName "Visibility1") (not visibilityFound))
              (progn
                (setq visibilityFound T)
                ;; Get the list of valid values for Visibility1
                (setq validValues (SafeArrayToList (GetVariantValue (vla-get-allowedvalues prop))))
                ;; Store the property object for later use
                (setq visibilityProp prop)
              )
            )
          )
        )

        ;; Get attribute properties if available
        (setq attList (vlax-invoke blkObj 'GetAttributes))
        (if attList
          (foreach attObj attList
            (if (= (strcase (vla-get-tagstring attObj)) "CONFIG")
              (progn
                (setq configFound T)
                ;; Get the CONFIG attribute value
                (setq configValue (vla-get-textstring attObj))
              )
            )
          )
        )

        ;; Apply CONFIG value to Visibility1 if both are found and valid
        (if (and visibilityFound configFound)
          (if (member configValue validValues)
            (progn
              ;; Set the new value for Visibility1
              (vla-put-value visibilityProp configValue)
              (princ (strcat "\nVisibility1 value changed to: " configValue " for block: " (cdr (assoc 2 (entget blkEnt)))))
            )
            (princ "\nInvalid CONFIG value for Visibility1. No changes made.")
          )
        )
      )
    )
    (princ "\nNo blocks found in the drawing.")
  )
  (princ "\nAuto Update Display Configuration completed.")
  (princ)
)

(defun c:SetVisibility1 (/ blkEnt blkObj dynProps propName propValue validValues newValue)
  ;; Helper function to get property value, handling VARIANTs
  (defun GetVariantValue (val)
    (if (= (type val) 'VARIANT)
      (vlax-variant-value val)  ;; If it's a VARIANT, extract the value
      val  ;; Otherwise, return the value as is
    )
  )

  ;; Helper function to convert SAFEARRAY to a list and extract VARIANT values
  (defun SafeArrayToList (sa)
    (mapcar 'GetVariantValue (vlax-safearray->list sa))  ;; Convert SAFEARRAY to a list and extract each VARIANT
  )

  ;; Prompt the user to select a block
  (setq blkEnt (car (entsel "\nSelect a dynamic block: ")))

  ;; Check if the selected entity is a block reference
  (if (and blkEnt
           (= (cdr (assoc 0 (entget blkEnt))) "INSERT"))
    (progn
      ;; Convert the block entity to a VLA object
      (setq blkObj (vlax-ename->vla-object blkEnt))

      ;; Get dynamic properties if available
      (setq dynProps (vlax-invoke blkObj 'GetDynamicBlockProperties))

      ;; Loop through dynamic properties to find "Visibility1"
      (if dynProps
        (progn
          (setq propFound nil)
          (foreach prop dynProps
            (setq propName (vla-get-propertyname prop))
            (if (and (= propName "Visibility1") (not propFound))
              (progn
                (setq propFound T)
                ;; Get the current value of Visibility1
                (setq propValue (GetVariantValue (vla-get-value prop)))

                ;; Get the list of valid values for Visibility1
                (setq validValues (SafeArrayToList (GetVariantValue (vla-get-allowedvalues prop))))

                ;; Display the current value and list of valid values
                (princ (strcat "\nCurrent Visibility1 Value: " (vl-princ-to-string propValue)))
                (princ "\nValid Values: ")
                (foreach val validValues
                  (princ (strcat val " "))
                )

                ;; Prompt user to enter a new value
                (setq newValue (getstring "\nEnter new value for Visibility1: "))

                ;; Validate the user's input
                (if (member newValue validValues)
                  (progn
                    ;; Set the new value for Visibility1
                    (vla-put-value prop newValue)
                    (princ (strcat "\nVisibility1 value changed to: " newValue))
                  )
                  (princ "\nInvalid value entered. No changes made.")
                )
              )
            )
          )
          ;; If no Visibility1 property was found
          (if (not propFound)
            (princ "\nVisibility1 property not found.")
          )
        )
        (princ "\nNo dynamic properties found in the selected block.")
      )
    )
    (princ "\nSelected entity is not a block.")
  )
  (princ)
)

(defun c:processAuxConfig ( / x ben blknam mfg1 cat1 mfg2 cat2 mfg3 cat3 
                             query11 query12 query13 catrec1 catrec2 catrec3
                             aux_list final_config update_result)
  
  ; Function to extract query1 value if it starts with "AUX"
  (defun get-aux-value (query / aux_val)
    (if (and query 
             (/= query "")
             (= (substr query 1 3) "AUX"))
      query
      ""
    )
  )
  
  ; Function to strip "AUX" prefix and return remaining string
  (defun strip-aux (str)
    (if (and str (/= str ""))
      (substr str 4)
      ""
    )
  )

  ; Main routine
  (if (setq x (entsel "\nPick component:"))
    (progn
      (setq ben (car x))
      
      ; Get block name
      (setq blknam (c:wd_get_pnlval ben "WDBLKNAM"))
      (if (OR (not blknam)(= blknam ""))
        (setq blknam (cdr (assoc 2 (entget ben))))
      )
      
      ; Get MFG and CAT pairs
      (setq mfg1 (c:wd_get_pnlval ben "MFG01"))
      (setq cat1 (c:wd_get_pnlval ben "CAT01"))
      (setq mfg2 (c:wd_get_pnlval ben "MFG02"))
      (setq cat2 (c:wd_get_pnlval ben "CAT02"))
      (setq mfg3 (c:wd_get_pnlval ben "MFG03"))
      (setq cat3 (c:wd_get_pnlval ben "CAT03"))
      
      ; Get catalog data for each pair
      (if (and mfg1 cat1 (/= mfg1 "") (/= cat1 ""))
        (setq catrec1 (car (c:ace_get_cat_data nil mfg1 cat1 "" blknam)))
      )
      (if (and mfg2 cat2 (/= mfg2 "") (/= cat2 ""))
        (setq catrec2 (car (c:ace_get_cat_data nil mfg2 cat2 "" blknam)))
      )
      (if (and mfg3 cat3 (/= mfg3 "") (/= cat3 ""))
        (setq catrec3 (car (c:ace_get_cat_data nil mfg3 cat3 "" blknam)))
      )
      
      ; Extract query1 values (nth 8) from catalog records
      (setq query11 (if catrec1 (get-aux-value (nth 8 catrec1)) ""))
      (setq query12 (if catrec2 (get-aux-value (nth 8 catrec2)) ""))
      (setq query13 (if catrec3 (get-aux-value (nth 8 catrec3)) ""))
      
      ; Create list of non-empty AUX values
      (setq aux_list (vl-remove "" (list query11 query12 query13)))
      
      ; Process based on number of AUX values found
      (cond 
        ; No AUX values found - set CONFIG to "NIL"
        ((= (length aux_list) 0)
         (setq update_result (c:wd_modattrval ben "CONFIG" "NIL" nil))
         (if update_result
           (princ "\nNo valid AUX values found. CONFIG set to NIL.")
           (princ "\nNo CONFIG attribute found on component.")))
        
        ; Single AUX value - use as is
        ((= (length aux_list) 1)
         (setq final_config (car aux_list))
         (setq update_result (c:wd_modattrval ben "CONFIG" final_config nil))
         (if update_result
           (princ (strcat "\nUpdated CONFIG with single value: " final_config))
           (princ "\nNo CONFIG attribute found on component.")))
        
        ; Multiple AUX values - sort and combine
        (T
         (setq aux_list 
               (vl-sort 
                 (mapcar 'strip-aux aux_list)
                 '<))
         (setq final_config 
               (strcat "AUX" (apply 'strcat aux_list)))
         (setq update_result (c:wd_modattrval ben "CONFIG" final_config nil))
         (if update_result
           (princ (strcat "\nUpdated CONFIG with combined value: " final_config))
           (princ "\nNo CONFIG attribute found on component.")))
      )
    )
  )
  (princ)
)

(defun c:processAuxConfigAll ( / ss ix slen ben blknam mfg1 cat1 mfg2 cat2 mfg3 cat3 
                                query11 query12 query13 catrec1 catrec2 catrec3
                                aux_list final_config update_result blocks_processed
                                blocks_updated blocks_no_config)
  
  ; Function to extract query1 value if it starts with "AUX"
  (defun get-aux-value (query / aux_val)
    (if (and query 
             (/= query "")
             (= (substr query 1 3) "AUX"))
      query
      ""
    )
  )
  
  ; Function to strip "AUX" prefix and return remaining string
  (defun strip-aux (str)
    (if (and str (/= str ""))
      (substr str 4)
      ""
    )
  )

  ; Initialize counters
  (setq blocks_processed 0
        blocks_updated 0
        blocks_no_config 0)

  ; Get all block references in drawing
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  
  (if ss
    (progn
      (setq slen (sslength ss))
      (setq ix 0)
      
      (princ "\nProcessing blocks in drawing...")
      
      ; Process each block
      (while (< ix slen)
        (setq ben (ssname ss ix))
        (setq blocks_processed (1+ blocks_processed))
        
        ; Reset variables for each block
        (setq catrec1 nil catrec2 nil catrec3 nil
              query11 "" query12 "" query13 "")
        
        ; Get block name
        (setq blknam (c:wd_get_pnlval ben "WDBLKNAM"))
        (if (OR (not blknam)(= blknam ""))
          (setq blknam (cdr (assoc 2 (entget ben))))
        )
        
        ; Get MFG and CAT pairs
        (setq mfg1 (c:wd_get_pnlval ben "MFG01"))
        (setq cat1 (c:wd_get_pnlval ben "CAT01"))
        (setq mfg2 (c:wd_get_pnlval ben "MFG02"))
        (setq cat2 (c:wd_get_pnlval ben "CAT02"))
        (setq mfg3 (c:wd_get_pnlval ben "MFG03"))
        (setq cat3 (c:wd_get_pnlval ben "CAT03"))
        
        ; Get catalog data for each valid pair
        (if (and mfg1 cat1 (/= mfg1 "") (/= cat1 ""))
          (setq catrec1 (car (c:ace_get_cat_data nil mfg1 cat1 "" blknam)))
        )
        (if (and mfg2 cat2 (/= mfg2 "") (/= cat2 ""))
          (setq catrec2 (car (c:ace_get_cat_data nil mfg2 cat2 "" blknam)))
        )
        (if (and mfg3 cat3 (/= mfg3 "") (/= cat3 ""))
          (setq catrec3 (car (c:ace_get_cat_data nil mfg3 cat3 "" blknam)))
        )
        
        ; Extract query1 values (nth 8) from catalog records
        (setq query11 (if catrec1 (get-aux-value (nth 8 catrec1)) ""))
        (setq query12 (if catrec2 (get-aux-value (nth 8 catrec2)) ""))
        (setq query13 (if catrec3 (get-aux-value (nth 8 catrec3)) ""))
        
        ; Create list of non-empty AUX values
        (setq aux_list (vl-remove "" (list query11 query12 query13)))
        
        ; Process based on number of AUX values found
        (cond 
          ; No AUX values found - set CONFIG to "NIL"
          ((= (length aux_list) 0)
           (setq update_result (c:wd_modattrval ben "CONFIG" "NIL" nil)))
          
          ; Single AUX value - use as is
          ((= (length aux_list) 1)
           (setq final_config (car aux_list))
           (setq update_result (c:wd_modattrval ben "CONFIG" final_config nil)))
          
          ; Multiple AUX values - sort and combine
          (T
           (setq aux_list 
                 (vl-sort 
                   (mapcar 'strip-aux aux_list)
                   '<))
           (setq final_config 
                 (strcat "AUX" (apply 'strcat aux_list)))
           (setq update_result (c:wd_modattrval ben "CONFIG" final_config nil)))
        )
        
        ; Update counters based on result
        (if update_result
          (setq blocks_updated (1+ blocks_updated))
          (setq blocks_no_config (1+ blocks_no_config))
        )
        
        ; Move to next block
        (setq ix (1+ ix))
      )
      
      ; Print summary
      (princ (strcat "\nProcessing complete:"
                     "\nTotal blocks processed: " (itoa blocks_processed)
                     "\nBlocks updated: " (itoa blocks_updated)
                     "\nBlocks without CONFIG attribute: " (itoa blocks_no_config)))
    )
    (princ "\nNo blocks found in drawing.")
  )
  (princ)
)
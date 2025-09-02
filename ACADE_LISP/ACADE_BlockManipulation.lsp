(defun c:CopyBlockNameToClipboard ()
  (vl-load-com) ; Ensure Visual LISP COM support is loaded

  ; Function to copy text to clipboard
  (defun *copy-to-clipboard* (str)
    (vlax-invoke-method (vlax-get (vlax-get (vlax-create-object "htmlfile") 'parentwindow) 'clipboardData) 'setData "text" str)
  )

  ; Prompt user to select a block
  (setq ss (entsel "\nSelect a block: "))
  (if ss
    (progn
      (setq ent (car ss))
      (setq entData (entget ent))
      (setq blkname (cdr (assoc 2 entData)))
      (if blkname
        (progn
          (*copy-to-clipboard* blkname)
          (princ (strcat "\nBlock name \"" blkname "\" copied to clipboard."))
        )
        (princ "\nSelected entity is not a block.")
      )
    )
    (princ "\nNo block selected.")
  )
  (princ)
)

(defun c:CopyBlockScaleToClipboard ()
  (vl-load-com) ; Ensure Visual LISP COM support is loaded
  
  ; Function to copy text to clipboard
  (defun *copy-to-clipboard* (str)
    (vlax-invoke-method (vlax-get (vlax-get (vlax-create-object "htmlfile") 'parentwindow) 'clipboardData) 'setData "text" str)
  )

  ; Prompt user to select a block
  (setq ss (entsel "\nSelect a block: "))
  (if ss
    (progn
      (setq ent (car ss))
      (setq entData (entget ent))
      (if (= (cdr (assoc 0 entData)) "INSERT")
        (progn
          (setq scale (cdr (assoc 41 entData))) ; Get X scale (41: X scale, 42: Y scale, 43: Z scale)
          (setq scaleStr (rtos scale 2 4)) ; Convert number to string
          (*copy-to-clipboard* scaleStr) ; Copy to clipboard
          (princ (strcat "\nScale of " (cdr (assoc 2 entData)) " copied to clipboard: " scaleStr))
        )
        (princ "\nSelected entity is not a block.")
      )
    )
    (princ "\nNo block selected.")
  )
  (princ)
)

(defun c:SelectBlocksByName (/ blkName ss)
    (setq blkName (getstring t "\nEnter block name: ")) ; Prompt user to enter the block name

    (if blkName
        (progn
            (setq ss (ssget "X" (list (cons 0 "INSERT") (cons 2 blkName))))
            (if ss
                (progn
                    (sssetfirst nil ss)
                    (princ (strcat "\nSelected " (itoa (sslength ss)) " instances of block: " blkName))
                )
                (princ "\nNo such blocks found.")
            )
        )
        (princ "\nBlock name was not provided.")
    )
    (princ)
)

(defun c:ZoomToBlockByName (/ blkName ss blkRef)
    ; Prompt user to enter the block name
    (setq blkName (getstring t "\nEnter block name: "))

    ; Check if block name was provided
    (if (= blkName "")
        (progn
            (princ "No block name provided.")
            (exit)
        )
    )

    ; Create a selection set to find the block
    (setq ss (ssget "_X" (list (cons 0 "INSERT") (cons 2 blkName))))

    ; Check if any blocks were found
    (if (not ss)
        (progn
            (princ (strcat "\nNo blocks found with name: " blkName))
            (exit)
        )
    )

    ; Get the first block reference
    (setq blkRef (ssname ss 0))
    ;uncomment this to select the block but please comment zoom
    ;(sssetfirst nil (ssadd blkRef))
    ; Zoom to the first block
    (command "_zoom" "_object" (ssadd blkRef) "")
    (princ)
)

(defun c:bswap_ex ( / ss en ed oldblknam newblknam slen ix)
  (setq en (entsel "nSelect block type to swap:"))
  (if en
    (progn
    ; Make sure AcadE is "awake"
      (if (not wd_load)  ;if not in memory
        (if (setq x (findfile "wd_load.lsp")) (load x))   ;load AcadE init functions
     )
      (wd_load)             ;wake up AcadE if not already awake
      (setq en (car en))    ;entity name of block that user picked
      (setq ed (entget en)) ;entity data
      (setq oldblknam (cdr (assoc 2 ed)))  ;get actual block name
      (setq newblknam (getstring "/nEnter New block name to use for swap:"))
      (if (/= newblknam "")
        (progn      ;Okay, prepare to do the swapping!
        ;First find all instances of the old block
          (setq ss (ssget "_X" (list (cons -4 "<AND")(cons 0 "INSERT")
                         (cons 2 oldblknam)(cons -4  "AND>"))))
          (setq slen (sslength ss))
          (setq ix 0)
          (while (< ix slen)
            (setq old_ben (ssname ss ix))   ;next block instance to do
          ;Do the swap. Don't force attributes to old locs, keep
          ;existing block insert scale factor, no attrib mapping.
            (c:wd_bswap old_ben newblknam 1 nil nil nil)
            (setq ix (1+ ix))   ;increment counter to get next instance
         )
       )
     )
   )
 )
  (setq ss nil)release selection set
  (princ)
)

(defun c:bswap_aut ( / ss en ed oldblknam newblknam slen ix)
    (progn
    ; Make sure AcadE is "awake"
      (if (not wd_load)  ;if not in memory
        (if (setq x (findfile "wd_load.lsp")) (load x))   ;load AcadE init functions
     )
      (wd_load)             ;wake up AcadE if not already awake

      (setq oldblknam (getstring "/nEnter Old block name to use for swap:"))  ;get actual block name
      (setq newblknam (getstring "/nEnter New block name to use for swap:"))
      (if (/= newblknam "")
        (progn      ;Okay, prepare to do the swapping!
        ;First find all instances of the old block
          (setq ss (ssget "_X" (list (cons -4 "<AND")(cons 0 "INSERT")
                         (cons 2 oldblknam)(cons -4  "AND>"))))
          (setq slen (sslength ss))
          (setq ix 0)
          (while (< ix slen)
            (setq old_ben (ssname ss ix))   ;next block instance to do
          ;Do the swap. Don't force attributes to old locs, keep
          ;existing block insert scale factor, no attrib mapping.
            (c:wd_bswap old_ben newblknam nil nil nil nil)
            (setq ix (1+ ix))   ;increment counter to get next instance
         )
       )
     )
 )
  (setq ss nil)release selection set
  (princ)
)

(defun c:GetCSVColumn (colNum Fsuffix / filePath file line items columnList)
    ; Hardcoded file path
    (setq filePath (strcat "C:/Users/user/Documents/ACADE_VBA/BLOCKSWAP_" Fsuffix ".csv")) ; Update this path as needed

    ; Open the CSV file
    (setq file (open filePath "r"))
    (if (not file)
        (princ "Unable to open file.")
        (progn
            (setq columnList '())
            (while (setq line (read-line file))
                (setq items (vl-string->list line ","))
                (if (and (>= (length items) colNum) (not (equal (nth (1- colNum) items) nil)))
                    (setq columnList (cons (nth (1- colNum) items) columnList))
                )
            )
            (close file)
            ; Reverse the list to correct order
            (setq columnList (reverse columnList))
        )
    )
    ; Return the column list
    columnList
)

(defun c:GetColumnCSVFile (colNum  filePath / file line items columnList)
    ; Open the CSV file
    (setq file (open filePath "r"))
    (if (not file)
        (princ "Unable to open file.")
        (progn
            (setq columnList '())
            (while (setq line (read-line file))
                (setq items (vl-string->list line ","))
                (if (and (>= (length items) colNum) (not (equal (nth (1- colNum) items) nil)))
                    (setq columnList (cons (nth (1- colNum) items) columnList))
                )
            )
            (close file)
            ; Reverse the list to correct order
            (setq columnList (reverse columnList))
        )
    )
    ; Return the column list
    columnList
)

(defun vl-string->list (str delim / pos start lst)
    (setq start 0 lst '())
    (while (setq pos (vl-string-search delim str start))
        (setq lst (cons (substr str (+ start 1) (- pos start)) lst)
              start (+ pos 1)
        )
    )
    (if (/= start (strlen str))
        (setq lst (cons (substr str (+ start 1)) lst))
    )
    (reverse lst)
)

(defun c:bswap_dwg ( / old_names new_names Fsuffix)
  (wd_load)
  (setq Fsuffix (getstring "\nEnter the suffix for the CSV file : "))
  (setq old_names (c:GetCSVColumn 1 Fsuffix))
  (setq new_names (c:GetCSVColumn 3 Fsuffix))
  (c:wd_bswap_list old_names new_names nil nil nil)
)

(defun c:bswap_custom_dwg ( / old_names new_names)
  (wd_load)
  (setq old_names (c:GetColumnCSVFile 1 "C:/Users/user/Documents/ACADE_VBA/SDBlocks.csv"))
  (setq new_names (c:GetColumnCSVFile 3 "C:/Users/user/Documents/ACADE_VBA/SDBlocks.csv"))
  (c:wd_bswap_list old_names new_names nil nil nil)
)

(defun c:bswap_project ( / dlst temp_dir scr_fname)
  ; Initialize AutoCAD Electrical if not already loaded
  (wd_load)
  
  ; Allow user to select drawings
  (prompt "\nPrompting for drawing selection")
  
  ; Get current project's drawing list
  (setq proj_dwgs (nth 6 (c:wd_proj_wdp_data)))
  
  ; Only proceed if we have a project drawing list
  (if proj_dwgs
    (progn
      ; Show drawing selection dialog
      (setq dlst (c:wd_pdwgs proj_dwgs 
                            "Select drawings for Block Swap" 
                            "" 
                            nil))
      
      ; Only proceed if drawings were selected
      (if (and dlst 
               (car dlst)  ; First element contains drawing list
               (/= 0 (length (car dlst)))) ; List is not empty
        (progn
          (prompt (strcat "\nSelected " (itoa (length (car dlst))) " drawings"))
          
          ; Create script file
          (setq temp_dir (getenv "TEMP")
                scr_fname (strcat temp_dir "/blockswap_script.scr")
                scr_fname (vl-string-translate "\\" "/" scr_fname))
          
          ; Create the script file with just the command name
          (setq f (open scr_fname "w"))
          (write-line "(c:bswap_dwg)" f)
          (close f)
          
          ; Run script on selected drawings
          (prompt "\nRunning block swap script")
          (c:ace_projwide_script (car dlst) scr_fname)
          (prompt "\nBlock swap completed on selected drawings.")
        )
        (prompt "\nNo drawings selected - operation canceled.")
      )
    )
    (prompt "\nNo active project found.")
  )
  (princ)
)

(defun c:ChangeBlocksFont ( / fontName blockRef attribs newStyle attrib entData exitFlag)
    ; Prompt user to enter the font name
    (setq fontName (getstring T "\nEnter font name: "))

    ; Validate fontName
    (if (= fontName "")
        (progn
            (princ "\nNo font name entered. Exiting.")
            (setq exitFlag T)
        )
        ; Create or set the text style to use the specified font (Optional step, might require additional logic)
        ; (setq newStyle "Text style based on entered font, needs creation or setting logic here")
    )

    ; Continuously prompt user to select a block until they decide to stop
    (while (not exitFlag)
        (princ "\nClick on an AutoCAD Electrical block to change its font (Press ESC to stop): ")
        (setq blockRef (car (entsel)))

        ; Check if a block was selected
        (if blockRef
            (progn
                ; Get all attribute references in the block
                (setq attribs (vlax-invoke (vlax-ename->vla-object blockRef) 'GetAttributes))

                ; Iterate through each attribute and change its style
                (foreach attrib attribs
                    (setq entData (entget (vlax-vla-object->ename attrib)))
                    (if (= (cdr (assoc 0 entData)) "ATTRIB")
                        (progn
                            ; Here, directly modify the font without changing the style, since fontName is provided
                            ; This step assumes simplification for demonstration. Adjustments may be needed.
                            (entmod (subst (cons 7 fontName) (assoc 7 entData) entData))
                            (entupd (vlax-vla-object->ename attrib))
                        )
                    )
                )
                (princ (strcat "\nFont for all attributes in the block changed to: " fontName "."))
            )
            ; Exit loop if no selection (user pressed ESC)
            (setq exitFlag T)
        )
    )
    (princ "\nExited block font update mode.")
    (princ)
)


(defun c:ChangeTextFont ( / fontName ent entData newStyle)
    ; Prompt user to enter the font name
    (setq fontName (getstring T "\nEnter the new font name: "))

    ; Check if a font name was entered
    (if (= fontName "")
        (princ "\nNo font name entered. Operation aborted.")
        (progn
            ; Loop to continuously prompt for text entity selection
            (while
                (progn
                    ; Prompt user to select a text or mtext entity
                    (setq ent (entsel "\nSelect a text entity or press ESC to exit: "))
                    (if (not ent) ; Exit loop if ESC is pressed or no selection
                        (nil)
                        (progn
                            (setq entData (entget (car ent)))

                            ; Check if a text or mtext entity was selected
                            (if (or (= (cdr (assoc 0 entData)) "TEXT")
                                    (= (cdr (assoc 0 entData)) "MTEXT"))
                                (progn
                                    ; Change the text style of the entity
                                    ; Note: Assuming 'YourTextStyle' is a style that uses 'fontName'. You may need to create or adjust a style first.
                                    (setq newStyle fontName)
                                    (entmod (subst (cons 7 newStyle) (assoc 7 entData) entData))
                                    (entupd (car ent))
                                    (princ "\nFont changed for selected entity.")
                                    T ; Continue the loop
                                )
                                (progn
                                    (princ "\nSelected entity is not a text or mtext. Try again.")
                                    T ; Continue the loop
                                )
                            )
                        )
                    )
                )
            )
            (princ "\nExited font change mode.")
        )
    )
    (princ)
)

(defun c:ChangeMLeaderFont ( / textStyleName mleaderObj newTextStyleName hasTextStyle result)
  (wd_load) ; Ensure the required functions are loaded
    ; Prompt user to enter the text style name
    (setq textStyleName (getstring T "\nEnter the new text style name: "))
    
    ; Validate text style name input
    (if (= textStyleName "")
        (princ "\nNo text style name entered. Operation aborted.")
        (progn
            (princ "\nSelect MLeaders to change text style. Press ESC to finish.")
            (while
                (setq mleader (entsel "\nSelect an MLeader: "))
                (if mleader
                    (progn
                        ; Convert entity name to VLA-object
                        (setq mleaderObj (vlax-ename->vla-object (car mleader)))
                        ; Verify the selection is an MLeader
                        (if (= (vla-get-objectname mleaderObj) "AcDbMLeader")
                            (progn
                                (setq hasTextStyle (vlax-property-available-p mleaderObj 'Textstylename))
                                ; Attempt to change the text style of the MLeader
                                (vla-put-textstylename mleaderObj textStyleName)
                                (princ "\nText style updated for selected MLeader.")
                            )
                            (princ "\nSelected entity is not an MLeader.")
                        )
                    )
                    (setq exitFlag T) ; Exit flag to break the loop when ESC is pressed
                )
            )
            (princ "\nExited MLeader text style update mode.")
        )
    )
    (princ)
)

(defun c:ZoomToObjectByHandle ( / objHandle objEname)
    ; Prompt user to enter the object handle
    (setq objHandle (getstring T "\nEnter object handle: "))
    
    ; Get the entity name from the handle
    (setq objEname (handent objHandle))
    
    ; Check if the entity exists
    (if (not objEname)
        (princ "\nInvalid handle or object does not exist.")
        (progn
            ; Zoom to the selected object
            (command "_zoom" "_object" objEname "")
            (command "_select" objEname)
            (princ "\nZoomed to the selected object.")
        )
    )
    (princ)
)

(defun c:DeleteObjectByHandle ( / objHandle objEname )
    ; Prompt user to enter the object handle
    (setq objHandle (getstring T "\nEnter object handle: "))
    
    ; Convert the handle to an entity name
    (setq objEname (handent objHandle))
    
    ; Check if the entity exists
    (if (not objEname)
        (princ "\nInvalid handle or object does not exist.")
        (progn
            ; Delete the entity
            (entdel objEname)
            (princ "\nObject deleted.")
        )
    )
    (princ)
)

(defun c:ReplaceTagValContd (/ x find replace ben readList readVal writeVal attr enx)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))  ; If not in memory
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)  ; Ensure AcadE functions are loaded

    ; Get find and replace strings from user
    (setq find (getstring "\nEnter the find text: "))
    (setq replace (getstring "\nEnter the replace text: "))

    ; Select block to change attribute value
    (while 
    (setq ben (entsel "\nSelect block to change attribute value: "))
    (if (not ben)
        (princ "\nOperation cancelled or invalid selection.")
        (progn
            ; Get the list of attributes and assume the first value is the text to find
            (setq ben (car ben))
            (setq readList (c:ace_get_tag_attrval ben nil))
            (if readList
                (progn
                    (setq readVal (car readList))  ; Get the first value which is the text
                    ; Perform a case-insensitive substring search for 'find' in 'readVal'
                    (if (/= (vl-string-search (strcase find) (strcase readVal)) nil)
                        (progn
                            ; If a match is found, replace the text
                            (setq writeVal (vl-string-subst replace find readVal))
                            ; Assuming an update function exists to change the attribute value
                            (c:ace_mod_tag_attrval ben writeVal nil)
                            (princ (strcat "\n\"" readVal "\" replaced with \"" writeVal "\"."))
                        )
                        (princ "\nSubstring not found.")
                    )
                )
            )
        )
    )
    )
    (princ)  ; Clean exit
)

(defun c:InsertTagValContd (/ x insertVal ben attr enx)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))  ; If not in memory
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)  ; Ensure AcadE functions are loaded

    ; Get find and replace strings from user
    (setq insertVal (getstring "\nEnter the Insert text: "))

    ; Select block to change attribute value
    (while 
    (setq ben (entsel "\nSelect block to Insert attribute value: "))
    (if (not ben)
        (princ "\nOperation cancelled or invalid selection.")
        (progn
            ; Get the list of attributes and assume the first value is the text to find
            (setq ben (car ben))
            (c:ace_mod_tag_attrval ben insertVal nil)
            (princ (strcat "\n" insertVal " Inserted"))
        )
    )
    )
    (princ)  ; Clean exit
)

(defun c:ReplaceAttrValContd (/ x find replace attrName ben readList readVal writeVal blockType)
  ;; Ensure AcadE is "awake" and auxiliary functions are loaded
  (if (not (wd_load))
    (if (setq x (findfile "wd_load.lsp"))
      (load x)  ;; Load AcadE init functions
    )
  )
  (wd_load)  ;; Ensure AcadE functions are loaded

  ;; Get find and replace strings from user
  (setq find (getstring "\nEnter the find text: "))
  (setq replace (getstring "\nEnter the replace text: "))
  ;; Ask user for attribute tag
  (setq attrName (getstring "\nEnter the attribute tag: "))

  ;; Loop to keep asking for block selection
  (while
    (progn
      (setq ben (entsel "\nSelect block to change attribute value: "))
      ben  ;; Continue loop if selection is made
    )
    (progn
      ;; Check if selection is made
      (if ben
        (progn
          ;; Process the block
          (setq ben (car ben))
          (setq readVal (c:wd_getattrval ben attrName))
          (if readVal
            (progn
              ;; Check for substring match and replace
              (if (/= (vl-string-search (strcase find) (strcase readVal)) nil)
                (progn
                  ;; Replace text and apply modification
                  (setq writeVal (vl-string-subst replace find readVal))
                  (c:wd_modattrval ben attrName writeVal nil)
                  (princ (strcat "\n\"" attrName "\" value changed from \"" readVal "\" to \"" writeVal "\"."))
                )
                (princ "\nSubstring not found in the attribute value.")
              )
            )
            (princ (strcat "\nAttribute \"" attrName "\" not found in the selected block."))
          )
        )
        (princ "\nInvalid selection or operation cancelled.")
      )
      ;; Optionally, you could add a condition to exit the loop based on user input here
      T  ;; Return true to keep the while loop running
    )
  )
  (princ)  ;; Ensure a clean exit
)

(defun c:ReplacePnlAttrValContd (/ x find replace attrName ben readList readVal writeVal blockType)
  ;; Ensure AcadE is "awake" and auxiliary functions are loaded
  (if (not (wd_load))
    (if (setq x (findfile "wd_load.lsp"))
      (load x)  ;; Load AcadE init functions
    )
  )
  (wd_load)  ;; Ensure AcadE functions are loaded

  ;; Get find and replace strings from user
  (setq find (getstring "\nEnter the find text: "))
  (setq replace (getstring "\nEnter the replace text: "))
  ;; Ask user for attribute tag
  (setq attrName (getstring "\nEnter the attribute tag: "))

  ;; Loop to keep asking for block selection
  (while
    (progn
      (setq ben (entsel "\nSelect block to change attribute value: "))
      ben  ;; Continue loop if selection is made
    )
    (progn
      ;; Check if selection is made
      (if ben
        (progn
          ;; Process the block
          (setq ben (car ben))
          (setq readVal (c:wd_get_pnlval ben attrName))
          (if readVal
            (progn
              ;; Check for substring match and replace
              (if (/= (vl-string-search (strcase find) (strcase readVal)) nil)
                (progn
                  ;; Replace text and apply modification
                  (setq writeVal (vl-string-subst replace find readVal))
                  (c:wd_upd_pnlval ben attrName writeVal)
                  (princ (strcat "\n\"" attrName "\" value changed from \"" readVal "\" to \"" writeVal "\"."))
                )
                (princ "\nSubstring not found in the attribute value.")
              )
            )
            (princ (strcat "\nAttribute \"" attrName "\" not found in the selected block."))
          )
        )
        (princ "\nInvalid selection or operation cancelled.")
      )
      ;; Optionally, you could add a condition to exit the loop based on user input here
      T  ;; Return true to keep the while loop running
    )
  )
  (princ)  ;; Ensure a clean exit
)

(defun c:InsertAttrValContd (/ x insertVal attrName ben blockType)
  ;; Ensure AcadE is "awake" and auxiliary functions are loaded
  (if (not (wd_load))
    (if (setq x (findfile "wd_load.lsp"))
      (load x)  ;; Load AcadE init functions
    )
  )
  (wd_load)  ;; Ensure AcadE functions are loaded

  ;; Get find and replace strings from user
  (setq insertVal (getstring "\nEnter the Insert text: "))
  ;; Ask user for attribute tag
  (setq attrName (getstring "\nEnter the attribute tag: "))

  ;; Loop to keep asking for block selection
  (while
    (progn
      (setq ben (entsel "\nSelect block to change attribute value: "))
      ben  ;; Continue loop if selection is made
    )
    (progn
      ;; Check if selection is made
      (if ben
        (progn
          ;; Process the block
          (setq ben (car ben))
          (c:wd_modattrval ben attrName insertVal nil)
          (princ (strcat "\n" insertVal " Inserted"))
        )
        (princ "\nInvalid selection or operation cancelled.")
      )
      ;; Optionally, you could add a condition to exit the loop based on user input here
      T  ;; Return true to keep the while loop running
    )
  )
  (princ)  ;; Ensure a clean exit
)

(defun c:InsertPnlAttrValContd (/ x insertVal attrName ben blockType)
  ;; Ensure AcadE is "awake" and auxiliary functions are loaded
  (if (not (wd_load))
    (if (setq x (findfile "wd_load.lsp"))
      (load x)  ;; Load AcadE init functions
    )
  )
  (wd_load)  ;; Ensure AcadE functions are loaded

  ;; Get find and replace strings from user
  (setq insertVal (getstring "\nEnter the Insert text: "))
  ;; Ask user for attribute tag
  (setq attrName (getstring "\nEnter the attribute tag: "))

  ;; Loop to keep asking for block selection
  (while
    (progn
      (setq ben (entsel "\nSelect block to change attribute value: "))
      ben  ;; Continue loop if selection is made
    )
    (progn
      ;; Check if selection is made
      (if ben
        (progn
          ;; Process the block
          (setq ben (car ben))
          (c:wd_upd_pnlval ben attrName insertVal )
          (princ (strcat "\n" insertVal " Inserted"))
        )
        (princ "\nInvalid selection or operation cancelled.")
      )
      ;; Optionally, you could add a condition to exit the loop based on user input here
      T  ;; Return true to keep the while loop running
    )
  )
  (princ)  ;; Ensure a clean exit
)

(defun c:FindReplaceTextMTextContd ( / findText replaceText ss i ent entData newText attrName )
  ;; Load required Visual LISP extensions
  (vl-load-com)

  ;; Prompt user for the text to find and the replacement text
  (setq findText (getstring T "\nEnter text to find: "))
  (setq replaceText (getstring T "\nEnter replacement text: "))
  
  ;; Remove quotes if input was passed with them
  ;; Assuming strip-quotes is a user-defined function to remove quotes from the input strings
  ;; If not defined, these lines should be commented or the function should be defined accordingly
  (setq findText (strip-quotes findText))
  (setq replaceText (strip-quotes replaceText))

  ;; Loop to continuously select and process TEXT and MTEXT entities
  (while
    (setq ent (entsel "\nSelect TEXT or MTEXT entity to replace text: "))
    (progn
      ;; Process the selected entity
      (setq entData (entget (car ent)))
      (setq newText nil)

      ;; Check if entity is TEXT or MTEXT and perform case-insensitive search and replace
      (cond
        ((= (cdr (assoc 0 entData)) "TEXT")
          (if (/= (vl-string-search (strcase findText) (strcase (cdr (assoc 1 entData)))) nil)
            (setq newText (vl-string-subst replaceText findText (cdr (assoc 1 entData))))
          )
        )
        ((= (cdr (assoc 0 entData)) "MTEXT")
          (if (/= (vl-string-search (strcase findText) (strcase (cdr (assoc 1 entData)))) nil)
            (setq newText (vl-string-subst replaceText findText (cdr (assoc 1 entData))))
          )
        )
      )

      ;; If newText is not nil, update the entity
      (if newText
        (progn
          (entmod (subst (cons 1 newText) (assoc 1 entData) entData))
          (entupd (car ent))
          (princ (strcat "\nText \"" findText "\" replaced with \"" replaceText "\"."))
        )
        (princ "\nNo matching text found or not a TEXT/MTEXT entity.")
      )
    )
    ;; Condition to exit or repeat, currently set to T for infinite loop, you might add a break condition here
    T
  )
  (princ)  ;; Ensure a clean exit
)

(defun c:InsertTextMTextContd ( / insertText ss i ent entData attrName )
  (vl-load-com)
  (setq insertText (getstring T "\nEnter text to Insert: "))
  (setq insertText (strip-quotes insertText))
  (while
    (setq ent (entsel "\nSelect TEXT or MTEXT entity to insert text: "))
    (progn
      ;; Process the selected entity
      (setq entData (entget (car ent)))
      (entmod (subst (cons 1 insertText) (assoc 1 entData) entData))
      (entupd (car ent))
      (princ (strcat "\n" insertText " Inserted"))
    )
    T
  )
  (princ)  
)

(defun c:ReplaceTagValByType (/ x find replace ben readList readVal writeVal blockType userBlockType)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)

    ; Get find and replace strings from user
    (setq find (getstring "\nEnter the find text: "))
    (setq replace (getstring "\nEnter the replace text: "))
    ; Ask user for block type
    (setq userBlockType (atoi (getstring "\nEnter block type (4,5 for schematic, 7 for terminal): ")))

    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                ; Get the block type
                (setq blockType (c:wd_is_it_schem_or_pnl enx))
                ; Check if the block type matches user's choice
                (if (= blockType userBlockType)
                    (progn
                        ; Process the block
                        (setq readList (c:ace_get_tag_attrval enx nil))
                        (if readList
                            (progn
                                (setq readVal (car readList))
                                (if (/= (vl-string-search (strcase find) (strcase readVal)) nil)
                                ;(if (= (wcmatch (strcase readVal) (strcase find)) T)
                                    (progn
                                        ; If a match is found, replace the text
                                        (setq writeVal (vl-string-subst replace find readVal))
                                        (c:ace_mod_tag_attrval enx writeVal nil)
                                        (princ (strcat "\nModification applied to block: " (cdr (assoc 2 (entget enx)))))
                                    )
                                    (princ "\nSubstring not found.")
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:ReplaceAttrValByType (/ x find replace attrName ben readList readVal writeVal blockType userBlockType)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)

    ; Get find and replace strings from user
    (setq find (getstring "\nEnter the find text: "))
    (setq replace (getstring "\nEnter the replace text: "))
    ; Ask user for block type
    (setq userBlockType (atoi (getstring "\nEnter block type (4,5 for schematic, 7 for terminal): ")))
    ; Ask user for attribute tag
    (setq attrName (getstring "\nEnter the attribute tag: "))

    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                ; Get the block type
                (setq blockType (c:wd_is_it_schem_or_pnl enx))
                ; Check if the block type matches user's choice
                (if (= blockType userBlockType)
                    (progn
                        ; Process the block
                        (setq readVal (c:wd_getattrval enx attrName))
                        (if readVal
                            (progn
                                (if (/= (vl-string-search (strcase find) (strcase readVal)) nil)
                                    (progn
                                        ; If a match is found, replace the text
                                        (setq writeVal (vl-string-subst replace find readVal))
                                        (c:wd_modattrval enx attrName writeVal nil)
                                        (princ (strcat "\nModification applied to block: " (cdr (assoc 2 (entget enx)))))
                                    )
                                    (princ "\nSubstring not found.")
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:ReplaceAttrVal (/ x find replace attrName ben readList readVal writeVal blockType userBlockType)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)

    ; Get find and replace strings from user
    (setq find (getstring "\nEnter the find text: "))
    (setq replace (getstring "\nEnter the replace text: "))
    ; Ask user for attribute tag
    (setq attrName (getstring "\nEnter the attribute tag: "))

    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                (progn
                    ; Process the block
                    (setq readVal (c:wd_getattrval enx attrName))
                    (if readVal
                        (progn
                            (if (/= (vl-string-search (strcase find) (strcase readVal)) nil)
                                (progn
                                    ; If a match is found, replace the text
                                    (setq writeVal (vl-string-subst replace find readVal))
                                    (c:wd_modattrval enx attrName writeVal nil)
                                    (princ (strcat "\nModification applied to block: " (cdr (assoc 2 (entget enx)))))
                                )
                                (princ "\nSubstring not found.")
                            )
                        )
                    )
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:ReplaceMultipleAttrValsByType (/ x find replace attrPrefix numAttrs i attrName ben readVal writeVal blockType userBlockType)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)

    ; Get find and replace strings from user
    (setq find (getstring "\nEnter the find text: "))
    (setq replace (getstring "\nEnter the replace text: "))
    ; Ask user for block type
    (setq userBlockType (atoi (getstring "\nEnter block type (4,5 for schematic, 7 for terminal): ")))
    ; Ask user for the attribute name prefix
    (setq attrPrefix (getstring "\nEnter the attribute name prefix: "))
    ; Ask user for the number of attributes to process
    (setq numAttrs (atoi (getstring "\nEnter number of attributes to process: ")))
    (setq i 0) ; Initialize attribute index
    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                ; Get the block type
                (setq blockType (c:wd_is_it_schem_or_pnl enx))
                ; Check if the block type matches user's choice
                (if (= blockType userBlockType)
                    (progn
                        ; Iterate over the number of attributes
                        (repeat numAttrs
                            (setq i (1+ i)) ; Increment attribute index
                            ; Construct attribute name with leading zero if necessary
                            (setq attrName (strcat attrPrefix (if (< i 10) (strcat "0" (itoa i)) (itoa i))))
                            ; Process each attribute
                            (setq readVal (c:wd_getattrval enx attrName))
                            (if readVal
                                (progn
                                    (if (/= (vl-string-search (strcase find) (strcase readVal)) nil)
                                        (progn
                                            ; If a match is found, replace the text
                                            (setq writeVal (vl-string-subst replace find readVal))
                                            (c:wd_modattrval enx attrName writeVal nil)
                                            (princ (strcat "\nModification applied to attribute: " attrName " in block: " (cdr (assoc 2 (entget enx)))))
                                        )
                                        (princ (strcat "\nSubstring not found in attribute: " attrName "."))
                                    )
                                )
                            )
                        )
                      (setq i 0) ; Reset attribute index
                    )
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:FindReplaceTextMText ( / findText replaceText ss i ent entData newText )
    ; Load required Visual LISP extensions
    (vl-load-com)

    ; Prompt user for the text to find and the replacement text
    (setq findText (getstring T "\nEnter text to find: "))
    (setq replaceText (getstring T "\nEnter replacement text: "))
    ; Remove quotes if input was passed with them
    (setq findText (strip-quotes findText))
    (setq replaceText (strip-quotes replaceText))

    ; Select all TEXT and MTEXT entities in the drawing
    (setq ss (ssget "X" '((0 . "TEXT,MTEXT"))))
    (setq i 0) ; Initialize the counter
    ; Check if any entities were selected
    (if (not ss)
        (princ "\nNo text entities found.")
        (progn
            ; Iterate over each entity in the selection set
            (repeat (sslength ss)
                (setq ent (ssname ss i))
                (setq entData (entget ent))
                (setq newText nil)

                ; Check if entity is TEXT or MTEXT and perform case-insensitive search and replace
                (cond
                    ((= (cdr (assoc 0 entData)) "TEXT")
                        ;(if (/= (vl-string-search findText (cdr (assoc 1 entData))) nil)
                        (if (/= (vl-string-search (strcase findText) (strcase (cdr (assoc 1 entData)))) nil)
                            (setq newText (vl-string-subst replaceText findText (cdr (assoc 1 entData))))
                        )
                    )
                    ((= (cdr (assoc 0 entData)) "MTEXT")
                        ;(if (/= (vl-string-search findText (cdr (assoc 1 entData))) nil)
                        (if (/= (vl-string-search (strcase findText) (strcase (cdr (assoc 1 entData)))) nil)
                            (setq newText (vl-string-subst replaceText findText (cdr (assoc 1 entData))))
                        )
                    )
                )

                ; If newText is not nil, update the entity
                (if newText
                    (progn
                        (entmod (subst (cons 1 newText) (assoc 1 entData) entData))
                        (entupd ent)
                    )
                )

                ; Increment counter
                (setq i (1+ i))
            )
            (princ "\nText replacement completed.")
        )
    )
    (princ)  ; Clean exit
)

(defun c:ReplaceTextOnClick ( / replaceText ent entData )
    ; Load required Visual LISP extensions
    (vl-load-com)

    ; Prompt user for the replacement text
    (setq replaceText (getstring T "\nEnter replacement text: "))
    ; Remove quotes if input was passed with them
    (setq replaceText (strip-quotes replaceText))

    ; Loop until the user presses ESC
    (while
        (progn
            ; Prompt the user to select a text entity
            (setq ent (entsel "\nSelect a text entity: "))
            (if (not ent)
                (princ "\nNo text entity selected or ESC pressed.")
                (progn
                    ; Get the entity data
                    (setq entData (entget (car ent)))

                    ; Check if entity is TEXT or MTEXT and replace the text
                    (cond
                        ((= (cdr (assoc 0 entData)) "TEXT")
                            (entmod (subst (cons 1 replaceText) (assoc 1 entData) entData))
                            (entupd (car ent))
                        )
                        ((= (cdr (assoc 0 entData)) "MTEXT")
                            (entmod (subst (cons 1 replaceText) (assoc 1 entData) entData))
                            (entupd (car ent))
                        )
                    )
                )
            )
        )
    )
    (princ)  ; Clean exit
)

(defun c:UpdateTextSequence ( / startNum increment prefix suffix numLength currentNum ent entData nextNumText formattedNumber)
  ;; Load Visual LISP extensions
  (vl-load-com)

  ;; Get initial number with leading zeros preserved
  (setq startNum (getstring T "\nEnter starting number (preserve leading zeros): "))
  
  ;; Get increment value
  (setq increment (atoi (getstring T "\nEnter increment value: ")))
  
  ;; Get prefix and suffix
  (setq prefix (getstring T "\nEnter prefix (optional, press Enter to skip): "))
  (setq suffix (getstring T "\nEnter suffix (optional, press Enter to skip): "))

  ;; Determine the length of the number for zero-padding
  (setq numLength (strlen startNum))

  ;; Initialize current number based on startNum
  (setq currentNum (atoi startNum))

  (setq prefix (strip-quotes prefix))
  (setq suffix (strip-quotes suffix))

  ;; Loop until user cancels
  (while
    (progn
      ;; Format the number with leading zeros
      (setq formattedNumber (itoa currentNum))
      (while (< (strlen formattedNumber) numLength)
        (setq formattedNumber (strcat "0" formattedNumber))
      )

      ;; Update the display text with the next number to be applied
      (setq nextNumText (strcat prefix formattedNumber suffix))
      (prompt (strcat "\nClick on the text to apply: " nextNumText))

      ;; Prompt the user to select a text entity
      (setq ent (entsel "\nSelect a text entity: "))
      (if (not ent)
        (princ "\nNo text entity selected or ESC pressed.")
        (progn
          ;; Get the entity data
          (setq entData (entget (car ent)))

          ;; Check if entity is TEXT or MTEXT and replace the text
          (if (or (= (cdr (assoc 0 entData)) "TEXT")
                  (= (cdr (assoc 0 entData)) "MTEXT"))
            (progn
              (entmod (subst (cons 1 nextNumText) (assoc 1 entData) entData))
              (entupd (car ent))
              ;; Increment the current number
              (setq currentNum (+ currentNum increment))
            )
            (princ "\nSelected entity is not a text or MTEXT.")
          )
        )
      )
    )
  )
  (princ)  ;; Clean exit
)


(defun strip-quotes (str / )
  (if (= (substr str 1 1) "\"") (setq str (substr str 2))) ; Remove leading quote
  (if (= (substr str (strlen str)) "\"") (setq str (substr str 1 (- (strlen str) 1)))) ; Remove trailing quote
  str
)

(defun c:FindMLeaderWithText ( textToFind / ss ss2 count count2 ent entData textValue x enx readVal total)
  ;; Prompt the user for the text to find
  ;(setq textToFind (getstring T "\nEnter the text to find in MULTILEADER: "))

  ;; Initialize the counter
  (setq count 0)
  (setq count2 0)
  (setvar "USERI1" 0) ; Reset the user integer variable (optional

  ;; Select all MULTILEADER entities in the drawing
  (setq ss (ssget "X" '((0 . "MULTILEADER"))))

  ;; Check if any MULTILEADERs were selected
  (if (not ss)
    (princ "\nNo MULTILEADER entities found.")
    (progn
      ;; Iterate through each entity in the selection set
      (foreach ent (mapcar 'cadr (ssnamex ss))
        ;; Retrieve the entity's data
        (setq entData (entget ent))
        ;; Attempt to find DXF code 302 and extract its value
        ;; Iterate through entity data
        (foreach x entData
          (if (= (car x) 302) ;; Check for DXF code 302
            (if (= (cdr x) textToFind) 
              (setq count (1+ count))
            )
          )
        )
      )
    )
  )
  ;; Select all INSERTS entities in the drawing
  (setq ss2 (ssget "X" '((0 . "INSERT"))))
  ;; Check if any INSERTS were selected
  (if (not ss2)
    (princ "\nNo INSERT entities found.")
    (progn
      ;; Iterate through each entity in the selection set
      (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss2)))
        (progn
          (setq readVal (c:wd_getattrval enx "TAGNUMBER"))
          (if readVal
            (if (= (strcase readVal) (strcase textToFind))
              (setq count2 (1+ count2))
            )
          )
        )
      )
    )
  )
  ;; Report how many MULTILEADERs contain the specified text
  (setq total (+ count count2))
  ;(setvar "USERI1" total) ; Store the total count in a user integer variable (optional)
  ;(princ (strcat "\nTotal number of entities containing text '" textToFind "': " (itoa total)))
  total ; Return Total
)

(defun c:FindMLeaderWithTextRange (/ start end data point table row)
  ;; Prompt the user for the end of the range
  (setq end (getint "\nEnter the end of the range: "))

  ;; Initialize the data list
  (setq data (list (list (cadr (c:wd_split_fnam (getvar "DWGNAME"))) "Drawing Name")))
  (setq data (append data (list (list "Eqip" "Count"))))

  ;; Loop from 1 to end
  (setq start 1)
  (repeat end
    ;; Call c:FindMLeaderWithText with the current number as string
    (setq count (c:FindMLeaderWithText (itoa start)))
    
    (if (= count 0)
        (setq count "")
        (setq count (itoa count))
      )
    
    ;; Add the text to find and its count to the data list
    (setq data (append data (list (list (itoa start) count))))

    ;; Increment start
    (setq start (1+ start))
  )

  ;; Prompt the user to pick the point to place the table
  (setq point (getpoint "\nPick a point to place the table: "))

  ;; Get the current document and model space
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq mspace (vla-get-modelspace doc))

  ;; Create the table
  (setq table (vla-addtable mspace (vlax-3d-point point) (+ end 2) 2 10 50))

  ;; Fill the table with the data
  (setq row 0)
  (foreach item data
    (vla-settext table row 0 (nth 0 item))
    (vla-settext table row 1 (nth 1 item))
    (setq row (1+ row))
  )
)

(defun c:ReseqTermNum (/ x ent ben numstart step blockType retryFlag userInput)
  ;; Ensure AcadE is "awake" and auxiliary functions are loaded
  (if (not (wd_load))
      (if (setq x (findfile "wd_load.lsp"))
          (load x)  ; Load AcadE init functions
      )
  )
  (wd_load)
  
  (setq numstart (getint "\nEnter starting number: "))
  (setq step (getint "\nEnter step: "))
  
  ;; Initialize retryFlag
  (setq retryFlag nil)
  
  (princ "\nInstructions: Select terminal to number, press S to skip current number, or ESC to exit")

  ;; Loop to keep asking for terminal selection
  (while T  ; Infinite loop, needs manual break
    (initget "Skip")  ; Allow "S" or "Skip" as keyword input
    (setq userInput (entsel (strcat "\nSelect terminal to apply " (itoa numstart) " (or S to skip, ESC to exit): ")))
    
    (cond
      ;; If user pressed S (skip current number)
      ((or (= userInput "Skip") (= userInput "S"))
       (progn
         (princ (strcat "\nSkipped number " (itoa numstart)))
         (setq numstart (+ numstart step))  ; Increment to next number
         (setq retryFlag nil)
       )
      )
      ;; If something was selected (check if it's a valid selection)
      ((and userInput (listp userInput))
       (progn
         (setq ent userInput)
         (setq ben (car ent))  ; Get the entity name
         (setq blockType (c:wd_is_it_schem_or_pnl ben))  ; Determine if it's a terminal
         (if (= blockType 7)  ; If it's a terminal
             (progn
               (c:wd_modattrval ben "TERM01" (itoa numstart) nil)  ; Modify the terminal number
               (princ (strcat "\nApplied " (itoa numstart) " to terminal."))
               (setq numstart (+ numstart step))  ; Increment to next number
               (setq retryFlag nil)  ; Reset the retry flag
             )
             (progn
               (princ "\nSelected entity is not a terminal. Retrying ...")
               (setq retryFlag T)  ; Set retryFlag to T for a retry
             )
         )
       )
      )
      ;; If userInput is nil (ESC was pressed)
      ((null userInput)
       (progn
         (princ "\nFunction cancelled by user (ESC pressed).")
         (exit)  ; Exit the function cleanly
       )
      )
      ;; Handle any other input (right-click, invalid input, etc.)
      (T
       (progn
         (princ "\nInvalid input. Please select a terminal, press SPACE to skip, or ESC to exit.")
         (setq retryFlag T)  ; Continue the loop without incrementing
       )
      )
    )
  )
  (princ)  ; Ensure a clean exit
)

(defun c:ChangeTagValByType (/ x ben readList readVal writeVal blockType userBlockType)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)

    ; Get find and replace strings from user
    (setq writeVal (getstring "\nEnter the tag value: "))
    ; Ask user for block type
    (setq userBlockType (atoi (getstring "\nEnter block type (4,5 for schematic, 7 for terminal): ")))

    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                ; Get the block type
                (setq blockType (c:wd_is_it_schem_or_pnl enx))
                ; Check if the block type matches user's choice
                (if (= blockType userBlockType)
                    (progn
                      (c:ace_mod_tag_attrval enx writeVal nil)
                      (princ (strcat "\nModification applied to block: " (cdr (assoc 2 (entget enx)))))
                    )
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:ChangeAttrValByType (/ x attrName ben readList readVal writeVal blockType userBlockType)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)

    ; Get find and replace strings from user
    (setq writeVal (getstring "\nEnter the attribute value: "))
    ; Ask user for block type
    (setq userBlockType (atoi (getstring "\nEnter block type (4,5 for schematic, 7 for terminal): ")))
    ; Ask user for attribute tag
    (setq attrName (getstring "\nEnter the attribute tag name: "))

    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                ; Get the block type
                (setq blockType (c:wd_is_it_schem_or_pnl enx))
                ; Check if the block type matches user's choice
                (if (= blockType userBlockType)
                    (progn
                        ; Process the block
                      (c:wd_modattrval enx attrName writeVal nil)
                      (princ (strcat "\nModification applied to block: " (cdr (assoc 2 (entget enx))))) 
                    )
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:ChangeAttrVal (/ x attrName ben readList readVal writeVal blockType userBlockType)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)

    ; Get find and replace strings from user
    (setq writeVal (getstring "\nEnter the replace text: "))
    ; Ask user for attribute tag
    (setq attrName (getstring "\nEnter the attribute tag: "))

    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                (progn
                    ; Process the block
                  (c:wd_modattrval enx attrName writeVal nil)
                  (princ (strcat "\nModification applied to block: " (cdr (assoc 2 (entget enx)))))  
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:ChangeMultipleAttrValsByType (/ x attrPrefix numAttrs i attrName ben readVal writeVal blockType userBlockType)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)

    ; Get find and replace strings from user
    (setq writeVal (getstring "\nEnter the value: "))
    ; Ask user for block type
    (setq userBlockType (atoi (getstring "\nEnter block type (4,5 for schematic, 7 for terminal): ")))
    ; Ask user for the attribute name prefix
    (setq attrPrefix (getstring "\nEnter the attribute name prefix: "))
    ; Ask user for the number of attributes to process
    (setq numAttrs (atoi (getstring "\nEnter number of attributes to process: ")))
    (setq i 0) ; Initialize attribute index
    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                ; Get the block type
                (setq blockType (c:wd_is_it_schem_or_pnl enx))
                ; Check if the block type matches user's choice
                (if (= blockType userBlockType)
                    (progn
                        ; Iterate over the number of attributes
                        (repeat numAttrs
                            (setq i (1+ i)) ; Increment attribute index
                            ; Construct attribute name with leading zero if necessary
                            (setq attrName (strcat attrPrefix (if (< i 10) (strcat "0" (itoa i)) (itoa i))))
                            ; Process each attribute
                            (progn
                                (c:wd_modattrval enx attrName writeVal nil)
                                (princ (strcat "\nModification applied to attribute: " attrName " in block: " (cdr (assoc 2 (entget enx)))))
                            ) 
                        )
                      (setq i 0) ; Reset attribute index
                    )
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:RedefineBlockList ( / x i namesList blockName)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)
  (setq i 0)
  (setq namesList (c:GetColumnCSVFile 1 "C:/Users/ACER/Documents/ACADE_VBA/SDBlockUpdate.csv"))
  (foreach blockName namesList
    (c:wd_bswap_redefine_curdwg blockName blockName nil nil 1 nil)
    ;(princ (strcat blockName " redefined.\n"))
   )
  (princ)
 )

(defun c:RedefineProjBlockList (/ projList fhandle)
  (wd_load)
  (setq projList (nth 2 (c:ace_proj_data nil))) ; Get list of all drawings in the project
  (c:ace_projwide_script nil "C:/Users/ACER/Documents/ACADE_SCRIPT/redefine.scr")
  (princ)
)

(defun c:redefineSelectedBlock ( / x ss ent entData blkname) 
  ; Ensure AcadE is "awake" and auxiliary functions are loaded
  (if (not (wd_load))
      (if (setq x (findfile "wd_load.lsp"))
          (load x)  ; Load AcadE init functions
      )
  )
  (wd_load) 
    ; Prompt user to select a block
  (setq ss (entsel "\nSelect a block: "))
  (if ss
    (progn
      (setq ent (car ss))
      (setq entData (entget ent))
      (setq blkname (cdr (assoc 2 entData)))
      (if blkname
        (c:wd_bswap_redefine_curdwg blkname blkname nil nil 1 nil)
      )
    )
  )
 )

(defun c:InsertMultiAttrValContd (/ x attrNames attrValues ben namesList valuesList blockType continueEditing)
  ;; Ensure AcadE is "awake" 
  (if (not (wd_load))
    (if (setq x (findfile "wd_load.lsp"))
      (load x)
    )
  )
  (wd_load)
  
  ;; Function to split comma-separated string into list
  (defun split-string (string delimiter / lst pos)
    (while (setq pos (vl-string-search delimiter string))
      (setq lst (cons (substr string 1 pos) lst)
            string (substr string (+ pos 2)))
    )
    (reverse (cons string lst))
  )

  ;; Get attribute names and values once at start
  (setq attrNames (getstring "\nEnter attribute names (comma separated): "))
  (if (= attrNames "")
    (progn 
      (princ "\nNo attribute names entered. Command canceled.")
      (exit)
    )
  )
  
  (setq attrValues (getstring "\nEnter values (comma separated): "))
  (if (= attrValues "")
    (progn 
      (princ "\nNo values entered. Command canceled.")
      (exit)
    )
  )

  ;; Convert strings to lists
  (setq namesList (split-string attrNames ","))
  (setq valuesList (split-string attrValues ","))

  ;; Validate lists have same length
  (if (/= (length namesList) (length valuesList))
    (progn
      (princ "\nError: Number of attributes does not match number of values.")
      (princ (strcat "\nAttributes count: " (itoa (length namesList))))
      (princ (strcat "\nValues count: " (itoa (length valuesList))))
      (exit)
    )
  )

  ;; Print summary of what will be changed
  (princ "\nWill update the following attributes:")
  (setq i 0)
  (repeat (length namesList)
    (princ (strcat "\n" (nth i namesList) " = " (nth i valuesList)))
    (setq i (1+ i))
  )
  (princ "\n\nSelect blocks to update (ESC to exit)")
  
  ;; Continuous block selection and processing loop
  (setq continueEditing T)
  (while continueEditing
    (if (setq ben (entsel "\nSelect block: "))
      (progn
        (setq ben (car ben))
        
        ;; Validate selection is a block
        (if (= "INSERT" (cdr (assoc 0 (entget ben))))
          (progn
            ;; Get block type if needed
            (setq blockType (c:wd_is_it_schem_or_pnl ben))
            
            ;; Process each attribute/value pair
            (setq i 0)
            (repeat (length namesList)
              (if (c:wd_modattrval ben (nth i namesList) (nth i valuesList) nil)
                (princ (strcat "\nUpdated " (nth i namesList)))
                (princ (strcat "\nFailed to set " (nth i namesList)))
              )
              (setq i (1+ i))
            )
          )
          (princ "\nSelected entity is not a block")
        )
      )
      ;; If entsel returns nil (user pressed ESC), exit loop
      (setq continueEditing nil)
    )
  )
  
  (princ "\nCommand completed.")
  (princ)
)

(defun c:InsertMultiPnlValContd (/ x attrNames attrValues ben namesList valuesList blockType continueEditing)
  ;; Ensure AcadE is "awake" 
  (if (not (wd_load))
    (if (setq x (findfile "wd_load.lsp"))
      (load x)
    )
  )
  (wd_load)
  
  ;; Function to split comma-separated string into list
  (defun split-string (string delimiter / lst pos)
    (while (setq pos (vl-string-search delimiter string))
      (setq lst (cons (substr string 1 pos) lst)
            string (substr string (+ pos 2)))
    )
    (reverse (cons string lst))
  )

  ;; Get attribute names and values once at start
  (setq attrNames (getstring "\nEnter attribute names (comma separated): "))
  (if (= attrNames "")
    (progn 
      (princ "\nNo attribute names entered. Command canceled.")
      (exit)
    )
  )
  
  (setq attrValues (getstring "\nEnter values (comma separated): "))
  (if (= attrValues "")
    (progn 
      (princ "\nNo values entered. Command canceled.")
      (exit)
    )
  )

  ;; Convert strings to lists
  (setq namesList (split-string attrNames ","))
  (setq valuesList (split-string attrValues ","))

  ;; Validate lists have same length
  (if (/= (length namesList) (length valuesList))
    (progn
      (princ "\nError: Number of attributes does not match number of values.")
      (princ (strcat "\nAttributes count: " (itoa (length namesList))))
      (princ (strcat "\nValues count: " (itoa (length valuesList))))
      (exit)
    )
  )

  ;; Print summary of what will be changed
  (princ "\nWill update the following attributes:")
  (setq i 0)
  (repeat (length namesList)
    (princ (strcat "\n" (nth i namesList) " = " (nth i valuesList)))
    (setq i (1+ i))
  )
  (princ "\n\nSelect blocks to update (ESC to exit)")
  
  ;; Continuous block selection and processing loop
  (setq continueEditing T)
  (while continueEditing
    (if (setq ben (entsel "\nSelect block: "))
      (progn
        (setq ben (car ben))
        
        ;; Validate selection is a block
        (if (= "INSERT" (cdr (assoc 0 (entget ben))))
          (progn
            ;; Get block type if needed
            (setq blockType (c:wd_is_it_schem_or_pnl ben))
            
            ;; Process each attribute/value pair
            (setq i 0)
            (repeat (length namesList)
              (if (c:wd_upd_pnlval ben (nth i namesList) (nth i valuesList))
                (princ (strcat "\nUpdated " (nth i namesList)))
                (princ (strcat "\nFailed to set " (nth i namesList)))
              )
              (setq i (1+ i))
            )
          )
          (princ "\nSelected entity is not a block")
        )
      )
      ;; If entsel returns nil (user pressed ESC), exit loop
      (setq continueEditing nil)
    )
  )
  
  (princ "\nCommand completed.")
  (princ)
)

(defun c:PinlistCSVUpd ( / csv_str lst blk_desc blk_name term_lst en ed ben)
  ;; Get CSV string from user
  (setq csv_str (getstring "\nEnter CSV (Description,BlockName,Term1,Term2,...): "))
  
  ;; Convert CSV to list using ACADE function
  (setq lst (c:wd_delim_str_to_lst csv_str ","))
  
  ;; Extract description and block name
  (if (>= (length lst) 2)
    (progn
      (setq blk_desc (car lst))
      (setq blk_name (cadr lst))
      ;; Remove first two elements to get terminal list
      (setq term_lst (cdr (cdr lst)))
      
      ;; Prompt user to select block
      (princ "\nSelect block to update terminals: ")
      (if (setq en (entsel))
        (progn
          (setq ben (car en))  ; Get block entity name
          (setq ed (entget ben))
          
          ;; Verify if it's a block and matches the specified name
          (if (and (= "INSERT" (cdr (assoc 0 ed)))
                   (= blk_name (cdr (assoc 2 ed))))
            (progn
              (princ "\nUpdating terminals...")
              
              ;; Update each terminal value
              (setq idx 1)  ; Start counter for terminal numbers
              (foreach term term_lst
                ;; Create attribute name with leading zero if needed
                (setq attnam (strcat "TERM" 
                                   (if (< idx 10) 
                                     (strcat "0" (itoa idx))
                                     (itoa idx))))
                
                ;; Update the attribute value
                (if (c:wd_modattrval ben attnam term 1)
                  (princ (strcat "\nUpdated " attnam " to " term))
                  (princ (strcat "\nWarning: Could not update " attnam)))
                
                (setq idx (1+ idx))  ; Increment counter
              )
              (princ "\nTerminal update complete.")
            )
            (alert "Selected block does not match specified block name!")
          )
        )
        (princ "\nNo block selected.")
      )
    )
    (alert "Invalid CSV format! Must include at least Description and BlockName.")
  )
  (princ)
)

(defun c:ResequenceTags (/ prefix suffix startNum increment currentNum)
  
  (setq prefix (getstring "\nEnter prefix for tag: "))
  (setq suffix (getstring "\nEnter suffix for tag: "))
  (setq startNum (getint "\nEnter starting number: "))
  (setq increment (getint "\nEnter increment value: "))
  (setq currentNum startNum)
  
  (while
    (progn
      (setq ent (car (entsel "\nSelect block to resequence (or press ESC to exit): ")))
      ent
    )
    (setq newTag (strcat prefix (itoa currentNum) suffix))
    (c:ace_mod_tag_attrval ent newTag nil)
    (setq currentNum (+ currentNum increment))
  )
  
  (princ "\nResequencing completed.")
  (princ)
)

(defun c:ResequenceAttributes (/ attrName prefix suffix startNum increment currentNum padLen)
  
  (princ "\nResequence Attributes - Select objects in order and specify attribute to update.")
  
  ; Get attribute name to resequence 
  (setq attrName (getstring "\nEnter attribute name to resequence (e.g. TAG1, TERM01, DESC1): "))
  
  ; Get prefix/suffix and numbering parameters
  (setq prefix (getstring "\nEnter prefix for value (or press Enter for none): "))
  (setq suffix (getstring "\nEnter suffix for value (or press Enter for none): "))
  
  ; Get starting number as string to analyze format
  (setq startNumStr (getstring "\nEnter starting number (e.g. 01, 001, 1): "))
  
  ; Convert to integer for calculations
  (setq startNum (atoi startNumStr))
  
  ; Determine padding length by analyzing input format
  (setq padLen (strlen (vl-string-right-trim "0123456789" startNumStr)))
  (if (= padLen 0) ; If no leading zeros, check if the number itself has leading zeros
    (setq padLen (- (strlen startNumStr) (strlen (itoa (atoi startNumStr)))))
  )
  
  (setq increment (getint "\nEnter increment value: "))
  (setq currentNum startNum)
  
  ; Helper function to pad number with zeros
  (defun pad-number (num padLen)
    (setq numStr (itoa num))
    (setq actualLen (strlen numStr))
    (if (> padLen actualLen)
      (strcat (repeat (- padLen actualLen) "0") numStr)
      numStr
    )
  )
  
  ; Let user select objects one by one to control sequence order
  (while
    (progn
      (setq ent (car (entsel "\nSelect object to resequence (or press ESC to exit): ")))
      ent
    )
    ; Build new value with prefix + padded number + suffix
    (setq newValue (strcat prefix (pad-number currentNum (+ padLen (strlen (itoa startNum)))) suffix))
    
    ; Update using AutoCAD Electrical attribute modification function
    (if (c:wd_modattrval ent attrName newValue nil)
      (progn
        (princ (strcat "\nSet " attrName " = " newValue))
        (setq success (1+ (if success success 0)))
      )
      (princ (strcat "\nFailed to update " attrName " on selected object"))
    )
    
    ; Increment to next number
    (setq currentNum (+ currentNum increment))
  )
  
  ; Report results
  (if success
    (princ (strcat "\nSuccessfully resequenced " (itoa success) " attributes."))
    (princ "\nNo attributes were resequenced.")
  )
  
  (princ)
)

(defun c:ReseqStrip (/ ent ben blockType numstart step)
  ;; Initialize variables
  (setq numstart (getint "\nEnter starting number: "))
  (setq step (getint "\nEnter step value (1 for consecutive): "))
  
  ;; Loop to keep asking for terminal selection
  (while 
    (progn
      (setq ent (entsel (strcat "\nSelect terminal to apply " (itoa numstart) " [Right click/Enter for next number]: ")))
      (if ent  ; If something was selected
        (progn
          (setq ben (car ent))  ; Get the entity name
          (setq blockType (c:wd_is_it_schem_or_pnl ben))  ; Determine if it's a terminal
          (if (= blockType 7)  ; If it's a terminal
              (progn
                (c:wd_modattrval ben "STRIPSEQ" (itoa numstart) nil)  ; Modify the terminal number
                (princ (strcat "\nApplied " (itoa numstart) " to terminal."))
                T  ; Continue the loop
              )
              (progn
                (princ "\nSelected entity is not a terminal.")
                T  ; Continue the loop
              )
          )
        )
        ; If nothing selected (right-click/Enter)
        (progn
          (setq numstart (+ numstart step))  ; Increment the number automatically
          (princ (strcat "\nNext number will be: " (itoa numstart)))
          T  ; Continue the loop
        )
      )
    )
  )
  (princ)  ; Ensure a clean exit
)

(defun c:AutoReseqStrip (/ x ss ben blockType term01Value)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)

    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                ; Get the block type
                (setq blockType (c:wd_is_it_schem_or_pnl enx))
                ; Check if it's a terminal block (type 7)
                (if (= blockType 7)
                    (progn
                        ; Read TERM01 value
                        (setq term01Value (c:wd_getattrval enx "TERM01"))
                        ; Copy TERM01 value to STRIPSEQ
                        (if term01Value  ; Only update if TERM01 has a value
                            (progn
                                (c:wd_modattrval enx "STRIPSEQ" term01Value nil)
                                (princ (strcat "\nUpdated terminal: TERM01=" term01Value))
                            )
                        )
                    )
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:AutoReseqPLCStrip (/ x ss ben blockType term01Value quotient remainder newValue)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)

    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                ; Get the block type
                (setq blockType (c:wd_is_it_schem_or_pnl enx))
                ; Check if it's a terminal block (type 7)
                (if (= blockType 7)
                    (progn
                        ; Read TERM01 value
                        (setq term01Value (c:wd_getattrval enx "TERM01"))
                        ; Process if TERM01 has a value
                        (if term01Value
                            (progn
                                ; Convert string to integer
                                (setq term01Int (atoi term01Value))
                                ; Calculate integer division by 2
                                (setq quotient (/ term01Int 2))
                                ; Calculate remainder
                                (setq remainder (rem term01Int 2))
                                ; Calculate new value (quotient + remainder)
                                (setq newValue (itoa (+ quotient remainder)))
                                ; Update STRIPSEQ with new value
                                (c:wd_modattrval enx "STRIPSEQ" newValue nil)
                                (princ (strcat "\nUpdated terminal: TERM01=" term01Value 
                                             " Quotient=" (itoa quotient)
                                             " Remainder=" (itoa remainder)
                                             " New STRIPSEQ=" newValue))
                            )
                        )
                    )
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:get_subsec ( / prjdata dwg_ix subsec_lst)
  ; Read the active project data 
  (if (not GBL_wd_prj_ixlst)
      (setq GBL_wd_prj_ixlst (c:ace_proj_data nil))
  )
  
  ; Get current drawing's index in project
  (setq dwg_ix GBL_wd_cip)
  (if (OR (not dwg_ix)(= dwg_ix 0))
      (setq dwg_ix (c:wd_is_cur_dwg_in_proj))
  )

  ; Print drawing index to console
  (princ "\nDrawing Index: ")
  (princ dwg_ix)

  ; If drawing found in project
  (if (> dwg_ix 0)
      (progn
        ; Get list of subsection codes from project data
        (setq subsec_lst (nth 10 GBL_wd_prj_ixlst))
        
        ; Use drawing index to look up its subsection
        (if (setq x (member dwg_ix (nth 1 GBL_wd_prj_ixlst)))
            (setq subsec (nth (- (length (nth 1 GBL_wd_prj_ixlst))
                                (length x))
                             subsec_lst))
        )
      )
  )
  
  ; Return subsection code (or "" if not found)
  (if (not subsec)(setq subsec ""))
  (princ "\nSubsection Code: ")
  (princ subsec)
  (princ)
)

(defun get_drawing_subsec ( / dwg_ix subsec_lst subsec)
  ; Read the active project data if not already loaded
  (if (not GBL_wd_prj_ixlst)
      (setq GBL_wd_prj_ixlst (c:ace_proj_data nil))
  )
  
  ; Get current drawing's index in project
  (setq dwg_ix GBL_wd_cip)
  (if (OR (not dwg_ix)(= dwg_ix 0))
      (setq dwg_ix (c:wd_is_cur_dwg_in_proj))
  )

  ; Get subsection code if drawing is in project
  (if (> dwg_ix 0)
      (progn
        (setq subsec_lst (nth 10 GBL_wd_prj_ixlst))
        (if (setq x (member dwg_ix (nth 1 GBL_wd_prj_ixlst)))
            (setq subsec (nth (- (length (nth 1 GBL_wd_prj_ixlst))
                                (length x))
                             subsec_lst))
        )
      )
  )
  
  ; Return subsection code (or "" if not found)
  (if (not subsec)(setq subsec ""))
  subsec
)

(defun c:ReplaceTagValBySubsec (/ x find subsec ben readList readVal writeVal blockType userBlockType)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)
        )
    )
    (wd_load)

    ; Get the subsection code using our utility function
    (setq subsec (get_drawing_subsec))
    
    ; If no subsection found, exit
    (if (= subsec "")
        (progn
            (princ "\nNo subsection code found for current drawing.")
            (exit)
        )
    )

    ; Get find string from user
    (setq find (getstring "\nEnter the find text: "))
    
    ; Ask user for block type
    (setq userBlockType (atoi (getstring "\nEnter block type (4,5 for schematic, 7 for terminal): ")))

    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Print the subsection being used
            (princ "\nUsing subsection code: ")
            (princ subsec)
            
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                ; Get the block type
                (setq blockType (c:wd_is_it_schem_or_pnl enx))
                ; Check if the block type matches user's choice
                (if (= blockType userBlockType)
                    (progn
                        ; Process the block
                        (setq readList (c:ace_get_tag_attrval enx nil))
                        (if readList
                            (progn
                                (setq readVal (car readList))
                                (if (/= (vl-string-search (strcase find) (strcase readVal)) nil)
                                    (progn
                                        ; If a match is found, replace the text with subsection
                                        (setq writeVal (vl-string-subst subsec find readVal))
                                        (c:ace_mod_tag_attrval enx writeVal nil)
                                        (princ (strcat "\nModification applied to block: " (cdr (assoc 2 (entget enx)))))
                                    )
                                    (princ "\nSubstring not found.")
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:ChangeTagValBySubsec (/ x ben prefix suffix subsec tagValue blockType userBlockType)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)

    ; Get the subsection code using our utility function
    (setq subsec (get_drawing_subsec))
    
    ; If no subsection found, exit
    (if (= subsec "")
        (progn
            (princ "\nNo subsection code found for current drawing.")
            (exit)
        )
    )

    ; Get prefix and suffix from user
    (setq prefix (getstring "\nEnter prefix (press ENTER for none): "))
    (setq suffix (getstring "\nEnter suffix (press ENTER for none): "))
    
    ; Construct the complete tag value
    (setq tagValue (strcat prefix subsec suffix))
    
    ; Print the tag value being used
    (princ "\nUsing tag value: ")
    (princ tagValue)

    ; Ask user for block type
    (setq userBlockType (atoi (getstring "\nEnter block type (4,5 for schematic, 7 for terminal): ")))

    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                ; Get the block type
                (setq blockType (c:wd_is_it_schem_or_pnl enx))
                ; Check if the block type matches user's choice
                (if (= blockType userBlockType)
                    (progn
                        (c:ace_mod_tag_attrval enx tagValue nil)
                        (princ (strcat "\nModification applied to block: " 
                                     (cdr (assoc 2 (entget enx)))))
                    )
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:ReplaceAttrValBySubsec (/ x find subsec attrName ben readList readVal writeVal blockType userBlockType)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)

    ; Get the subsection code using our utility function
    (setq subsec (get_drawing_subsec))
    
    ; If no subsection found, exit
    (if (= subsec "")
        (progn
            (princ "\nNo subsection code found for current drawing.")
            (exit)
        )
    )

    ; Get find text from user
    (setq find (getstring "\nEnter the find text: "))
    
    ; Ask user for block type
    (setq userBlockType (atoi (getstring "\nEnter block type (4,5 for schematic, 7 for terminal): ")))
    
    ; Ask user for attribute tag
    (setq attrName (getstring "\nEnter the attribute tag: "))

    ; Print the subsection being used
    (princ "\nUsing subsection code: ")
    (princ subsec)

    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                ; Get the block type
                (setq blockType (c:wd_is_it_schem_or_pnl enx))
                ; Check if the block type matches user's choice
                (if (= blockType userBlockType)
                    (progn
                        ; Process the block
                        (setq readVal (c:wd_getattrval enx attrName))
                        (if readVal
                            (progn
                                (if (/= (vl-string-search (strcase find) (strcase readVal)) nil)
                                    (progn
                                        ; If a match is found, replace the text with subsection
                                        (setq writeVal (vl-string-subst subsec find readVal))
                                        (c:wd_modattrval enx attrName writeVal nil)
                                        (princ (strcat "\nModification applied to block: " (cdr (assoc 2 (entget enx)))))
                                    )
                                    (princ "\nSubstring not found.")
                                )
                            )
                        )
                    )
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:ChangeAttrValBySubsec (/ x attrName ben prefix suffix subsec attrValue blockType userBlockType)
    ; Ensure AcadE is "awake" and auxiliary functions are loaded
    (if (not (wd_load))
        (if (setq x (findfile "wd_load.lsp"))
            (load x)  ; Load AcadE init functions
        )
    )
    (wd_load)

    ; Get the subsection code using our utility function
    (setq subsec (get_drawing_subsec))
    
    ; If no subsection found, exit
    (if (= subsec "")
        (progn
            (princ "\nNo subsection code found for current drawing.")
            (exit)
        )
    )

    ; Get prefix and suffix from user
    (setq prefix (getstring "\nEnter prefix (press ENTER for none): "))
    (setq suffix (getstring "\nEnter suffix (press ENTER for none): "))
    
    ; Construct the complete attribute value
    (setq attrValue (strcat prefix subsec suffix))
    
    ; Print the attribute value being used
    (princ "\nUsing attribute value: ")
    (princ attrValue)

    ; Ask user for block type
    (setq userBlockType (atoi (getstring "\nEnter block type (4,5 for schematic, 7 for terminal): ")))
    
    ; Ask user for attribute tag
    (setq attrName (getstring "\nEnter the attribute tag name: "))

    ; Select all block references in the drawing
    (setq ss (ssget "X" '((0 . "INSERT"))))

    ; Check if any blocks were selected
    (if (not ss)
        (princ "\nNo blocks found.")
        (progn
            ; Iterate over each block in the selection set
            (foreach enx (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
                ; Get the block type
                (setq blockType (c:wd_is_it_schem_or_pnl enx))
                ; Check if the block type matches user's choice
                (if (= blockType userBlockType)
                    (progn
                        (c:wd_modattrval enx attrName attrValue nil)
                        (princ (strcat "\nModification applied to block: " 
                                     (cdr (assoc 2 (entget enx)))))
                    )
                )
            )
        )
    )
    (princ "\nOperation completed.")
    (princ)
)

(defun c:ShiftPLCDescriptions (/ xx numWords ss i charIndex blockName blockType descA descB descC descD descE words-processed)
  ;; Ensure AcadE is "awake" and auxiliary functions are loaded
  (if (not wd_load)
      (progn
        (if (setq x (findfile "wd_load.lsp"))
            (load x)
        )
        (wd_load)
      )
  )
  
  ;; Initialize counter for processed blocks
  (setq words-processed 0)
  
  ;; Get inputs from user
  (setq xx (getstring "\nEnter the attribute number (XX): "))
  ;; Ensure XX is a two-digit number
  (if (< (strlen xx) 2)
      (setq xx (strcat "0" xx))
  )
  
  (setq numWords (atoi (getstring "\nEnter the number of words to move from DESCA to DESCB (counting from right): ")))
  
  ;; Select all block references in the drawing
  (setq ss (ssget "X" '((0 . "INSERT"))))
  
  ;; Check if any blocks were selected
  (if (not ss)
      (princ "\nNo blocks found.")
      (progn
        ;; Process each block
        (setq i 0)
        (repeat (sslength ss)
          (setq enx (ssname ss i))
          (setq blockName (cdr (assoc 2 (entget enx))))
          (setq i (1+ i))
          
          ;; Get the block type
          (setq blockType (c:wd_is_it_schem_or_pnl enx))
          
          ;; Check if the block type is PLC IO (1)
          (if (= blockType 1)
              (progn
                ;; Read current values
                (setq descA (c:wd_getattrval enx (strcat "DESCA" xx)))
                (setq descB (c:wd_getattrval enx (strcat "DESCB" xx)))
                (setq descC (c:wd_getattrval enx (strcat "DESCC" xx)))
                (setq descD (c:wd_getattrval enx (strcat "DESCD" xx)))
                (setq descE (c:wd_getattrval enx (strcat "DESCE" xx)))
                
                ;; Skip if DESCA is empty
                (if (and descA (/= descA ""))
                    (progn
                      ;; Split the string into words
                      (setq words '())
                      (setq currentWord "")
                      (setq len (strlen descA))
                      
                      ;; Process each character to separate words
                      (setq charIndex 1)
                      (while (<= charIndex len)
                        (setq char (substr descA charIndex 1))
                        (if (= char " ")
                            (progn
                              (if (/= currentWord "")
                                  (setq words (cons currentWord words))
                              )
                              (setq currentWord "")
                            )
                            (setq currentWord (strcat currentWord char))
                        )
                        (setq charIndex (1+ charIndex))
                      )
                      
                      ;; Add the last word if there is one
                      (if (/= currentWord "")
                          (setq words (cons currentWord words))
                      )
                      
                      ;; Words are now in reverse order (right to left)
                      ;; Count total words
                      (setq wordCount (length words))
                      
                      ;; Check if we have enough words
                      (if (>= wordCount numWords)
                          (progn
                            ;; Take the first numWords from the list (right side of original text)
                            (setq rightWords '())
                            (setq j 0)
                            (while (< j numWords)
                              (setq rightWords (cons (nth j words) rightWords))
                              (setq j (1+ j))
                            )
                            
                            ;; Take the remaining words (left side of original text)
                            (setq leftWords '())
                            (setq j numWords)
                            (while (< j wordCount)
                              (setq leftWords (cons (nth j words) leftWords))
                              (setq j (1+ j))
                            )
                            
                            ;; Convert lists back to strings
                            (setq rightPart "")
                            (foreach word rightWords
                              (if (= rightPart "")
                                  (setq rightPart word)
                                  (setq rightPart (strcat rightPart " " word))
                              )
                            )
                            
                            (setq leftPart "")
                            (foreach word leftWords
                              (if (= leftPart "")
                                  (setq leftPart word)
                                  (setq leftPart (strcat leftPart " " word))
                              )
                            )
                            
                            ;; Shift values down
                            (c:wd_modattrval enx (strcat "DESCE" xx) descD nil)
                            (c:wd_modattrval enx (strcat "DESCD" xx) descC nil)
                            (c:wd_modattrval enx (strcat "DESCC" xx) descB nil)
                            
                            ;; Move specified words from DESCA to DESCB
                            (c:wd_modattrval enx (strcat "DESCB" xx) rightPart nil)
                            
                            ;; Update DESCA with remaining words
                            (c:wd_modattrval enx (strcat "DESCA" xx) leftPart nil)
                            
                            (setq words-processed (1+ words-processed))
                            (princ (strcat "\nMoved " (itoa numWords) " words from right of DESCA" xx 
                                           " to DESCB" xx " for block " blockName))
                          )
                          (princ (strcat "\nNot enough words in DESCA" xx ". Found " (itoa wordCount) 
                                         " words but need " (itoa numWords) " for block " blockName))
                      )
                    )
                    (princ (strcat "\nDESCA" xx " attribute not found or empty for block " blockName))
                )
              )
          )
        )
        (princ (strcat "\nOperation completed. Processed " (itoa words-processed) " blocks."))
      )
  )
  (princ)
)

(defun c:SwapTrmLvl (/ ent l01label l02label lnumber success)
  ;; Prompt user to select a terminal
  (princ "\nSelect terminal to swap level information: ")
  (if (setq ent (car (entsel)))
    (progn
      ;; Read the current values
      (setq l01label (c:wd_get_1000_xdata ent "VIA_WD_L01LABEL"))
      (setq l02label (c:wd_get_1000_xdata ent "VIA_WD_L02LABEL"))
      (setq lnumber (c:wd_get_1000_xdata ent "VIA_WD_LNUMBER"))
      
      ;; Check if we have values to swap
      (if (and l01label l02label)
        (progn
          ;; Swap L01LABEL and L02LABEL
          (setq success T)
          (if (not (c:wd_mod_1000_xdata ent "VIA_WD_L01LABEL" l02label))
            (setq success nil)
          )
          (if (not (c:wd_mod_1000_xdata ent "VIA_WD_L02LABEL" l01label))
            (setq success nil)
          )
          
          ;; Update LNUMBER if it exists
          (if lnumber
            (progn
              (cond
                ((= lnumber "01")
                  (if (not (c:wd_mod_1000_xdata ent "VIA_WD_LNUMBER" "02"))
                    (setq success nil)
                  )
                )
                ((= lnumber "02")
                  (if (not (c:wd_mod_1000_xdata ent "VIA_WD_LNUMBER" "01"))
                    (setq success nil)
                  )
                )
                (T 
                  (princ (strcat "\nWarning: LNUMBER value '" lnumber "' not recognized as '01' or '02'. Not changed."))
                )
              )
            )
            (princ "\nNote: VIA_WD_LNUMBER not found on this entity. No level number changed.")
          )
          
          ;; Report results
          (if success
            (progn
              (princ "\nTerminal level data successfully swapped:")
              (princ (strcat "\n  Level 1 Label: " l02label " (was: " l01label ")"))
              (princ (strcat "\n  Level 2 Label: " l01label " (was: " l02label ")"))
              (if lnumber
                (princ (strcat "\n  Level Number: " (if (= lnumber "01") "02" "01") " (was: " lnumber ")"))
              )
            )
            (princ "\nError: Failed to update some terminal level data.")
          )
        )
        (princ "\nError: Could not find level label data (VIA_WD_L01LABEL, VIA_WD_L02LABEL) on selected entity.")
      )
    )
    (princ "\nNo entity selected.")
  )
  (princ)
)

;; Create a version that can process multiple terminals
(defun c:SwapTrmLvlM (/ ss count i ent l01label l02label lnumber success)
  (princ "\nSelect terminals to swap level information: ")
  (if (setq ss (ssget))
    (progn
      (setq count (sslength ss))
      (setq i 0)
      (setq success_count 0)
      
      (princ (strcat "\nProcessing " (itoa count) " selected entities..."))
      
      ;; Loop through all selected entities
      (while (< i count)
        (setq ent (ssname ss i))
        
        ;; Read the current values
        (setq l01label (c:wd_get_1000_xdata ent "VIA_WD_L01LABEL"))
        (setq l02label (c:wd_get_1000_xdata ent "VIA_WD_L02LABEL"))
        (setq lnumber (c:wd_get_1000_xdata ent "VIA_WD_LNUMBER"))
        
        ;; Check if we have values to swap
        (if (and l01label l02label)
          (progn
            ;; Swap L01LABEL and L02LABEL
            (setq success T)
            (if (not (c:wd_mod_1000_xdata ent "VIA_WD_L01LABEL" l02label))
              (setq success nil)
            )
            (if (not (c:wd_mod_1000_xdata ent "VIA_WD_L02LABEL" l01label))
              (setq success nil)
            )
            
            ;; Update LNUMBER if it exists
            (if lnumber
              (cond
                ((= lnumber "01")
                  (if (not (c:wd_mod_1000_xdata ent "VIA_WD_LNUMBER" "02"))
                    (setq success nil)
                  )
                )
                ((= lnumber "02")
                  (if (not (c:wd_mod_1000_xdata ent "VIA_WD_LNUMBER" "01"))
                    (setq success nil)
                  )
                )
              )
            )
            
            ;; Count successful operations
            (if success
              (setq success_count (1+ success_count))
            )
          )
        )
        
        (setq i (1+ i))
      )
      
      ;; Report results
      (princ (strcat "\nProcessed " (itoa count) " entities: " (itoa success_count) " terminals updated successfully."))
    )
    (princ "\nNo entities selected.")
  )
  (princ)
)

(defun c:ChangeTrmLvlM (/ ss ent new_level success_count)
  ;; Ask for terminal level first
  (setq new_level (getstring "\nEnter terminal level to apply: "))
  
  ;; Only proceed if user entered something
  (if (/= new_level "")
    (progn
      ;; Select terminals
      (prompt "\nSelect terminals to update: ")
      (setq ss (ssget))
      
      (if ss
        (progn
          (setq success_count 0)
          
          ;; Process each selected terminal
          (foreach ent (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
            (if (c:wd_mod_1000_xdata ent "VIA_WD_LNUMBER" new_level)
              (setq success_count (1+ success_count))
            )
          )
          
          (princ (strcat "\nSuccessfully updated " (itoa success_count) 
                        " terminals to level " new_level))
        )
        (princ "\nNo terminals selected.")
      )
    )
    (princ "\nNo level entered - operation cancelled.")
  )
  (princ)
)

(defun c:CountDwgMfgCat (/ ss count slen ix ben mfg_val cat_val search_mfg search_cat)
  
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
        (setq mfg_val (c:wd_getattrval ben "MFG"))
        (setq cat_val (c:wd_getattrval ben "CAT"))
        
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

(defun c:CalcCompTag (/ ent_sel ent_name tag_result)
  
    ; Function to copy text to clipboard
  (defun *copy-to-clipboard* (str)
    (vlax-invoke-method (vlax-get (vlax-get (vlax-create-object "htmlfile") 'parentwindow) 'clipboardData) 'setData "text" str)
  )
  
  (princ "\nSelect an AutoCAD Electrical component: ")
  
  ;; Prompt user to select an entity
  (setq ent_sel (entsel))
  
  (if ent_sel
    (progn
      ;; Get the entity name from the selection
      (setq ent_name (car ent_sel))
      
      ;; Check if entity exists
      (if ent_name
        (progn
          (princ "\nCalculating component tag...")
          
          ;; Call the AutoCAD Electrical function to calculate component tag
          ;; According to the API documentation, c:wd_calc_comptag expects
          ;; a parameter list with the entity name
          (setq tag_result (c:wd_calc_comptag (list ent_name)))
          
          (if (and tag_result (/= tag_result ""))
            (progn
              ;; Display the calculated tag
              (princ (strcat "\nCalculated tag: " tag_result))
              
              ;; Copy to clipboard using your existing helper function
              (if (vl-catch-all-error-p 
                    (vl-catch-all-apply '*copy-to-clipboard* (list tag_result)))
                (princ "\nTag calculated but could not copy to clipboard.")
                (princ "\nTag copied to clipboard!")
              )
            )
            (princ "\nError: Could not calculate component tag or tag is blank.")
          )
        )
        (princ "\nError: Invalid entity selected.")
      )
    )
    (princ "\nNo entity selected.")
  )
  
  (princ) ;; Clean exit
)

;; Simple version that just calculates and displays the tag
(defun c:CalcTag (/ ent_sel ent_name tag_result)
  (princ "\nSelect an AutoCAD Electrical component: ")
  
  (if (setq ent_sel (entsel))
    (progn
      (setq ent_name (car ent_sel))
      
      (if ent_name
        (progn
          (setq tag_result (c:wd_calc_comptag (list ent_name)))
          
          (if (and tag_result (/= tag_result ""))
            (princ (strcat "\nCalculated component tag: " tag_result))
            (princ "\nError: Could not calculate component tag.")
          )
        )
        (princ "\nError: Invalid selection.")
      )
    )
    (princ "\nNo selection made.")
  )
  
  (princ)
)

;;; AutoCAD Rating Calculator - Simplified Version
;;; Uses c:ace_find_substr for efficient suffix detection

;; Predefined list of rating suffixes
(setq *rating-suffixes* '("kW" "KW" "A" "V" "kA" "KA" "MW" "mW" "W" "VA" "kVA" "HP" "hp"))

;; Function to extract numeric value and suffix from rating string
(defun extract-rating-value (rating-str / suffix found-suffix num-str pos)
  (setq rating-str (vl-string-trim " " rating-str)) ; Remove spaces
  (setq found-suffix "")
  
  ;; Check each suffix using ace_find_substr
  (foreach suffix *rating-suffixes*
    (if (and (= found-suffix "") 
             (setq pos (c:ace_find_substr rating-str suffix 1)))
      (setq found-suffix suffix)
    )
  )
  
  ;; Extract numeric part
  (if found-suffix
    (setq num-str (substr rating-str 1 (1- pos)))
    (setq num-str rating-str)
  )
  ;; Return list with numeric value and suffix
  (list (atof num-str) found-suffix)
)

;; Main function
(defun c:calcrating (/ rating-choice blocks-selected rating-values total-value 
                     suffixes common-suffix result-str i current-block 
                     rating-val extracted-data numeric-val suffix-val)
  
  ;; Initialize VLA
  (vl-load-com)
  
  ;; Step 1: Ask user to select rating type
  (initget "RATING1 RATING2 RATING3")
  (setq rating-choice (getkword "\nSelect rating to calculate [RATING1/RATING2/RATING3]: "))
  
  (if (null rating-choice)
    (progn
      (princ "\nNo rating selected. Command cancelled.")
      (exit)
    )
  )
  
  ;; Step 2: Prompt user to select blocks
  (princ (strcat "\nSelect blocks to read " rating-choice " values from:"))
  (setq blocks-selected (ssget '((0 . "INSERT"))))
  
  (if (null blocks-selected)
    (progn
      (princ "\nNo blocks selected. Command cancelled.")
      (exit)
    )
  )
  
  ;; Initialize variables
  (setq rating-values '())
  (setq suffixes '())
  (setq total-value 0.0)
  
  ;; Step 3 & 4: Read rating values from each block
  (setq i 0)
  (repeat (sslength blocks-selected)
    (setq current-block (ssname blocks-selected i))
    
    ;; Use wd_getattrval to get the rating attribute
    (setq rating-val (c:wd_getattrval current-block rating-choice))
 
    (if (and rating-val (> (strlen rating-val) 0))
      (progn
        ;; Extract numeric value and suffix
        (setq extracted-data (extract-rating-value rating-val))
        (setq numeric-val (car extracted-data))
        (setq suffix-val (cadr extracted-data))
        
        (if (> numeric-val 0)
          (progn
            (setq rating-values (cons numeric-val rating-values))
            (setq suffixes (cons suffix-val suffixes))
            (setq total-value (+ total-value numeric-val))
            (princ (strcat "\n" rating-val " -> " (rtos numeric-val 2 2) " " suffix-val))
          )
        )
      )
    )
    (setq i (1+ i))
  )
  
  ;; Step 5: Calculate result and copy to clipboard
  (if (> (length rating-values) 0)
    (progn
      ;; Use first suffix as common suffix
      (setq common-suffix (car suffixes))
      
      (setq result-str (strcat (rtos total-value 2 2) common-suffix))
      
      (princ (strcat "\n\nTotal " rating-choice ": " result-str))
      (princ (strcat "\nBlocks processed: " (itoa (length rating-values))))
      
      ;; Copy to clipboard
      (*copy-to-clipboard* result-str)
      (princ (strcat "\nResult copied to clipboard: " result-str))
    )
    (princ "\nNo valid rating values found.")
  )
  
  (princ)
)

(defun C:MOVEDESC123 (/ from_desc to_desc ss ent from_tag to_tag from_val continue)
  (princ "\nMove DESC Attributes Routine")
  (princ "\nPress ESC to exit at any time")
  

    
    ;; Get DESC number to move FROM
    (setq from_desc (getstring "\nEnter DESC number to move from: "))
    (if (= from_desc "")
        (setq continue nil)
        (progn
          ;; Get DESC number to move TO
          (setq to_desc (getstring "\nEnter DESC number to move to: "))
          (if (= to_desc "")
              (setq continue nil)
              (progn
                ;; Construct attribute tag names
                (setq from_tag (strcat "DESC" from_desc))
                (setq to_tag (strcat "DESC" to_desc))
        (setq continue T)
        (while continue
        (princ "\n")
                ;; Prompt user to select symbol/block
                (princ (strcat "\nSelect symbol to move " from_tag " to " to_tag ":"))
                (setq ss (ssget "_:S" '((0 . "INSERT"))))
                
                (if ss
                    (progn
                      (setq ent (ssname ss 0))
                      
                      ;; Get value from source attribute using AutoCAD Electrical function
                      (setq from_val (c:wd_getattrval ent from_tag))
                      
                      (cond
                        ;; Check if from_val is valid and not empty
                        ((and from_val (/= from_val "") (/= from_val nil))
                         ;; Copy value to destination attribute
                         (if (c:wd_modattrval ent to_tag from_val nil)
                             (progn
                               ;; Clear source attribute
                               (c:wd_modattrval ent from_tag "" nil)
                               (princ (strcat "\nMoved '" from_val "' from " from_tag " to " to_tag)))
                             (princ (strcat "\nError: Could not modify " to_tag " attribute"))))
                        
                        ((= from_val "")
                         (princ (strcat "\nNothing to move: " from_tag " is empty")))
                        
                        ((= from_val nil)
                         (princ (strcat "\nError: " from_tag " attribute not found in selected block")))
                        
                        (t
                         (princ (strcat "\nError: Could not get value from " from_tag)))
                      )
                    )
                    (progn
                      (princ "\nNo block selected or ESC pressed")
                      (setq continue nil)
                    )
                )
              )
          )
        )
    )
  )
  
  (princ "\nMove DESC routine ended")
  (princ)
)

(defun c:RemoveAssyCode (/ ss i ent blk-name att-names j)
  ;; AutoLISP routine to remove all ASSYCODE data from blocks
  ;; Uses wd_upd_pnlval function to update attribute values
  
  (princ "\nRemoving ASSYCODE data from all blocks...")
  
  ;; Define the attribute names to clear
  (setq att-names '("ASSYCODE" "ASSYCODE01" "ASSYCODE02" "ASSYCODE03" "ASSYCODE04"))
  
  ;; Get all block references in the drawing
  (setq ss (ssget "X" '((0 . "INSERT"))))
  
  (if ss
    (progn
      (princ (strcat "\nFound " (itoa (sslength ss)) " block(s) to process..."))
      
      ;; Loop through each block reference
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq blk-name (cdr (assoc 2 (entget ent))))
        
        (princ (strcat "\nProcessing block: " blk-name))
        
        ;; Loop through each ASSYCODE attribute name
        (setq j 0)
        (while (< j (length att-names))
          (setq current-att (nth j att-names))
          (c:wd_upd_pnlval ent current-att "")
          (setq j (1+ j))
        )
        
        (setq i (1+ i))
      )
      
      (princ "\nASSYCODE data removal completed!")
      (princ (strcat "\nProcessed " (itoa (sslength ss)) " block(s)."))
    )
    (princ "\nNo blocks found in the drawing.")
  )
  
  (princ)
)

(defun C:GETBLKHANDLE (/ ent entdata handle blkname)
  (princ "\nSelect a block to display its handle: ")
  (setq ent (car (entsel)))
  
  (if ent
    (progn
      (setq entdata (entget ent))
      (setq handle (cdr (assoc 5 entdata)))
      
      ;; Check if it's a block reference (INSERT)
      (if (= (cdr (assoc 0 entdata)) "INSERT")
        (progn
          (setq blkname (cdr (assoc 2 entdata)))
          (princ (strcat "\nBlock Name: " blkname))
          (princ (strcat "\nBlock Handle: " handle))
          (*copy-to-clipboard* handle)
        )
        (princ "\nSelected object is not a block reference.")
      )
    )
    (princ "\nNo object selected.")
  )
  (princ)
)
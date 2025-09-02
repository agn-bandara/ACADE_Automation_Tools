(defun c:CreateNewWireLayer ( / layerName)
  (wd_load)
  (setq layerName (getstring "\nEnter The Layer Name :"))
  ;(c:ace_new_wiretype layerName (list "WIRE COLOR" "White" "SIZE" nil "_COLOR" "White" "_LTYPE" "Continuous" "_LWEIGHT" "Default") nil)
  (c:ace_new_wiretype layerName (list "WIRE COLOR" layerName) nil)
)

(defun c:CreateNewWireLayerNoNum ( / layerName)
  (wd_load)
  (setq layerName (getstring "\nEnter The Layer Name :"))
  ;(c:ace_new_wiretype layerName (list "WIRE COLOR" "White" "SIZE" nil "_COLOR" "White" "_LTYPE" "Continuous" "_LWEIGHT" "Default") nil)
  (c:ace_new_wiretype layerName (list "WIRE COLOR" layerName "WIRENO" "0") nil)
)

(defun c:UpdateWireNo (wen / return entType wireNum newWire)

    ; Validate selection
    (if (and wen (setq entType (cdr (assoc 0 (entget wen))))
             (= entType "LINE")) ; Check if the entity is a line (or replace with wire type)
        (progn
            ; Get wire number
            (setq return (c:ace_get_wnum wen))

            ; Extract the wire number and check if it is not empty
            (setq wireNum (car return))
            (if (and wireNum (/= wireNum ""))
                (progn
                    ; Delete and update wire number
                    (c:ace_del_wnum wen)
                    (setq newWire (c:wd_putwn wen wireNum))
                    (princ "\nWire number updated.")
                )
                (princ "\nInvalid or empty wire number.")
            )
        )
        (princ "\nInvalid selection or not a wire.")
    )
    (princ)
)


(defun c:UpdateBlkWireNo (wnum_ent / wnum_txt x wire_ent new_wireno)
  (setq wnum_txt (c:wd_getattrval wnum_ent "WIRENO*"))
  (if wnum_txt ; attribute exists, continue
    (progn
      ; Find LINE wires tied to this wire number
      (setq x (c:ace_wnum_find_wire_en wnum_ent nil))
      (if (AND x (car x))
          (progn ; success, found wire. Check its layer name.
            (setq wire_ent (car x)) ; wire is first element of return
            (c:ace_del_wnum wire_ent)
            (setq new_wireno (c:wd_putwn wire_ent wnum_txt))
          )
      )
    ) 
  )
  (princ)
 )

(defun c:UpdateWireNoXY (wen / return entType wireNum newWire xy)

    ; Validate selection
    (if (and wen (setq entType (cdr (assoc 0 (entget wen))))
             (= entType "LINE")) ; Check if the entity is a line (or replace with wire type)
        (progn
            ; Get wire number
            (setq return (c:ace_get_wnum wen))

            ; Extract the wire number and check if it is not empty
            (setq wireNum (car return))
            (setq xy (cdr (assoc 10 (entget (cadr return)))))
            (if (and wireNum (/= wireNum ""))
                (progn
                    ; Delete and update wire number
                    (c:ace_del_wnum wen)
                    (setq newWire (c:wd_putwnxy xy wireNum))
                    (princ "\nWire number updated.")
                )
                (princ "\nInvalid or empty wire number.")
            )
        )
        (princ "\nInvalid selection or not a wire.")
    )
    (princ)
)

(defun c:UpdateBlkWireNoXY (wnum_ent / wnum_txt x wire_ent new_wireno xy)
  (setq wnum_txt (c:wd_getattrval wnum_ent "WIRENO*"))
  (if wnum_txt ; attribute exists, continue
    (progn
      ; Find LINE wires tied to this wire number
      (setq x (c:ace_wnum_find_wire_en wnum_ent nil))
      (if (AND x (car x))
          (progn ; success, found wire. Check its layer name.
            (setq wire_ent (car x)) ; wire is first element of return
            (setq xy (cdr (assoc 10 (entget wnum_ent))))
            (c:ace_del_wnum wire_ent)
            (setq new_wireno (c:wd_putwn wire_ent wnum_txt))
          )
      )
    ) 
  )
  (princ)
 )


(defun c:UpdateOneWireNo ( / wire wen)
    (wd_load)
    ; Prompt user to select a wire
    (setq wire (entsel "\nSelect a wire: "))
    (setq wen (car wire))
    (c:UpdateWireNo wen)
)

(defun c:UpdateOneWireNoXY ( / wire wen)
    (wd_load)
    ; Prompt user to select a wire
    (setq wire (entsel "\nSelect a wire: "))
    (setq wen (car wire))
    (c:UpdateWireNoXY wen)
)

(defun c:StretchUpdateOneWireNoXY ( / wire wen point return xy wNum newWnum)
    (wd_load)
    ; Prompt user to select a wire
    (while t ; Infinite loop
      (setq wire (entsel "\nSelect a wire: "))
      (setq wen (car wire))
      (setq point (cadr wire))
      (setq return (c:ace_get_wnum wen))
      (setq xy (cdr (assoc 10 (entget (cadr return)))))
      (setq wNum (car return))
      (c:ace_del_wnum wen)
      (c:wd_do_str_wire wen point)
      (setq newWnum (c:wd_putwnxy xy wNum))
      (command "_.DELAY" 100); Delay for 100 milliseconds
    )
)

(defun c:CollectWireDataOnLayer (layerName / ss i entList)
    (setq ss (ssget "X" (list (cons 0 "LINE") (cons 8 layerName))))
    (setq entList nil)
    (setq i 0)
    (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq entList (cons ent entList))
        (setq i (1+ i))
    )
    entList
)

(defun c:UpdateAllWireNoOnLayer ( / wire wen wireLayer entList)
    (wd_load)
    
    ; Prompt user to select a wire
    (setq wire (entsel "\nSelect a wire: "))
    (setq wen (car wire))

    ; Get the layer of the selected wire
    (setq wireLayer (cdr (assoc 8 (entget wen))))

    ; Collect entity names of all wires on the same layer
    (setq entList (c:CollectWireDataOnLayer wireLayer))

    ; Loop through each wire on the same layer and update
    (foreach ent entList
        (if (entget ent) ; Check if the wire entity still exists
            (c:UpdateWireNo ent) ; Update wire number
        )
    )
    (princ)
)

(defun c:UpdateAllWireNoOnLayerXY ( / wire wen wireLayer ss i WireNum updatedWires)
    (wd_load)

    ; Prompt user to select a wire
    (setq wire (entsel "\nSelect a wire: "))
    (command "_zoom" "_extents")
    (setq wen (car wire))
    ; Get the layer of the selected wire
    (setq wireLayer (cdr (assoc 8 (entget wen))))

    ; Initialize list of updated wire numbers
    (setq updatedWires '())

    ; Select all lines on the same layer
    (setq ss (ssget "X" (list (cons 0 "LINE") (cons 8 wireLayer))))

    ; Check if there are entities on the layer
    (if ss
        (progn
            ; Loop through each entity
            (setq i 0)
            (repeat (sslength ss)
                (setq wen (ssname ss i))
                
                ; Get current wire number
                (setq WireNum (car (c:ace_get_wnum wen)))

                ; Update wire number if not already updated
                (if (not (member WireNum updatedWires))
                    (progn
                        (c:UpdateWireNoXY wen)
                        (setq updatedWires (cons WireNum updatedWires)) ; Add to updated list
                    )
                )
                (setq i (1+ i))
            )
        )
        (princ "\nNo wires found on the selected wire's layer.")
    )
    (princ)
)


(defun c:UpdateAllWireNoOnAllLayersXY ( / wire wen wireLayer ss i WireNum updatedWires LayersList LayerName)
    (wd_load)
    (command "_zoom" "_extents")
    (setq LayersList (c:ace_get_wiretype_list nil))
    (foreach LayerName LayersList
      ; Get the layer of the selected wire
      (setq wireLayer LayerName)
      (princ wireLayer)
      ; Initialize list of updated wire numbers
      (setq updatedWires '())
      ; Select all lines on the same layer
      (setq ss (ssget "X" (list (cons 0 "LINE") (cons 8 wireLayer))))
      ;(setq ss (ssget "X" '((0 . "LINE") (8 . wireLayer))))
      ; Check if there are entities on the layer
      (if ss
          (progn
              ; Loop through each entity
              (setq i 0)
              (repeat (sslength ss)
                  (setq wen (ssname ss i))
                  
                  ; Get current wire number
                  (setq WireNum (car (c:ace_get_wnum wen)))
                  (if (not (eq WireNum nil))
                  ; Update wire number if not already updated
                  (if (not (member WireNum updatedWires))
                      (progn
                          (c:UpdateWireNoXY wen)
                          (setq updatedWires (cons WireNum updatedWires)) ; Add to updated list
                      )
                  )
                  )
                  (setq i (1+ i))
              )
          )
          (princ "\nNo wires found on the selected wire's layer.")
      )
    )
    (princ)
)

(defun c:UpdateWiresContinuously ( / wire wen)
    (wd_load)

    (princ "\nClick on wires to update them. Press ESC to exit.")

    (while T
        ; Prompt user to select a wire
        (setq wire (entsel "\nSelect a wire: "))
        (setq wen (car wire))

        ; Check if a wire was selected
        (if wen
            (progn
                (c:UpdateWireNo wen) ; Update wire number
            )
            ; Exit loop if user presses ESC (null selection)
            (exit)
        )
    )
    (princ "\nExited wire update mode.")
    (princ)
)

(defun c:UpdateWiresContinuouslyXY ( / wire wen)
    (wd_load)
    (princ "\nClick on wires to update their numbers. Press ESC to exit.")

    (while T ; Infinite loop
        ; Prompt user to select a wire
        (setq wire (entsel "\nSelect a wire: "))
        (setq wen (car wire))

        ; Check if a wire was selected
        (if wen
            (c:UpdateWireNoXY wen) ; Update wire number
            ; Exit loop if user presses ESC (null selection)
            (exit)
        )
    )
    (princ "\nExited wire update mode.")
    (princ)
)

(defun c:RedfineAndUpdateAllWireNO ( / ss wnum_ent wnum_txt x wire_ent savedata wirenoList new_wireno xy)
  (wd_load)
  (setq wirenoList '())
  (command "_.ZOOM" "_E")
  (setq ss (ssget "_X" '((-4 . "<AND")(0 . "INSERT")(2 . "WD_WN*")(-4 . "AND>"))))
  (if (/= ss nil)
    (progn
    (foreach wnum_ent (mapcar 'cadr (ssnamex ss))
      (setq wnum_txt (c:wd_getattrval wnum_ent "WIRENO*")); Get wire number
      (if wnum_txt
        (progn
          (setq x (c:ace_wnum_find_wire_en wnum_ent nil)); Find LINE wires tied to this wire number
          (if (AND x (car x))
            (progn ; success, found wire. get the wire entity
              (setq wire_ent (car x)); wire is first element of return
              (setq xy (cdr (assoc 10 (entget wnum_ent)))); Get the xy of the block
              (setq savedata (list wnum_txt xy))
              (setq wirenoList (cons savedata wirenoList)) ; Corrected line
              (c:ace_del_wnum wire_ent); Delete the wire number
            )
          )
        )
      )
    )
    ;(command "_.PURGE" "BLOCKS" "*" "N")
    (command "_.PURGE" "B" "WD_WN*" "N")
    (command "_.DELAY" 100); Delay for 100 milliseconds
    (foreach savedata wirenoList
      (setq wnum_txt (car savedata))
      (setq xy (cadr savedata))
      (setq new_wireno (c:wd_putwnxy xy wnum_txt))
      (princ (strcat "\nWire number updated: " wnum_txt))
    )
  )
  )
  (princ)
)
  
(defun c:RenameWireLayers ( / search_text replace_text wire_types matched_count doc layers)
  ; Get the active document and its layers collection
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq layers (vla-get-layers doc))

  ; Prompt the user for search and replace text
  (setq search_text (getstring "\nEnter search text: "))
  (setq replace_text (getstring "\nEnter replace text: "))
  
  ; Get a list of all wire types/layers in the current drawing
  (setq wire_types (c:ace_get_wiretype_list nil))
  ; Initialize a counter for renamed layers
  (setq matched_count 0)
  
  ; Loop through each wire layer
  (foreach layer wire_types
    ; Check if the layer name matches the search pattern
    (if (wcmatch layer (strcat "*" search_text "*"))
      (progn
        ; Create the new layer name by substituting replace_text for search_text
        (setq new_layer_name (vl-string-subst replace_text search_text layer))
        
        ; Attempt to rename the layer using ActiveX
        (vl-catch-all-apply 'vla-put-name 
          (list (vla-item layers layer) new_layer_name))
        
        ; Check if the layer was successfully renamed
        (if (= (vla-get-name (vla-item layers new_layer_name)) new_layer_name)
          (progn
            ; If successful, update the COLOR parameter and increment the counter
            (c:ace_mod_wiretype new_layer_name (list "COLOR" new_layer_name) nil)
            (setq matched_count (1+ matched_count))
            (princ (strcat "\nRenamed: " layer " to " new_layer_name))
          )
          ; If renaming fails, print an error message
          (princ (strcat "\nFailed to rename: " layer))
        )
      )
    )
  )
  
  ; Print a summary of the operation
  (princ (strcat "\n\nCompleted. Renamed " (itoa matched_count) " layer(s)."))
  ; Return nil to the AutoCAD command line
  (princ)
)

(defun c:PlaceHolderWireNo ( / pt)
  ;; Make sure AutoCAD Electrical is "awake"
  (if (not wd_load)
      (if (setq x (findfile "wd_load.lsp")) 
          (load x)
      )
  )
  (wd_load) ;; Initialize AutoCAD Electrical

  ;; Start loop to get user input
  (setq continue T)
  (while continue
    ;; Prompt user to select a point on a wire
    (setq pt (getpoint "\nSelect point on wire (press ESC to exit): "))
    
    ;; Check if user pressed ESC (pt will be nil)
    (if pt
        ;; Insert placeholder wire number "?" at selected point
        (progn
          (c:wd_putwnxy pt "?")
          (princ "\nPlaceholder wire number inserted.")
        )
        ;; User pressed ESC, exit loop
        (setq continue nil)
    )
  )
  
  (princ "\nPlaceholder wire number insertion complete.")
  (princ)
)

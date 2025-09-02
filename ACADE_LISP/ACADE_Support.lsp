;;; ACADE_Support.lsp
;; This file contains support functions for AutoCAD ACADE LISP applications.

;;; Function to show a yes/no dialog with a custom message and title.
(defun show-yesno-dialog (message_text dialog_title / dcl_id result)
  ;; Set default title if not provided
  (if (not dialog_title)
    (setq dialog_title "Confirmation")
  )
  
  ;; Load the DCL file
  (setq dcl_id (load_dialog "C:/Users/user/Documents/ACADE_LISP/simple_yesno.dcl"))
  
  (if (not (new_dialog "simple_yesno" dcl_id))
    (progn
      (alert "Error: Could not load dialog")
      (unload_dialog dcl_id)
      nil
    )
    (progn
      ;; Set the custom message and title
      (set_tile "message_text" message_text)
      (set_tile "main_dialog" dialog_title)
      
      ;; Set up button actions
      (action_tile "yes_btn" 
        "(setq result T)(done_dialog 1)")
      
      (action_tile "no_btn" 
        "(setq result nil)(done_dialog 0)")
      
      ;; Start the dialog
      (start_dialog)
      
      ;; Clean up
      (unload_dialog dcl_id)
      
      ;; Return the result
      result
    )
  )
)

;; Function to copy text to clipboard
(defun *copy-to-clipboard* (str)
  (vlax-invoke-method 
    (vlax-get 
      (vlax-get 
        (vlax-create-object "htmlfile") 
        'parentwindow
      ) 
      'clipboardData
    ) 
    'setData "text" str
  )
)
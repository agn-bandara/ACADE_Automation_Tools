//
// simple_yesno.dcl - Reusable Yes/No dialog with custom message
//

simple_yesno : dialog {
    key = "main_dialog";
    label = "Confirmation";
    initial_focus = "yes_btn";
    
    : column {
        alignment = centered;
        
        : text {
            key = "message_text";
            label = "";
            alignment = centered;
            width = 50;
        }
        
        : spacer { height = 1; }
        
        : row {
            alignment = centered;
            fixed_width = true;
            
            : button {
                key = "yes_btn";
                label = "&Yes";
                width = 12;
                is_default = true;
            }
            
            : spacer { width = 2; }
            
            : button {
                key = "no_btn";
                label = "&No";
                width = 12;
                is_cancel = true;
            }
        }
        
        : spacer { height = 0.5; }
    }
}
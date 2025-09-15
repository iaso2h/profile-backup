(defun MyAlert2 (Title$ Message1$ Message2$ / Dcl_Id%)
  (princ "\nMyAlert2")(princ)
  ; Load Dialog
  (setq Dcl_Id% (load_dialog "cxt.dcl"))
  (new_dialog "CXT" Dcl_Id%)
  ; Set Dialog Initial Settings
  (set_tile "Title" Title$)
  (set_tile "Text1" Message1$)
  (set_tile "Text2" Message2$)
  ; Dialog Actions
  (action_tile "Help" "(alert \"You don't need any help.\nYou're doing great!\")")
  (start_dialog)
  ; Unload Dialog
  (unload_dialog Dcl_Id%)
  (princ)
);defun MyAlert2
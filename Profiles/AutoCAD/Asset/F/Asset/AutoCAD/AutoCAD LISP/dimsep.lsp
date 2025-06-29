(defun c:foo (/ dimstyles dimstyle)
  (vl-load-com) ; Load Visual LISP extensions
  
  ; Get the current drawing's dimension styles
  (setq dimstyles (vla-get-dimstyles (vla-get-activedocument (vlax-get-acad-object))))
  
  ; Iterate through all dimension styles
  (vlax-for dimstyle dimstyles
    ; Set DIMDSEP to "." (dot) instead of "," (comma)
    (vla-put-dimdsep dimstyle ".")
  )
  
  (princ "\nDIMDSEP changed to dot for all dimension styles.")
  (princ)
)
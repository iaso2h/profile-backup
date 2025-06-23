(defun c:selectByHandle (/ doc inputString handle sset) 
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; Get the user input
  (setq handle (getstring "Input handle: "))
  (setq sset (ssadd))
  (ssadd (handent handle) sset)
  (sssetfirst nil sset)
  
  
  (princ)
)
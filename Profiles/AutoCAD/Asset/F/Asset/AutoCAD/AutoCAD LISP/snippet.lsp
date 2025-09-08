(defun c:snpObj (/) 
  (terpri)
  (princ "(setq foo (vlax-ename->vla-object (car (entsel))))")
  (princ)
)


(defun c:snpTblEnt (/) 
  (terpri)
  (princ "(setq foo (entget (tblobjname \"dimstyle\" \"ISO-25\")))")
  (princ)
)

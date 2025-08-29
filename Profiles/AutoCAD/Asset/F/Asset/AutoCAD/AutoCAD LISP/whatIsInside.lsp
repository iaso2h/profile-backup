(defun c:whatIsInside (/ ss ent vlaObj eType) 
  (vl-load-com)
  (princ "\n")
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
      (princ)
    )
  )

  (if (setq ss (ssget "_:S+.")) 
    (setq ent (ssname ss 0))
    (setq ent (car (entsel)))
  )
  (if ent 
    (progn 
      (setq eType (cdr (assoc 0 (entget ent))))
      (if 
        (not 
          (vl-catch-all-error-p 
            (setq vlaObj (vl-catch-all-apply 'vlax-ename->vla-object 
                                            (list ent)
                        )
            )
          )
        )
        ; )
        (progn 
          (vlax-dump-object vlaObj)
        )
      )
    )
  )

  (princ)
)
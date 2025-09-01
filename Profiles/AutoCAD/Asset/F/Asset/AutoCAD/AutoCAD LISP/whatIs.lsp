(defun c:whatIs (/ savedCmdecho activeDoc ss ent obj entType objType savedEntLast 
                 inspectatioinText
                ) 
  (vl-load-com)
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )
  (setq savedCmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (setq activeDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (setq ss (ssget "_:S")) 
    (setq ent (ssname ss 0))
    (setq ent (car (entsel)))
  )

  (if ent 
    (progn 
      (if 
        (vl-catch-all-error-p 
          (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))
        )
        (setq objType "No Info")
        (setq objType (vla-get-ObjectName obj))
      )

      (setq entType (cdr (assoc 0 (entget ent))))
      (princ (strcat entType "\n"))
      (princ (strcat objType "\n"))
      (setq savedEntLast (entlast))
      (setq inspectatioinText (getpoint "\n插入文字: "))
      (command "_text" 
               "j"
               "mc"
               inspectatioinText
               25
               0
               (strcat entType "\n" objType)
      )

      (if *SearchIncluded* 
        (progn 
          (load "util.lsp")
          (iaso2h:layerSetXline savedEntLast)
        )
      )
    )
  )

  (setvar "cmdecho" savedCmdecho)
  (princ)
)
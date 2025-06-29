; ; credit: https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/lisp-code-to-change-the-overall-scale-for-all-dimension-objects/td-p/7968485
(defun dimStyleMod (assocNum val / factor dimStyles dimStyleEnt fontSizeAssoc fontSize) 
  (while (setq dimStyles (tblnext "dimstyle" (not dimStyles))) 
    (setq dimStyleEnt (entget (tblobjname "dimstyle" (cdr (assoc 2 dimStyles)))))
    (setq fontSizeAssoc (assoc 140 dimStyleEnt))
    (if fontSizeAssoc 
      (setq factor (/ (cdr fontSizeAssoc) 3.5))
      (setq factor 1)
    )


    (if (member assocNum '(42 44)) 
      (entmod 
        (subst (cons assocNum (* val factor)) (assoc assocNum dimStyleEnt) dimStyleEnt)
      )
      (progn 
        (if (assoc assocNum dimStyleEnt) 
          (entmod (subst (cons assocNum val) (assoc assocNum dimStyleEnt) dimStyleEnt)) ; then -- replace it
          (entmod (append dimStyleEnt (list (cons assocNum val)))) ; else -- add it
        )
      )
    )

    (princ)
  )
)


(defun C:dimSWFix () 
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )
  
  (setvar "CMDECHO" 0)
  ; https://help.autodesk.com/view/OARX/2024/ENU/?guid=GUID-F2FAD36F-0CE3-4943-9DAD-A9BCD2AE81DA
  (dimStyleMod 144 1.0) ; DIMLFAC. Scale factor
  (dimStyleMod 280 0) ; DIMJUST. Text horizontal position
  (dimStyleMod 77 1) ; DIMTAD. Text vertical position
  (dimStyleMod 74 1) ; ISO text alginment standard
  (dimStyleMod 44 2) ; extended line length
  (dimStyleMod 42 2) ; offset length
  (dimStyleMod 5 "") ; arrow style
  (dimStyleMod 172 1) ; draw line between dismension ends
  (dimStyleMod 278 0) ; dimension decimal value separator

  (setvar "CMDECHO" 1)
  (princ)
)

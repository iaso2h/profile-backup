;; Credit: https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/set-all-text-to-standard-text-style/td-p/6676904
(defun fontStandardConvert
  ;; = Text (& Mtext) [to] Standard Style, including in Blocks,
  ;; and Attributes, both in Blocks & unassociated Att. Def's
  (style / tss edata blkname blklist ent)

  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  (setvar "CMDECHO" 0)
  (setvar "NOMUTT" 1)

  (if (setq tss (ssget style '((0 . "*TEXT,ATTDEF"))))
    (repeat (setq n (sslength tss))
      (setq edata (entget (ssname tss (setq n (1- n)))))
      (entmod (subst '(7 . "STANDARD") (assoc 7 edata) edata))
    ) ; repeat
  ) ; if
  (while (setq blkname (cdadr (tblnext "block" (not blkname))))
    (if
      (not
        (or
          (wcmatch blkname "`*D*,*|*") ; Dimension or Xref-dependent
          (assoc 1 (tblsearch "block" blkname)) ; an Xref
          (member blkname blklist) ;already in the list
        ) ; or
      ) ; not
      (setq blklist (cons blkname blklist))
    ) ; if
  ) ; while
  (foreach blkname blklist
    (setq ent (tblobjname "block" blkname))
    (while (setq ent (entnext ent))
      (if (wcmatch (cdr (assoc 0 (setq edata (entget ent)))) "*TEXT,ATTDEF")
        (entmod (subst '(7 . "STANDARD") (assoc 7 edata) edata))
      ) ; if
    ) ; while
  ) ; foreach
  (command
    "_.regen"
    "_.attsync"
    "_name"
    "*"
  ) ; command

  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )
  
  (setvar "CMDECHO" 1)
  (setvar "NOMUTT" 0)
  (princ)
); defun

(defun C:font2Standard () (fontStandardConvert "_:L") (princ))
(defun C:font2StandardAll () (fontStandardConvert "_X") (princ))

(princ)

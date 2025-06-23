(defun C:xrefLayerMerge (/ actDoc layerStrIdx layerDesc nameOld nameNew) 
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数己取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )

  (defun getNewName (nameOld / layerStrIdx) 
    (setq layerStrIdx (vl-string-search "$0$" nameOld))
    (substr nameOld (+ layerStrIdx 4))
  )

  (princ "\n")
  (setvar "CMDECHO" 0)
  (setvar "NOMUTT" 1)
  (setvar "CLAYER" "0")
  (command "undo" "be")

  (setq actDoc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (vlax-for l (vla-get-Layers actDoc) 
    ;  (setq layers(cons(vla-get-Name l)layers))
    (setq nameOld (vla-get-Name l))
    ;  ); end vlax-for
    ;; (reverse layers)
    (if (wcmatch nameOld "*$0$*") 
      ; Layers from xrefs
      (progn 
        (setq nameNew (getNewName nameOld))
        (if (tblsearch "LAYER" nameNew) 
          ; Layer exists beforhand
          ; (command ".-laymrg" "n" nameOld "" "n" nameNew "y")
          (vl-catch-all-apply 'vl-cmdf 
                              (list ".-laymrg" "n" nameOld "" "n" nameNew "y")
          )
          (if (eq "SLD-0" nameNew) 
            ; Merge SLD-0 to 0
            (command ".-laymrg" "n" nameOld "" "n" "0" "y")
            ; Rename to a new layer and remove the prefix
            (command ".-rename" "la" nameOld nameNew)
          )
        )
      )
    )

    (if 
      (and (not (vlax-erased-p l)) 
           (wcmatch (vla-get-description l) "XRef Layer")
      )
      ; Merge layer with "XRef Layer" description to 0
      (command ".-laymrg" "n" nameOld "" "n" "0" "y")
    )
  )

  ; (vlax-for Obj (vla-get-Blocks actDoc)
  ;   (setq nameOld (vla-get-Name Obj))
  ;   (if (wcmatch nameOld "*$0$*")
  ;     (if (/= (setq nameNew (getNewName nameOld)) nameOld)
  ;       (if
  ;         (vl-catch-all-error-p
  ;           (vl-catch-all-apply 'vla-put-Name (list Obj nameNew))
  ;         )
  ;         (prompt (strcat "\n Block: " nameOld " was not renamed."))
  ;       )
  ;     )
  ;   )
  ; )

  ; (vlax-for Obj (vla-get-UserCoordinateSystems actDoc)
  ;   (setq nameOld (vla-get-Name Obj))
  ;   (if (wcmatch nameOld "*$0$*")
  ;     (if (/= (setq nameNew (getNewName nameOld)) nameOld)
  ;       (if
  ;         (vl-catch-all-error-p
  ;           (vl-catch-all-apply 'vla-put-Name (list Obj nameNew))
  ;         )
  ;         (prompt (strcat "\n UCS: " nameOld " was not renamed."))
  ;       )
  ;     )
  ;   )
  ; )

  ; (vlax-for Obj (vla-get-Views actDoc)
  ;   (setq nameOld (vla-get-Name Obj))
  ;   (if (wcmatch nameOld "*$0$*")
  ;     (if (/= (setq nameNew (getNewName nameOld)) nameOld)
  ;       (if
  ;         (vl-catch-all-error-p
  ;           (vl-catch-all-apply 'vla-put-Name (list Obj nameNew))
  ;         )
  ;         (prompt (strcat "\n View: " nameOld " was not renamed."))
  ;       )
  ;     )
  ;   )
  ; )

  (setvar "CMDECHO" 1)
  (setvar "NOMUTT" 0)
  (command "undo" "e")
  (princ)
)

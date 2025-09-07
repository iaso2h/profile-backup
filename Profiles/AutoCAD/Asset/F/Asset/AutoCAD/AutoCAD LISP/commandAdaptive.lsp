(defun commandAdaptive (commandAlias /) 
  (defun *error* (msg) 
    (if oldCmdEcho (setvar "CMDECHO" oldCmdEcho))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*BREAK*,*EXIT*"))) 
      (princ (strcat "\nError: " msg))
    )
    (princ) ; Suppress error message on quiet exit
  )

  (if (setq ss (ssget "I")) 
    (progn
      (setq oldCmdEcho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (if *AutoCADLoaded* 
        (command "._change" "_P" "p" "c" color "")
        (command "._change" ss "" "p" "c" color "")
      )




      (setvar "CMDECHO" oldCmdEcho)
    )
  )



  (princ)
)
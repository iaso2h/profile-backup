(defun c:th () (c:toggleHidden) (princ))
(defun c:toggleHidden (/ ans hiddenState oldCmdEcho) 
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
      (princ)
    )
    (if hiddenState 
      (command-s "_.unisolateobjects")
    )
    (if oldCmdEcho 
      (setvar "CMDECHO" oldCmdEcho)
    )
    (if oldDynamicInput 
      (setvar "DYNMODE" oldDynamicInput)
    )
  )
  (setq hiddenState nil)
  (setq oldCmdEcho (getvar "CMDECHO"))
  (setq oldDynamicInput (getvar "DYNMODE"))
  (setvar "DYNMODE" 0)
  (setvar "CMDECHO" 0)

  (terpri)
  (princ "选择要反复现显的物体:\n")
  (if (not (setq ss (ssget "_:L"))) (exit))
  (while (/= ans "Cancle") 
    (if hiddenState 
      (command "_.unisolateobjects")
      (command "_.HIDEOBJECTS" ss "")
    )
    (setq hiddenState (not hiddenState))
    (initget "Jimbo Cancle")
    (setq ans (getkword "是否继续？[继续\(J\)/取消\(C\)]:<继续\(J\)>"))
    (princ "\n")
  )

  (if hiddenState 
    (command "_.unisolateobjects")
  )
  (setvar "CMDECHO" oldCmdEcho)
  (setvar "DYNMODE" oldDynamicInput)



  (princ)
)

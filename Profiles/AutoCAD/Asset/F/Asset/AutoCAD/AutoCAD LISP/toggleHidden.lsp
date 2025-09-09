(defun c:th() (c:toggleHidden) (princ))
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
  )
  (setq hiddenState nil)
  (setq oldCmdEcho (getvar "CMDECHO"))
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
    (setq ans (getkword "对图模式[继续\(J\)/取消\(C\)]:<继续\(J\)>"))
    (terpri)
    (pp ans)
  )

  (if hiddenState 
    (command "_.unisolateobjects")
  )
  (setvar "CMDECHO" oldCmdEcho)

  
  
  (princ)
)

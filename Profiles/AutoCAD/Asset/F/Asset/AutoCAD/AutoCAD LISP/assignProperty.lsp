; File: assignProperty.lsp
; Author: iaso2h
; Description: 增强版属性匹配
; Version: 0.0.1
; Last Modified: 2024-12-24

(defun c:ass (/ ssSrc ssTgt entSrc entTgt objSrc objTgt dimTextOverrideSrc i) 
  (vl-load-com)
  (princ "\n")
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )
  (defun loopEntSel (/ entSrc) 
    (setq entSrc (car (entsel "\n选择源图元：")))
    (while (null entSrc) 
      (setq entSrc (car (entsel "\n选择源图元：")))
    )

    entSrc
  )

  (if (not (setq ssSrc (ssget "_I"))) 
    (progn 
      (setq entSrc (loopEntSel))
    )
    (if (> (sslength ssSrc) 1) 
      (progn 
        (sssetfirst nil nil)
        (setq entSrc (loopEntSel))
      )
      (setq entSrc (ssname ssSrc 0))
    )
  )
  (if entSrc 
    (progn 
      (sssetfirst nil nil)
      (setq ssTgt (ssget))
      (if ssTgt 
        (progn 
          (setq objSrc (vlax-ename->vla-object entSrc))

          (command "_matchprop" entSrc ssTgt "")
          (if 
            (and 
              (wcmatch (vla-get-ObjectName objSrc) "*Dimension")
              (setq dimTextOverrideSrc (vla-get-TextOverride objSrc))
              (or (eq dimTextOverrideSrc "") 
                  (eq dimTextOverrideSrc "<>")
              )
            )
            (progn 
              (setq i 0)
              (while (< i (sslength ssTgt)) 
                (setq entTgt (ssname ssTgt i))
                (setq objTgt (vlax-ename->vla-object entTgt))
                (if (wcmatch (vla-get-ObjectName objSrc) "*Dimension") 
                  (vla-put-TextOverride objTgt dimTextOverrideSrc)
                )

                (setq i (1+ i))
              )
            )
          )
        )
      )
    )
  )


  (princ)
)
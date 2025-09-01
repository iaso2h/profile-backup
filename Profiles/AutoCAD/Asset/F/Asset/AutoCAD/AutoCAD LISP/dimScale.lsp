; Change dimsacle
(defun dimscleChangeHelper (factor / ss i obj) 
  (princ "\n")
  (if (setq ss (ssget "I" '((0 . "*DIMENSION,MULTILEADER")))) 
    (progn 
      (setq i 0)
      (while (< i (sslength ss)) 
        (setq obj (vlax-ename->vla-object (ssname ss i)))
        (vlax-put-property obj 'ScaleFactor factor)
        (setq i (1+ i))
      )
      (princ 
        (strcat "Dimscale of " 
                (itoa (sslength ss))
                " dimensions have been changed to "
                (rtos factor)
                ".\n"
        )
      )
    )
    (progn 
      (setvar "DIMSCALE" factor)
      (princ (strcat "Current dimscale: " (rtos factor) ".\n"))
    )
  )

  (princ)
)
(setq i 0)
(repeat 9 
  (setq i (+ 1 i))
  (eval 
    (read 
      (strcat "(defun c:g" 
              (rtos i)
              (chr 40)
              (chr 41)
              "(dimscleChangeHelper "
              (rtos i 2 1)
              "))"
      )
    )
  )
)


(princ)
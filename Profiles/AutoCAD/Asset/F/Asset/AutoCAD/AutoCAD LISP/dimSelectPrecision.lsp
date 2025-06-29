(defun c:dimSelectPrecision (/ ss ss0 ss1 ss2 i ename vlaObj vlaType textPrecision 
                             ans tmp
                            ) 
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )
  (princ "\n")
  (vl-load-com)

  (setq ss (ssget "_X" '((0 . "*dimension"))))
  (setq i 0)
  (repeat (sslength ss) 
    (setq ename (ssname ss i))
    (setq vlaObj (vlax-ename->vla-object ename))
    (setq vlaType (vla-get-ObjectName vlaObj))
    (if (not (eq vlaType "AcDb2LineAngularDimension")) 
      (progn 
        (setq textPrecision (vla-get-primaryunitsprecision vlaObj))
        (cond 
          ((eq textPrecision 0)
           (progn 
             (if (null ss0) 
               (setq ss0 (ssadd))
             )
             (ssadd ename ss0)
           )
          )
          ((eq textPrecision 1)
           (progn 
             (if (null ss1) 
               (setq ss1 (ssadd))
             )
             (ssadd ename ss1)
           )
          )
          (t
           (progn 
             (if (null ss2) 
               (setq ss2 (ssadd))
             )
             (ssadd ename ss2)
           )
          )
        )
      )
    )

    (setq i (+ 1 i))
  )

  (while t 
    (if (null ans) 
      (setq ans "0位小数")
    )
    (initget "0位小数 1位小数 2位以及2位以上小数")
    (if (setq tmp (getkword (strcat "选择标注精确度[0位小数/1位小数/2位以及2位以上小数] <" ans ">: \n"))) 
      (setq ans tmp)
    )
    (cond 
      ((= ans "0位小数")
       (if ss0 
         (sssetfirst nil ss0)
         (prompt "提示：没有精确度为0位小数的标注\n")
       )
      )
      ((= ans "1位小数")
       (if ss1 
         (sssetfirst nil ss1)
         (prompt "提示：没有精确度为1位小数的标注 \n")
       )
      )
      (t
       (if ss2 
         (sssetfirst nil ss2)
         (prompt "提示：没有精确度为2位以及2位小数以上的标注 \n")
       )
      )
    )
  )

  (princ)
)
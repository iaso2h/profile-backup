(defun c:dimTangentToggle () 
  (princ "\n")
  (if *TangentLoaded* 
    (progn 
      (if (null *tchDimToggleChk*) 
        (progn 
          (setq *tchDimToggleChk* t)
          (princ "已切换为使用天正标注\n")
        )
        (progn 
          (setq *tchDimToggleChk* nil)
          (princ "已切换为使用默认标注\n")
        )
      )
    )
    (princ "天正插件没有加载\n")
  )

  (princ)
)

(defun c:dimTangentAdapt (/ doc) 
  (vl-load-com)
  (if 
    (and *TangentLoaded* 
         *tchDimToggleChk*
    )
    (command "_TDimMP")
    (vla-SendCommand (vla-get-ActiveDocument (vlax-get-acad-object)) "dim ")
  )
  (princ)
)(defun c:dimTangentToggle () 
  (princ "\n")
  (if *TangentLoaded* 
    (progn 
      (if (null *tchDimToggleChk*) 
        (progn 
          (setq *tchDimToggleChk* t)
          (princ "已切换为使用天正标注\n")
        )
        (progn 
          (setq *tchDimToggleChk* nil)
          (princ "已切换为使用默认标注\n")
        )
      )
    )
    (princ "天正插件没有加载\n")
  )

  (princ)
)

(defun c:dimTangentAdapt (/ doc) 
  (vl-load-com)
  (if 
    (and *TangentLoaded* 
         *tchDimToggleChk*
    )
    (command "_TDimMP")
    (vla-SendCommand (vla-get-ActiveDocument (vlax-get-acad-object)) "dim ")
  )
  (princ)
)(defun c:dimTangentToggle () 
  (princ "\n")
  (if *TangentLoaded* 
    (progn 
      (if (null *tchDimToggleChk*) 
        (progn 
          (setq *tchDimToggleChk* t)
          (princ "已切换为使用天正标注\n")
        )
        (progn 
          (setq *tchDimToggleChk* nil)
          (princ "已切换为使用默认标注\n")
        )
      )
    )
    (princ "天正插件没有加载\n")
  )

  (princ)
)

(defun c:dimTangentAdapt (/ doc) 
  (vl-load-com)
  (if 
    (and *TangentLoaded* 
         *tchDimToggleChk*
    )
    (command "_TDimMP")
    (vla-SendCommand (vla-get-ActiveDocument (vlax-get-acad-object)) "dim ")
  )
  (princ)
)(defun c:dimTangentToggle () 
  (princ "\n")
  (if *TangentLoaded* 
    (progn 
      (if (null *tchDimToggleChk*) 
        (progn 
          (setq *tchDimToggleChk* t)
          (princ "已切换为使用天正标注\n")
        )
        (progn 
          (setq *tchDimToggleChk* nil)
          (princ "已切换为使用默认标注\n")
        )
      )
    )
    (princ "天正插件没有加载\n")
  )

  (princ)
)

(defun c:dimTangentAdapt (/ doc) 
  (vl-load-com)
  (if 
    (and *TangentLoaded* 
         *tchDimToggleChk*
    )
    (command "_TDimMP")
    (vla-SendCommand (vla-get-ActiveDocument (vlax-get-acad-object)) "dim ")
  )
  (princ)
)
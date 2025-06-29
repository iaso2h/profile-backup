(defun c:getLength (/ sset ename vlObj vlaType textLocation textContent savedEntLast) 

  (if (setq sset (ssget "_I")) 
    (setq ename (ssname sset 0))
    (setq ename (car (entsel "\n选择图元: ")))
  )

  (vl-load-com)
  (setq vlObj (vlax-ename->vla-object ename))
  (setq vlaType (vla-get-ObjectName vlObj))

  (cond 
    ((= vlaType "AcDbLine") (setq textContent (vla-get-length vlObj)))
    ((= vlaType "AcDbPolyline") (setq textContent (vla-get-length vlObj)))
    ((= vlaType "AcDbArc") (setq textContent (vla-get-arclength vlObj)))
    ((= vlaType "AcDbCircle") (setq textContent (vla-get-circumference vlObj)))
    (t nil)
  )

  (if (not (null textContent)) 
    (progn 
      (setq textLocation (getpoint "\n插入文字: "))
      ; (vla-sendcommand activeDoc
      (setq savedEntLast (entlast))
      (command "_text" "j" "mc" textLocation 25 0 textContent)
      (if *searchIncluded* 
        (progn 
          (load "util.lsp")
          (iaso2h:layerSetXline savedEntLast)
        )
      )
    )
  )
  (princ)
)
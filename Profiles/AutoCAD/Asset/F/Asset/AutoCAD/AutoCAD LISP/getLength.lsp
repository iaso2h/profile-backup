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
      (command "_text" "j" "ml" textLocation 25 0 textContent)
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

(defun c:getLengthAverage (/ ss i ent ent-obj length total-length count) 
  ;; Select entities with filter for lines, 2D polylines, arcs, and circles
  (prompt "\n选择要计算平均长度的实体: ")
  (setq ss (ssget "_:L" '((0 . "LINE,LWPOLYLINE,ARC,CIRCLE"))))
  (if ss 
    (progn 
      (setq i 0)
      (setq total-length 0.0)
      (setq count 0)

      ;; Loop through all selected entities
      (repeat (sslength ss) 
        (setq ent (ssname ss i))
        (setq ent-obj (vlax-ename->vla-object ent))

        ;; Get length using ActiveX method (works for all supported object types)
        (setq length (vla-get-length ent-obj))
        (setq total-length (+ total-length length))
        (setq count (1+ count))

        (setq i (1+ i))
      )

      ;; Calculate and display average length
      (if (> count 0) 
        (progn 
          (setq textLocation (getpoint "\n插入文字: "))
          ; (vla-sendcommand activeDoc
          (setq savedEntLast (entlast))
          (command "_text" "j" "ml" textLocation 25 0 (rtos (/ total-length count) 2 12))
          (if *searchIncluded* 
            (progn 
              (load "util.lsp")
              (iaso2h:layerSetXline savedEntLast)
            )
          )
        )
        (princ "\nNo valid entities found")
      )
    )
    (princ "\nNo entities selected")
  )

  (princ)
)

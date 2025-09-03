(defun c:getLength (/ ss ent vlaObj vlaType textLocation textContent savedEntLast) 

  (if (setq ss (ssget "_:S")) 
    (setq ent (ssname ss 0))
    (setq ent (car (entsel)))
  )

  (vl-load-com)
  (setq vlaObj (vlax-ename->vla-object ent))
  (setq vlaType (vla-get-ObjectName vlaObj))

  (cond 
    ((= vlaType "AcDbLine") (setq textContent (vla-get-length vlaObj)))
    ((= vlaType "AcDbPolyline") (setq textContent (vla-get-length vlaObj)))
    ((= vlaType "AcDbArc") (setq textContent (vla-get-arclength vlaObj)))
    ((= vlaType "AcDbCircle") (setq textContent (vla-get-circumference vlaObj)))
    (t nil)
  )

  (if (not (null textContent)) 
    (progn 
      (setq textLocation (getpoint "\n插入文字: "))
      ; (vla-sendcommand activeDoc
      (setq savedEntLast (entlast))
      (command "_text" "j" "ml" textLocation 25 0 textContent)
      (if *SearchIncluded* 
        (progn 
          (if (not *IsLoadedUtil*) 
            (load "util.lsp")
          )
          (iaso2h:layerSetXline savedEntLast)
        )
      )
    )
  )
  (princ)
)


(defun c:getLengthAverage (/ ss i ent vlaObj vlaType totalLength count) 
  ;; Select entities with filter for lines, 2D polylines, arcs, and circles
  (prompt "\n选择要计算平均长度的实体: ")
  (setq ss (ssget "_:L" '((0 . "LINE,LWPOLYLINE,ARC,CIRCLE"))))
  (if ss 
    (progn 
      (setq totalLength 0.0)
      (setq count 0)
      (setq i 0)

      ;; Loop through all selected entities
      (repeat (sslength ss) 
        (setq ent (ssname ss i))
        (setq vlaObj (vlax-ename->vla-object ent))
        (setq vlaType (vla-get-ObjectName vlaObj))

        ;; Get length using ActiveX method (works for all supported object types)
        (cond 
          ((= vlaType "AcDbLine")
           (setq totalLength (+ totalLength (vla-get-length vlaObj)))
          )
          ((= vlaType "AcDbPolyline")
           (setq totalLength (+ totalLength (vla-get-length vlaObj)))
          )
          ((= vlaType "AcDbArc")
           (setq totalLength (+ totalLength (vla-get-arclength vlaObj)))
          )
          ((= vlaType "AcDbCircle")
           (setq totalLength (+ totalLength (vla-get-circumference vlaObj)))
          )
          (t nil)
        )

        (setq count (1+ count))
        (setq i (1+ i))
      )

      ;; Calculate and display average length
      (if (> count 0) 
        (progn 
          (setq textLocation (getpoint "\n插入文字: "))
          (setq savedEntLast (entlast))
          (command "_mtext" 
                   "j"
                   "ml"
                   textLocation
                   25
                   0
                   (rtos (/ totalLength count) 2 12)
          )
          (if *SearchIncluded* 
            (progn 
              (if (not *IsLoadedUtil*) 
                (load "util.lsp")
              )
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

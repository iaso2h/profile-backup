(defun c:getLength (/ ss ent vlaObj vlaType basepoint textContent savedEntLast) 

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
      (setq basepoint (getpoint "\n插入文字: "))

      ; (vla-sendcommand activeDoc
      (setq savedEntLast (entlast))
      (command "_text" 
               "j"
               "ml"
               basepoint
               (* (getVar "viewSize") 0.05)
               0
               textContent
      )
      (if *SearchIncluded* 
        (progn 
          (if (not *IsLoadedUtil*) 
            FFF
            (load "util.lsp")
          )
          (iaso2h:layerSetXline savedEntLast)
        )
      )
    )
  )
  (princ)
)


(defun c:getLengthAverage (/ ss colors legnth i ent vlaObj vlaType totalLength count 
                           basepoint contentComposed savedEntLast
                          ) 
  ;; Select entities with filter for lines, 2D polylines, arcs, and circles
  (prompt "\n选择要计算平均长度的实体: ")
  (setq ss (ssget "_:L" '((0 . "LINE,LWPOLYLINE,ARC,CIRCLE"))))
  (if ss 
    (progn 
      (setq totalLength 0.0)
      (setq count 0)
      (setq i 0)

      ;; Loop through all selected entities
      (setq colors '())
      (setq lengths '())
      (repeat (sslength ss) 
        (setq ent (ssname ss i))
        (setq vlaObj (vlax-ename->vla-object ent))
        (setq vlaType (vla-get-ObjectName vlaObj))

        ;; Get length using ActiveX method (works for all supported object types)
        (cond 
          ((= vlaType "AcDbLine")
           (setq totalLength (+ totalLength (vla-get-length vlaObj)))
           (setq lengths (append lengths (list (vla-get-length vlaObj))))
          )
          ((= vlaType "AcDbPolyline")
           (setq totalLength (+ totalLength (vla-get-length vlaObj)))
           (setq lengths (append lengths (list (vla-get-length vlaObj))))
          )
          ((= vlaType "AcDbArc")
           (setq totalLength (+ totalLength (vla-get-arclength vlaObj)))
           (setq lengths (append lengths (list (vla-get-arclength vlaObj))))
          )
          ((= vlaType "AcDbCircle")
           (setq totalLength (+ totalLength (vla-get-circumference vlaObj)))
           (setq lengths (append lengths (list (vla-get-circumference vlaObj))))
          )
          (t nil)
        )

        (setq count (1+ count))
        (setq i (1+ i))
      )

      ;; Calculate and display average length
      (if (> count 0) 
        (progn 
          (setq basepoint (getpoint "\n插入文字: "))
          (setq oppositePoint (list 
                                (+ (car basepoint) (* (getVar "viewSize") 1))
                                (- (cadr basepoint) (* (getVar "viewSize") 1))
                              )
          )

          (setq savedEntLast (entlast))

          ; Compose text content
          (if (> count 1) 
            (setq contentComposed (strcat 
                                    "平均长度: "
                                    (rtos (/ totalLength count) 2 12)
                                    "\\P"
                                    "总长度: "
                                    (rtos totalLength 2 12)
                                  )
            )
            (setq contentComposed (rtos (/ totalLength count) 2 12))
          )
          (command "_mtext" 
                   basepoint
                   "h"
                   (* (getVar "viewSize") 0.05)
                   oppositePoint
                   contentComposed
                   ""
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

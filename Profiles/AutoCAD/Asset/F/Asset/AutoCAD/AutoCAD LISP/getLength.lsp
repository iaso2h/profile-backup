(defun c:getLength (/ ss doc color colors multiColorChk lengthVal i ent obj vlaType 
                    totalLength basepoint contentComposed content
                   ) 
  ;; Select entities with filter for lines, 2D polylines, arcs, and circles
  (prompt "\n选择要计算平均长度的实体: ")
  (setq ss (ssget "_:L" '((0 . "LINE,LWPOLYLINE,ARC,CIRCLE"))))
  (if (not ss) (exit))

  (if (not *IsLoadedUtil*) 
    (load "util.lsp")
  )
  (setq totalLength 0.0)
  (setq i 0)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq colors '())
  (setq multiColorChk nil)
  (setq lengths '())

  ;; Loop through all selected entities
  (repeat (sslength ss) 
    (setq ent (ssname ss i))
    (setq obj (vlax-ename->vla-object ent))
    (setq vlaType (vla-get-ObjectName obj))

    ;; Get length using ActiveX method (works for all supported object types)
    (cond 
      ((= vlaType "AcDbLine")
       (setq lengthVal (vla-get-length obj))
      )
      ((= vlaType "AcDbPolyline")
       (setq lengthVal (vla-get-length obj))
      )
      ((= vlaType "AcDbArc")
       (setq lengthVal (vla-get-arclength obj))
      )
      ((= vlaType "AcDbCircle")
       (setq lengthVal (vla-get-circumference obj))
      )
      (t nil)
    )

    (setq totalLength (+ totalLength lengthVal))
    (setq lengths (append (list lengthVal) lengths))
    (setq color (vla-get-color obj))
    (cond 
      ((= color 0)
       (setq color 7)
      ) ; Use white for byBlock
      ((= color 256)
       (setq color (vla-get-color 
                     (vla-item (vla-get-layers doc) (vla-get-layer obj))
                   )
       ) ; Use color of layer for byLayer
      )
    )
    (setq colors (append (list color) colors))


    (setq i (1+ i))
  )



  ;; Calculate and display average length
  (setq basepoint (getpoint "\n插入文字: "))
  (setq oppositePoint (list 
                        (+ (car basepoint) (* (getVar "viewSize") 1))
                        (- (cadr basepoint) (* (getVar "viewSize") 1))
                      )
  )

  ; Compose text content
  (if (> i 1) 
    (progn 
      ; Check if colors other than white exists. Prerequisite to add prefix {} to Mtext
      (if (vl-remove 7 (LM:Unique colors)) 
        (setq multiColorChk T)
      )
      (if multiColorChk 
        (setq contentComposed "{")
        (setq contentComposed "")
      )

      (setq i 0) ; Reuse the i for new loop
      (repeat (sslength ss) 
        ; Add color override
        (setq contentComposed (strcat contentComposed 
                                      "\\C"
                                      (itoa (nth i colors))
                                      ";"
                              )
        )
        ; Fill in length value
        (setq contentComposed (strcat contentComposed (rtos (nth i lengths) 2 11)))

        ; Before entering into the next loop
        (setq i (1+ i))
        (setq contentComposed (strcat contentComposed "\\P"))
        (if 
          (and multiColorChk 
               (= i (sslength ss))
          )
          (setq contentComposed (strcat contentComposed "}"))
        )
      )

      ; Compose final  text content
      (setq content (strcat contentComposed 
                            "平均长度: "
                            (rtos (/ totalLength i) 2 11)
                            "\\P"
                            "总长度: "
                            (rtos totalLength 2 11)
                    )
      )

      ; Insert with Mtext command
      (command "_mtext" 
               basepoint
               "h"
               (* (getVar "viewSize") 0.05)
               oppositePoint
               content
               ""
      )
    ) ; end of progn
    (progn 
      (setq content (rtos (/ totalLength i) 2 11))

      (command "_text" 
               "j"
               "ml"
               basepoint
               (* (getVar "viewSize") 0.05)
               0
               content
      )
    )
  )


  ; Set layer of last entity to Xline
  (iaso2h:layerSetXline (entlast))
  (command "_change" "l" "" "p" "la" "xline" "")

  (princ)
)

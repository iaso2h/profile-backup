(defun c:gg (/ firstPoint oldOsnMode gr currentPoint rawAngle quadrant snappedAngle 
             vec rotatedVec oppositeCorner previewVectors newPreview corner2 corner4 
             corner3 boundaryOffset point1_local point2_local point1_rotated 
             point1_global point2_rotated point2_global done
            ) 
  (setq firstPoint (getpoint "\nSpecify first corner: "))
  (if (null firstPoint) (exit))

  (setq oldOsnMode (getvar "osmode"))
  (setvar "osmode" 0)

  (setq previewVectors nil
        done           nil
  )

  (while (not done) 
    (setq gr (grread t))
    (cond 
      ((= (car gr) 5) ; Mouse movement
       (setq currentPoint (cadr gr))
       (setq rawAngle (angle firstPoint currentPoint))
       (setq quadrant (fix (/ (+ rawAngle (* pi 0.25)) (* pi 0.5))))
       (setq snappedAngle (* quadrant (* pi 0.5)))
       (setq snappedAngle (rem snappedAngle (* 2 pi)))

       (setq vec (list 600 400))
       (setq rotatedVec (cond 
                          ((= snappedAngle 0) vec)
                          ((= snappedAngle (* pi 0.5))
                           (list (- (cadr vec)) (car vec))
                          )
                          ((= snappedAngle pi) (list (- (car vec)) (- (cadr vec))))
                          ((= snappedAngle (* pi 1.5))
                           (list (cadr vec) (- (car vec)))
                          )
                        )
       )
       (setq oppositeCorner (mapcar '+ firstPoint rotatedVec))

       ; Calculate rectangle corners
       (setq corner2 (mapcar '+ 
                             firstPoint
                             (list (* 600 (cos snappedAngle)) 
                                   (* 600 (sin snappedAngle))
                             )
                     )
       )
       (setq corner4 (mapcar '+ 
                             firstPoint
                             (list (* 400 (cos (+ snappedAngle (* pi 0.5)))) 
                                   (* 400 (sin (+ snappedAngle (* pi 0.5))))
                             )
                     )
       )
       (setq corner3 oppositeCorner)

       ; Clear ALL previous preview vectors
       (if previewVectors 
         (progn 
           (foreach vec previewVectors 
             (grdraw (car vec) (cadr vec) -1)
           )
           (setq previewVectors nil)
         )
       )

       ; Create new preview list
       (setq newPreview '())

       ; Add rectangle edges to preview
       (setq newPreview (cons (list firstPoint corner2) newPreview))
       (setq newPreview (cons (list corner2 corner3) newPreview))
       (setq newPreview (cons (list corner3 corner4) newPreview))
       (setq newPreview (cons (list corner4 firstPoint) newPreview))

       ; Add internal lines to preview
       (setq boundaryOffset 5)
       (while (<= boundaryOffset 50) 
         (setq point1_local (list boundaryOffset boundaryOffset))
         (setq point2_local (list boundaryOffset (- 400 boundaryOffset)))

         ; Rotate points
         (setq point1_rotated (cond 
                                ((= snappedAngle 0) point1_local)
                                ((= snappedAngle (* pi 0.5))
                                 (list (- (cadr point1_local)) (car point1_local))
                                )
                                ((= snappedAngle pi)
                                 (list (- (car point1_local)) 
                                       (- (cadr point1_local))
                                 )
                                )
                                ((= snappedAngle (* pi 1.5))
                                 (list (cadr point1_local) (- (car point1_local)))
                                )
                              )
         )
         (setq point1_global (mapcar '+ firstPoint point1_rotated))

         (setq point2_rotated (cond 
                                ((= snappedAngle 0) point2_local)
                                ((= snappedAngle (* pi 0.5))
                                 (list (- (cadr point2_local)) (car point2_local))
                                )
                                ((= snappedAngle pi)
                                 (list (- (car point2_local)) 
                                       (- (cadr point2_local))
                                 )
                                )
                                ((= snappedAngle (* pi 1.5))
                                 (list (cadr point2_local) (- (car point2_local)))
                                )
                              )
         )
         (setq point2_global (mapcar '+ firstPoint point2_rotated))

         (setq newPreview (cons (list point1_global point2_global) newPreview))
         (setq boundaryOffset (+ boundaryOffset 5))
       )

       ; Draw new preview and store vectors
       (foreach vec newPreview 
         (grdraw (car vec) (cadr vec) 1)
         (setq previewVectors (cons vec previewVectors))
       )
      )

      ((= (car gr) 3) ; User click
       (setq done t)
      )

      (t ; Exit on any other input
       (setq done t)
      )
    )
  )

  ; Clear final preview
  (if previewVectors 
    (progn 
      (foreach vec previewVectors 
        (grdraw (car vec) (cadr vec) -1)
      )
      (setq previewVectors nil)
    )
  )

  ; Draw final geometry if user clicked
  (if (and (= (car gr) 3) (not (null currentPoint))) 
    (progn 
      ; Draw rectangle
      (entmake 
        (list 
          '(0 . "LWPOLYLINE")
          '(100 . "AcDbEntity")
          '(100 . "AcDbPolyline")
          (cons 90 4)
          (cons 70 1)
          (cons 10 firstPoint)
          (cons 10 corner2)
          (cons 10 corner3)
          (cons 10 corner4)
        )
      )

      ; Draw internal lines
      (setq boundaryOffset 5)
      (while (<= boundaryOffset 50) 
        (setq point1_local (list boundaryOffset boundaryOffset))
        (setq point2_local (list boundaryOffset (- 400 boundaryOffset)))

        (setq point1_rotated (cond 
                               ((= snappedAngle 0) point1_local)
                               ((= snappedAngle (* pi 0.5))
                                (list (- (cadr point1_local)) (car point1_local))
                               )
                               ((= snappedAngle pi)
                                (list (- (car point1_local)) 
                                      (- (cadr point1_local))
                                )
                               )
                               ((= snappedAngle (* pi 1.5))
                                (list (cadr point1_local) (- (car point1_local)))
                               )
                             )
        )
        (setq point1_global (mapcar '+ firstPoint point1_rotated))

        (setq point2_rotated (cond 
                               ((= snappedAngle 0) point2_local)
                               ((= snappedAngle (* pi 0.5))
                                (list (- (cadr point2_local)) (car point2_local))
                               )
                               ((= snappedAngle pi)
                                (list (- (car point2_local)) 
                                      (- (cadr point2_local))
                                )
                               )
                               ((= snappedAngle (* pi 1.5))
                                (list (cadr point2_local) (- (car point2_local)))
                               )
                             )
        )
        (setq point2_global (mapcar '+ firstPoint point2_rotated))

        (entmake 
          (list 
            '(0 . "LINE")
            (cons 10 point1_global)
            (cons 11 point2_global)
          )
        )

        (setq boundaryOffset (+ boundaryOffset 5))
      )
    )
  )

  (setvar "osmode" oldOsnMode)
  (princ)
)
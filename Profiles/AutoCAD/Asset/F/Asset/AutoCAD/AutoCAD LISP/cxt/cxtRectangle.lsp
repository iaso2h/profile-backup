(defun c:gg (/ pt1 pt2 rotAngle previewEnts grResult grType grData dx dy 
             boundaryOffset
            ) 
  (vl-load-com)

  ;; Define boundary offset
  (setq boundaryOffset 5.0)

  ;; Function to rotate point around base point
  (defun rotateAroundPoint (pt basePt angle) 
    (setq cosA (cos angle))
    (setq sinA (sin angle))
    (setq relX (- (car pt) (car basePt)))
    (setq relY (- (cadr pt) (cadr basePt)))
    (list 
      (+ (* relX cosA) (* relY (- sinA)) (car basePt))
      (+ (* relX sinA) (* relY cosA) (cadr basePt))
    )
  )

  ;; Function to get rectangle corners
  (defun getRectCorners (basePt width height angle) 
    (setq pt2 (list (+ (car basePt) width) (cadr basePt)))
    (setq pt3 (list (+ (car basePt) width) (+ (cadr basePt) height)))
    (setq pt4 (list (car basePt) (+ (cadr basePt) height)))
    (list 
      basePt
      (rotateAroundPoint pt2 basePt angle)
      (rotateAroundPoint pt3 basePt angle)
      (rotateAroundPoint pt4 basePt angle)
    )
  )

  ;; Function to get internal line endpoints
  (defun getInternalLinePoints (basePt width height angle) 
    ;; Define points for a VERTICAL line parallel to the shorter side (height)
    (setq ptA (list (+ (car basePt) boundaryOffset) 
                    (+ (cadr basePt) boundaryOffset)
              )
    )
    (setq ptB (list (+ (car basePt) boundaryOffset) 
                    (- (+ (cadr basePt) height) boundaryOffset)
              )
    )
    ;; Return the points after rotating them
    (list 
      (rotateAroundPoint ptA basePt angle)
      (rotateAroundPoint ptB basePt angle)
    )
  )

  ;; Function to draw preview
  (defun drawPreview (corners linePoints) 
    ;; Erase previous preview entities
    (if previewEnts 
      (foreach ent previewEnts 
        (if (and ent (entget ent)) 
          (entdel ent)
        )
      )
    )

    ;; Create new preview entities
    (setq previewEnts nil)

    ;; Main rectangle
    (setq rectEnt (entmake 
                    (append 
                      (list '(0 . "LWPOLYLINE") 
                            '(100 . "AcDbEntity")
                            '(100 . "AcDbPolyline")
                            '(90 . 4)
                            '(70 . 1)
                      )
                      (mapcar '(lambda (pt) (cons 10 pt)) corners)
                    )
                  )
    )

    ;; Internal line
    (setq lineEnt (entmake 
                    (list '(0 . "LINE") 
                          '(100 . "AcDbEntity")
                          '(100 . "AcDbLine")
                          (cons 10 (nth 0 linePoints))
                          (cons 11 (nth 1 linePoints))
                    )
                  )
    )

    ;; Store entity names for later deletion
    (if rectEnt (setq previewEnts (cons (cdr (assoc -1 rectEnt)) previewEnts)))
    (if lineEnt (setq previewEnts (cons (cdr (assoc -1 lineEnt)) previewEnts)))
  )

  ;; Main program
  (princ "\nSelect base point for rectangle: ")
  (setq pt1 (getpoint))

  (if pt1 
    (progn 
      (setq rotAngle 0.0)
      (setq previewPts (getRectCorners pt1 600.0 400.0 rotAngle))
      (setq linePts (getInternalLinePoints pt1 600.0 400.0 rotAngle))
      (drawPreview previewPts linePts)

      ;; Interactive loop
      (while t 
        (setq grResult (grread t 13 0))
        (setq grType (car grResult))
        (setq grData (cadr grResult))

        (cond 
          ;; Mouse movement with ortho mode
          ((= grType 5)
           (setq pt2 grData)
           (if (and pt1 pt2) 
             (progn 
               (setq dx (- (car pt2) (car pt1)))
               (setq dy (- (cadr pt2) (cadr pt1)))

               ;; Snap to 90-degree increments based on mouse position
               (cond 
                 ;; First quadrant
                 ((and (>= dx 0) (>= dy 0))
                  (if (>= dx dy) 
                    (setq rotAngle 0.0) ;; 0 degrees (right)
                    (setq rotAngle (* pi 0.5)) ;; 90 degrees (up)
                  )
                 )
                 ;; Second quadrant
                 ((and (< dx 0) (>= dy 0))
                  (if (>= (abs dx) dy) 
                    (setq rotAngle pi) ;; 180 degrees (left)
                    (setq rotAngle (* pi 0.5)) ;; 90 degrees (up)
                  )
                 )
                 ;; Third quadrant
                 ((and (< dx 0) (< dy 0))
                  (if (>= (abs dx) (abs dy)) 
                    (setq rotAngle pi) ;; 180 degrees (left)
                    (setq rotAngle (* pi 1.5)) ;; 270 degrees (down)
                  )
                 )
                 ;; Fourth quadrant
                 ((and (>= dx 0) (< dy 0))
                  (if (>= dx (abs dy)) 
                    (setq rotAngle 0.0) ;; 0 degrees (right)
                    (setq rotAngle (* pi 1.5)) ;; 270 degrees (down)
                  )
                 )
               )

               (setq previewPts (getRectCorners pt1 600.0 400.0 rotAngle))
               (setq linePts (getInternalLinePoints pt1 600.0 400.0 rotAngle))
               (drawPreview previewPts linePts)
             )
           )
          )

          ;; Left mouse click - confirm
          ((= grType 3)
           (if previewEnts 
             (foreach ent previewEnts (if (and ent (entget ent)) (entdel ent)))
           )
           ;; Create final rectangle
           (command "_.pline" 
                    (nth 0 previewPts)
                    (nth 1 previewPts)
                    (nth 2 previewPts)
                    (nth 3 previewPts)
                    "_close"
           )
           ;; Create final internal line
           (command "_.line" (nth 0 linePts) (nth 1 linePts) "")
           (princ "\nRectangle with internal line created.")
           (exit)
          )

          ;; Right mouse click or ESC - cancel
          ((or (= grType 25) (= grType 2))
           (if previewEnts 
             (foreach ent previewEnts (if (and ent (entget ent)) (entdel ent)))
           )
           (princ "\nOperation cancelled.")
           (exit)
          )
        )
      )
    )
    (princ "\nOperation cancelled.")
  )
  (princ)
)

;; Load the command
(princ "\nType GG to run the enhanced rectangle preview program")
(princ)

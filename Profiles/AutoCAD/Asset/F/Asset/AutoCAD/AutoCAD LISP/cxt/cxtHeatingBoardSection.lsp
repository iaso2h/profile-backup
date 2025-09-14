(defun c:cxt_fq (/ ss ent entdata enttype pt1 pt2 pt3 pt4 pts width length temp areaA 
                 areaB areaC x y exprB exprC ptBL ptTR ptBL_B ptTR_B ptBL_C ptTR_C
                ) 

  ;; Function to check if selected object is a rectangle
  (defun isRectangle (entdata / objtype vertices) 
    (setq objtype (cdr (assoc 0 entdata)))
    (if (= objtype "LWPOLYLINE") 
      (progn 
        (setq vertices (cdr (assoc 90 entdata))) ; vertex count
        (= vertices 4)
      )
      nil
    )
  )

  ;; Get user selection
  (princ "\选择矩形(4个顶点): ")
  (setq ss (ssget '((0 . "LWPOLYLINE"))))

  (if (not ss) 
    (progn 
      (princ "\n没有选择对象。")
      (exit)
    )
    (if (> (sslength ss) 1) 
      (progn 
        (princ "\n请选择一个矩形。")
        (exit)
      )
    )
  )

  ;; Get entity data
  (setq ent (ssname ss 0))
  (setq entdata (entget ent))

  ;; Check if it's a valid rectangle
  (if (not (isRectangle entdata)) 
    (progn 
      (princ "\nSelected object is not a valid rectangle (LWPOLYLINE with 4 vertices).")
      (exit)
    )
  )

  ;; Get rectangle vertices
  (setq pts (getVertices entdata))
  ;; Check if we have exactly 4 points
  (if (/= (vl-list-length pts) 4) 
    (progn 
      (princ "\nSelected object does not have 4 vertices.")
      (exit)
    )
  )

  ;; Sort points to get bottom-left and top-right
  (setq pt1 (nth 0 pts))
  (setq pt2 (nth 1 pts))
  (setq pt3 (nth 2 pts))
  (setq pt4 (nth 3 pts))

  ;; Determine bottom-left and top-right points
  (setq ptBL (list (min (car pt1) (car pt2) (car pt3) (car pt4)) 
                   (min (cadr pt1) (cadr pt2) (cadr pt3) (cadr pt4))
             )
  )
  (setq ptTR (list (max (car pt1) (car pt2) (car pt3) (car pt4)) 
                   (max (cadr pt1) (cadr pt2) (cadr pt3) (cadr pt4))
             )
  )

  ;; Calculate dimensions
  (setq length (abs (- (car ptTR) (car ptBL))))
  (setq width (abs (- (cadr ptTR) (cadr ptBL))))

  ;; Verify it's a rectangle (90-degree angles)
  ; (if (not (isOrthogonalRectangle pts)) 
  ;   (progn 
  ;     (princ "\nSelected object is not an orthogonal rectangle.")
  ;     (exit)
  ;   )
  ; )

  ;; Calculate areas
  (setq areaA (* length width))
  (setq areaB (/ areaA 3.0))
  (setq areaC (* areaA 2.0 (/ 3.0)))

  ;; Calculate margins using the derived formulas
  ;; For rectangle B (1/3 area)
  (setq exprB (- (* 9.0 (+ (* length length) (* width width))) 
                 (* 6.0 length width)
              )
  )
  (if (>= exprB 0) 
    (setq x (/ (- (* 3.0 (+ length width)) (sqrt exprB)) 12.0))
    (progn 
      (princ "\n无法计算矩形B面积。")
      (exit)
    )
  )

  ;; For rectangle C (2/3 area)
  (setq exprC (- (* 36.0 (+ (* length length) (* width width))) 
                 (* -24.0 length width)
              )
  )
  (if (>= exprC 0) 
    (setq y (/ (- (* 6.0 (+ length width)) (sqrt exprC)) 24.0))
    (progn 
      (princ "\n无法计算矩形C面积。")
      (exit)
    )
  )

  ;; Calculate rectangle B coordinates
  (setq ptBL_B (list (+ (car ptBL) x) (+ (cadr ptBL) x)))
  (setq ptTR_B (list (- (car ptTR) x) (- (cadr ptTR) x)))

  ;; Calculate rectangle C coordinates
  (setq ptBL_C (list (+ (car ptBL) y) (+ (cadr ptBL) y)))
  (setq ptTR_C (list (- (car ptTR) y) (- (cadr ptTR) y)))

  ;; Draw rectangles B and C
  (drawRectangle ptBL_B ptTR_B)
  (drawRectangle ptBL_C ptTR_C)

  (princ "\n发热丝分区成功。")
  (princ)
)

;; Function to extract vertices from LWPOLYLINE
(defun getVertices (entdata / vertices n i pt pts ptEntData) 
  (setq vertices '())
  (setq n (cdr (assoc 90 entdata))) ; number of vertices
  (setq ptEntData (vl-remove-if-not '(lambda (x) (= (car x) 10)) 
                                    entdata
                  )
  )
  (setq i 0)
  (repeat n 
    (setq pt (nth i ptEntData)) ; vertex coordinates
    (setq vertices (append vertices 
                           (list 
                             (list 
                               (cadr pt)
                               (caddr pt)
                             )
                           )
                   )
    )

    (setq i (1+ i))
  )

  vertices
)

;; Function to check if 4 points form an orthogonal rectangle
(defun isOrthogonalRectangle (pts / pt1 pt2 pt3 pt4 v1 v2 v3 v4) 
  (setq pt1 (nth 0 pts))
  (setq pt2 (nth 1 pts))
  (setq pt3 (nth 2 pts))
  (setq pt4 (nth 3 pts))

  ;; Calculate vectors
  (setq v1 (list (- (car pt2) (car pt1)) (- (cadr pt2) (cadr pt1))))
  (setq v2 (list (- (car pt3) (car pt2)) (- (cadr pt3) (cadr pt2))))
  (setq v3 (list (- (car pt4) (car pt3)) (- (cadr pt4) (cadr pt3))))
  (setq v4 (list (- (car pt1) (car pt4)) (- (cadr pt1) (cadr pt4))))

  ;; Check if adjacent vectors are perpendicular (dot product = 0)
  (and 
    (= (abs (+ (* (car v1) (car v2)) (* (cadr v1) (cadr v2)))) 0)
    (= (abs (+ (* (car v2) (car v3)) (* (cadr v2) (cadr v3)))) 0)
    (= (abs (+ (* (car v3) (car v4)) (* (cadr v3) (cadr v4)))) 0)
    (= (abs (+ (* (car v4) (car v1)) (* (cadr v4) (cadr v1)))) 0)
  )
)

;; Function to draw rectangle from bottom-left and top-right points
(defun drawRectangle (ptBL ptTR  / pt1 pt2 pt3 pt4) 
  (setq pt1 ptBL)
  (setq pt2 (list (car ptTR) (cadr ptBL)))
  (setq pt3 ptTR)
  (setq pt4 (list (car ptBL) (cadr ptTR)))

  (command "_PLINE" pt1 pt2 pt3 pt4 "C")
)

(princ)
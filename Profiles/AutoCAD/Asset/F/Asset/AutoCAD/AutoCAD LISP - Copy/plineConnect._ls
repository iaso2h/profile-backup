(defun c:connect (/ ent1 ent2 obj1 obj2 enttype1 enttype2 pt1 pt2) 
  (princ "\nConnect entities without FILLET command")

  (while T 
    (setq ent1 nil
          ent2 nil
    )

    ;; Select first entity
    (princ "\nSelect the first entity to connect: ")
    (setq ent1 (car (entsel)))
    (if (null ent1) 
      (progn 
        (princ "\nNo entity selected. Exiting...")
        (exit)
      )
    )

    ;; Get entity type and object
    (setq obj1 (vlax-ename->vla-object ent1))
    (setq enttype1 (vla-get-ObjectName obj1))

    ;; Validate first entity type
    (if (not (member enttype1 '("AcDbLine" "AcDbArc" "AcDbPolyline"))) 
      (progn 
        (princ "\nInvalid entity type. Please select a Line, Arc, or LWPOLYLINE.")
        (continue)
      )
    )

    ;; Select second entity
    (princ "\nSelect the second entity to connect: ")
    (setq ent2 (car (entsel)))
    (if (null ent2) 
      (progn 
        (princ "\nNo entity selected. Exiting...")
        (exit)
      )
    )

    ;; Get entity type and object
    (setq obj2 (vlax-ename->vla-object ent2))
    (setq enttype2 (vla-get-ObjectName obj2))

    ;; Validate second entity type
    (if (not (member enttype2 '("AcDbLine" "AcDbArc" "AcDbPolyline"))) 
      (progn 
        (princ "\nInvalid entity type. Please select a Line, Arc, or LWPOLYLINE.")
        (continue)
      )
    )

    ;; Check if one of the entities is LWPOLYLINE
    (if (or (= enttype1 "AcDbPolyline") (= enttype2 "AcDbPolyline")) 
      ;; Use native JOIN command
      (progn 
        (command "._JOIN" ent1 ent2 "")
        (princ "\nEntities joined using native JOIN command.")
      )
      ;; Connect without FILLET - extend to intersection
      (progn 
        (connect-entities ent1 ent2)
      )
    )
  )

  (princ)
)

(defun connect-entities (ent1 ent2 / obj1 obj2 enttype1 enttype2 pt1start pt1end 
                         pt2start pt2end intersection line1data line2data newpt1 
                         newpt2
                        ) 
  ;; Get entity objects and types
  (setq obj1 (vlax-ename->vla-object ent1))
  (setq obj2 (vlax-ename->vla-object ent2))
  (setq enttype1 (vla-get-ObjectName obj1))
  (setq enttype2 (vla-get-ObjectName obj2))

  ;; Get endpoints for different entity types
  (cond 
    ((= enttype1 "AcDbLine")
     (setq pt1start (vlax-get obj1 'StartPoint))
     (setq pt1end (vlax-get obj1 'EndPoint))
     (setq line1data (list pt1start pt1end))
    )
    ((= enttype1 "AcDbArc")
     (setq pt1start (vlax-get obj1 'StartPoint))
     (setq pt1end (vlax-get obj1 'EndPoint))
     (setq line1data (list pt1start pt1end))
    )
  )

  (cond 
    ((= enttype2 "AcDbLine")
     (setq pt2start (vlax-get obj2 'StartPoint))
     (setq pt2end (vlax-get obj2 'EndPoint))
     (setq line2data (list pt2start pt2end))
    )
    ((= enttype2 "AcDbArc")
     (setq pt2start (vlax-get obj2 'StartPoint))
     (setq pt2end (vlax-get obj2 'EndPoint))
     (setq line2data (list pt2start pt2end))
    )
  )

  ;; Find closest endpoints and extend lines to meet
  (if (and line1data line2data) 
    (progn 
      ;; Find intersection point of infinite lines
      (setq intersection (find-line-intersection 
                           (car line1data)
                           (cadr line1data)
                           (car line2data)
                           (cadr line2data)
                         )
      )

      (if intersection 
        (progn 
          ;; Modify first entity
          (cond 
            ((= enttype1 "AcDbLine")
             (setq newpt1 (find-closest-point intersection pt1start pt1end))
             (if 
               (< (distance intersection pt1start) (distance intersection pt1end))
               (vla-put-StartPoint obj1 intersection)
               (vla-put-EndPoint obj1 intersection)
             )
            )
            ((= enttype1 "AcDbArc")
             ;; For arc, we'll extend by creating a new line segment
             (setq newpt1 (find-closest-point intersection pt1start pt1end))
             (command "._LINE" intersection newpt1 "")
             (command "._ERASE" ent1 "")
            )
          )

          ;; Modify second entity
          (cond 
            ((= enttype2 "AcDbLine")
             (setq newpt2 (find-closest-point intersection pt2start pt2end))
             (if 
               (< (distance intersection pt2start) (distance intersection pt2end))
               (vla-put-StartPoint obj2 intersection)
               (vla-put-EndPoint obj2 intersection)
             )
            )
            ((= enttype2 "AcDbArc")
             ;; For arc, we'll extend by creating a new line segment
             (setq newpt2 (find-closest-point intersection pt2start pt2end))
             (command "._LINE" intersection newpt2 "")
             (command "._ERASE" ent2 "")
            )
          )

          (princ "\nEntities connected at intersection point.")
        )
        (princ "\nLines are parallel, cannot connect.")
      )
    )
  )
)

(defun find-line-intersection (pt1 pt2 pt3 pt4 / x1 y1 x2 y2 x3 y3 x4 y4 denom ua ub 
                               xi yi
                              ) 
  ;; Find intersection of two lines defined by points
  (setq x1 (car pt1)
        y1 (cadr pt1)
        x2 (car pt2)
        y2 (cadr pt2)
        x3 (car pt3)
        y3 (cadr pt3)
        x4 (car pt4)
        y4 (cadr pt4)
  )

  (setq denom (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4))))

  (if (/= denom 0.0) 
    (progn 
      (setq ua (/ (- (* (- x1 x3) (- y3 y4)) (* (- y1 y3) (- x3 x4))) denom))
      (setq ub (/ (- (* (- x1 x3) (- y1 y2)) (* (- y1 y3) (- x1 x2))) denom))
      (setq xi (+ x1 (* ua (- x2 x1))))
      (setq yi (+ y1 (* ua (- y2 y1))))
      (list xi yi 0.0)
    )
  )
)

(defun find-closest-point (refpt pt1 pt2) 
  ;; Find which of two points is closer to reference point
  (if (< (distance refpt pt1) (distance refpt pt2)) 
    pt1
    pt2
  )
)

(defun continue () 
  ;; Dummy function to continue loop
  nil
)

(princ "\nType CONNECT to start connecting entities")
(princ)
s
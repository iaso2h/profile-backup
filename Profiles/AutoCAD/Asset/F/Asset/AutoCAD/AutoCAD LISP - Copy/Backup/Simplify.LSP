;;Program to Simplify Pline vertices given a polyline and Max allowed error.
;;Will remove bulges (arcs).
;;
;;By Steve Carson
;;
;;
;;
;;
 
(vl-load-com)
 
(defun C:Simplify ( / SS MAXERR COUNTS TOT RTOT)
  (setq TOT 0 RTOT 0)
  (princ "\nSelect Polyline(s) to process (<Enter> for all): ")
  (cond
   ((setq SS (ssget '((0 . "POLYLINE,LWPOLYLINE")) )) (princ))
   ((setq SS (ssget "_A" '((0 . "POLYLINE,LWPOLYLINE")) )) (princ))
   (T (princ "\nNo Polylines exist!"))
  )
  (if SS
   (progn
     (setq MAXERR (getreal "\nEnter maximum error: "))
     (if (< (abs MAXERR) 0.00000001)
         (setq MAXERR 0.000000005)
         (setq MAXERR (abs MAXERR))
     );if
     (repeat (sslength SS)
       (setq COUNTS (SC:Simplify MAXERR (ssname SS 0) (sslength SS)))
       (setq TOT (+ (car COUNTS) TOT) RTOT (+ (cdr COUNTS) RTOT))
       (ssdel (ssname SS 0) SS)
     );repeat
     (princ (strcat "\nA total of " (itoa TOT) " vertices were simplified to " (itoa RTOT)))
   );progn
  );if
  (princ)
);defun
 
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Main Simplification code
;;
;;By Steve Carson
;;
 
(defun SC:Simplify ( MaxErr Pline ObjNum / ERR OBJ PL1 PL2 S E EINX CHK NL A SA CNT I)
  
;Set Variables
  (setq ERR MaxErr
        OBJ Pline)
  
  (if (= (cdr (assoc 0 (entget Pline))) "LWPOLYLINE")
    (setq PL1 (SC:IndexPline OBJ))
    (setq PL1 (SC:Index3DPline OBJ))
  )
  
  (setq PL2 (list (car PL1) (last PL1))                       ; New Pline
          S (car PL2)                                         ; First Element of Pline
          E (cadr PL2)                                        ; Last Element of Pline
       EINX (car E)                                           ; Ending Index
        CHK nil
         NL '()
          I 0
  )
 
;Remove Bulges
  (cond 
   ((= (cdr (assoc 0 (entget Pline))) "LWPOLYLINE")
     (repeat (length PL1)
      (vla-SetBulge (vlax-ename->vla-object OBJ) I 0.0)
      (setq I (1+ I))
     )
   )
   ((= (cdr (assoc 0 (entget Pline))) "POLYLINE")
     (if (and (= (vla-get-type (vlax-ename->vla-object OBJ)) 0)
              (vlax-method-applicable-p (vlax-ename->vla-object OBJ) 'SetBulge)
         )
        (repeat (length PL1)
          (vla-SetBulge (vlax-ename->vla-object OBJ) I 0.0)
          (setq I (1+ I))
        )
     );if
   )
  )
 
(if acet-ui-progress (acet-ui-progress (strcat (itoa ObjNum) " objects remaining. Current object progress: ") EINX))
 
(while (null CHK)
 
  (if acet-ui-progress (acet-ui-progress (car S)))
 
 (if (> (- (car E) (car S)) 1)
  (progn
   ;Determine point on PL1 that is farthest away from PL2 
   (setq A (SC:GetMaxDist (cdr S) (cdr E) (SC:ListBetween (car S) (car E) PL1)))
   (cond 
 
    ;If the max distance is less than the max error AND the second element equals the end point, setq CHK to T
    ( (and (< (car A) ERR) (= (car E) EINX))   (setq CHK T) )
 
    ;If the max dist is greater than max error, add point to list and set new point to E
    ( (> (car A) ERR)   (setq PL2 (SC:SortByFirst (append (list (cdr A)) PL2)) E (cdr A)) )
 
    ;If the max dist is less than max error, set S and E to next points
    ( (< (car A) ERR)   (setq S E E (SC:ListNext E PL2))  )
 
   );cond
  );progn
  (if (= (car E) EINX) (setq CHK T) (setq S E E (SC:ListNext E PL2)))
 
 );if
 
);while
 
(if acet-ui-progress (acet-ui-progress))
(setq CNT (length PL2))
 
;Create new Pline from PL2 list
(foreach P (reverse PL2)
  (setq NL (append (cdr P) NL))
);foreach
 
;Make a safearray of the coordinates
(setq SA (vlax-safearray-fill 
            (vlax-make-safearray vlax-vbdouble (cons 0 (1- (length NL))))
                NL
         )
)
 
;Modify Pline
(vlax-put-property (vlax-ename->vla-object OBJ) 'Coordinates (vlax-make-variant SA))
(princ (strcat "\n" (itoa (1+ EINX)) " points simplified to " (itoa CNT)))
(cons (1+ EINX) CNT)
);defun
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Sort a list by first element
;;
;;By Steve Carson
;;
 
(defun SC:SortByFirst (L / )
  (vl-sort L (function (lambda (a b) (< (car a) (car b)))))
)
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Get Maximum Distance
;;By Steve Carson
;;
;;Returns list element and distance that is farthest away from a line drawn between 2 points
;;List needs to be in the form ((1 X1 Y1) (2 X2 Y2) ... (n Xn Yn))
;;Returned list is in the form (d n Xn Yn)
;;Also works for lists including a Z value and returns a list with a Z value.
 
(defun SC:GetMaxDist (p1 p2 lst / d d2 i)
   (setq d 0)
   (foreach l lst
     (if (> (setq d2 (SC:DistToLine (cdr l) p1 p2)) d)
         (setq d d2 i l)
     )
   )
   (cons d i)
 
);defun
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;List between indices
;;By Steve Carson
;;
;;Returns a non-inclusive list of items between 2 indices given 2 indices and a list
;;List needs to be in the form ((1 X1 Y1) (2 X2 Y2) ... (n Xn Yn))
;;or ((1 X1 Y1 Z1) (2 X2 Y2 Z2) ... (n Xn Yn Zn))
 
(defun SC:ListBetween (indx1 indx2 lst / n i l)
  (setq n (1- (- indx2 indx1))
        i indx1
        l '()
  )
  (repeat n
    (setq l (cons (nth (setq i (1+ i)) lst) l))
  )
  (reverse l)
 
)
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Perpendicular Distance of a point (p1) to a line defined by 2 points (p2 p3)
;;By Steve Carson
;;
;;Uses the numerically stable version of "Heron's Formula" shown on Wikipedia to find 
;;the area of the triangle formed by the 3 points, then multiplies it by 2 to get the
;;area of the rectangle, then divides by the length of the line to get the width of the 
;;rectangle, which is the perpendicular distance required.
 
(defun SC:DistToLine ( pt1 pt2 pt3 / LIN A B C A1 B1 C1)
 
 
(if (equal pt2 pt3 0.0001)
(distance pt1 pt2)
(progn
 (setq LIN (distance pt2 pt3) A (distance pt1 pt2) B (distance pt1 pt3) C LIN)
 
;Sorts lengths so A1<=B1<=C1
 (cond 
  ((<= A B C) (setq A1 A B1 B C1 C))
  ((<= A C B) (setq A1 A B1 C C1 B))
  ((<= B A C) (setq A1 B B1 A C1 C))
  ((<= B C A) (setq A1 B B1 C C1 A))
  ((<= C A B) (setq A1 C B1 A C1 B))
  ((<= C B A) (setq A1 C B1 B C1 A))
  (T (setq A1 A B1 B C1 C))
 );cond
 
 (if (and (not (equal A1 0.0 0.0001)) 
          (not (equal B1 0.0 0.0001)) 
          (not (equal C1 0.0 0.0001)))
  (/
          (sqrt
           (abs
            (*
             (+ A1 (+ B1 C1))
             (- C1 (- A1 B1))
             (+ C1 (- A1 B1))
             (+ A1 (- B1 C1))
            );*
           );abs
          );sqrt
 
          (* 2 LIN)
  );/
  0
 );if
);progn
);if
)
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Index Pline vertices
;;By Steve Carson
;;
;;
;;Returns a list of coordinates in the form:
;;((1 X1 Y1) (2 X2 Y2) ... (n Xn Yn))
 
(defun SC:IndexPline (ent / P C1 C2 IDX)
  (setq C1 (vlax-safearray->list 
               (vlax-variant-value 
                   (vla-get-coordinates 
                       (vlax-ename->vla-object ent)
                   )
               )
           )
        IDX 0
        C2 (list (list IDX (car C1) (cadr C1))) 
        C1 (cddr C1)
  )
  (repeat (/ (length C1) 2)
    (setq C2 (cons (list (setq IDX (1+ IDX))  (car C1) (cadr C1)) C2)
          C1 (cddr C1)
    )
  );repeat
  (reverse C2)
);defun
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Index 3DPline vertices
;;By Steve Carson
;;
;;
;;Returns a list of coordinates in the form:
;;((1 X1 Y1 Z1) (2 X2 Y2 Z2) ... (n Xn Yn Zn))
 
(defun SC:Index3DPline (ent / P C1 C2 IDX)
  (setq C1 (vlax-safearray->list 
               (vlax-variant-value 
                   (vla-get-coordinates 
                       (vlax-ename->vla-object ent)
                    )
                )
            )
        IDX 0
        C2 (list (list IDX (car C1) (cadr C1) (caddr C1))) 
        C1 (cdddr C1)
  )
  (repeat (/ (length C1) 3)
    (setq C2 (cons (list (setq IDX (1+ IDX))  (car C1) (cadr C1) (caddr C1)) C2)
          C1 (cdddr C1)
    )
  );repeat
  (reverse C2)
);defun
 
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Next List Element
;;By Steve Carson
;;
;;Given an element and a list, returns the next element in the list.
;;Returns nil if element is last element of list, or is not in the list.
 
(defun SC:ListNext (E L / A N)
  (if (setq A (member E L))
       (progn
        (setq N (1+ (- (length L) (length A))))
        (if (< N (length L))
            (nth N L)
            nil
        )
       )
       nil
  )
);defun
 
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(princ "\nType \"SIMPLIFY\" to invoke.")
(princ)
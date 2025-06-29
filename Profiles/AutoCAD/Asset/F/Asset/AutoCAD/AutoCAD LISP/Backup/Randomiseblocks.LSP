(defun c:randomiseblocks ( / new )
    (vlax-for def (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object)))
        (if (and  (= :vlax-false  (vla-get-islayout def)  (vla-get-isxref def))
                  (wcmatch (vla-get-name def) "~`**")
            )
            (progn
                (while (tblsearch "block" (setq new (randomname 10))))
                (vla-put-name def new)
            )
        )
    )
    (princ)
)

(defun randomname ( n / a b r )
    (setq a 65 b 90)
    (repeat n
        (setq r (cons (chr (LM:randrange a b)) r)
              a (- 113 a)
              b (- 147 b)
        )
    )
    (apply 'strcat (reverse r))
)

;; Rand  -  Lee Mac
;; PRNG implementing a linear congruential generator with
;; parameters derived from the book 'Numerical Recipes'

(defun LM:rand ( / a c m )
    (setq m   4294967296.0
          a   1664525.0
          c   1013904223.0
          $xn (rem (+ c (* a (cond ($xn) ((getvar 'date))))) m)
    )
    (/ $xn m)
)

;; Random in Range  -  Lee Mac
;; Returns a pseudo-random integral number in a given range (inclusive)

(defun LM:randrange ( a b )
    (+ (min a b) (fix (* (LM:rand) (1+ (abs (- a b))))))
)

(vl-load-com) (princ)﻿
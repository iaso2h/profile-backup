(defun biggerEven (value / intPart) 
  "Returns the next even number that is bigger than the given value"
  (setq intPart (fix value))
  (cond 
    ((and (= (rem intPart 2) 0) (= intPart value))
     ;; Even integer exact match
     intPart
    )
    ((= (rem intPart 2) 0)
     ;; Even integer but not exact match
     (+ intPart 2)
    )
    (T
     ;; Odd integer, next even
     (+ intPart 1)
    )
  )
)

(defun biggerOdd (value / intPart) 
  "Returns the next odd number that is bigger than the given value"
  (setq intPart (fix value))
  (setq intPart (fix value))
  (cond 
    ((and (= (rem intPart 2) 0) (= intPart value))
     ;; Even integer exact match
     intPart
    )
    ((= (rem intPart 2) 0)
     ;; Even integer, next even
     (+ intPart 1)
    )
    (T
     ;; Odd integer, but not exact match
     (+ intPart 2)
    )
  )
)

(setq *IsCXTUtilLoaded* T)
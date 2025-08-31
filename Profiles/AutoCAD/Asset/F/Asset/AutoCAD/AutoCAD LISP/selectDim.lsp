(defun c:sed () (selectDim nil) (princ))
(defun c:selectDim () (selectDim nil) (princ))
(defun c:sedd () (selectDim T) (princ))
(defun c:selectDimMore () (selectDim T) (princ))

(defun selectDim (leaderChk / ss) 
  (princ "\n")
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )
  (if leaderChk 
    (if (setq ss (ssget "_:L" '((0 . "*DIMENSION,MULTILEADER")))) 
      (sssetfirst nil ss)
    )
    (if (setq ss (ssget "_:L" '((0 . "*DIMENSION")))) 
      (sssetfirst nil ss)
    )
  )



  (princ)
)

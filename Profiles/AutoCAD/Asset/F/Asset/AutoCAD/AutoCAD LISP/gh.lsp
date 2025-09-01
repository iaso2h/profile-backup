(defun c:gh (/ foo bar) 
  (eval (read "*load-pathname*"))
  vl-filename-directory
)
(defun c:rev ()
  (command "command" "rectang" pause pause)
  (command "REVCLOUD" "s" "c" "a" (/ (getvar "dimscale") 2) "" "o" (entlast) "")
  (princ)
) ;_ end of defun

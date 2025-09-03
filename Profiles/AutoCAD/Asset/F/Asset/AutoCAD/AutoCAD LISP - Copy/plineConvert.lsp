(defun c:2` () (c:plineConvert) (princ))

(defun c:plineConvert (/ ss ent obj) 
  (princ "\nSelect lines to convert to polyline: ")
  (setq ss (ssget '((0 . "LINE")))) ; Select only LINE entities
  (if ss 
    (progn 
      (command "._PEDIT" "_M") ; Start PEDIT command with Multiple option
      (setq i 0)
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (command ent) ; Add each selected entity to PEDIT selection
        (setq i (1+ i))
      )
      (command "" "_J" "" "") ; Confirm selection, Join, and exit PEDIT
      (princ "\nLines converted to polyline.")
    )
    (princ "\nNo lines selected.")
  )
  (princ)
)

(defun c:2` () (c:plineConvert) (princ))

(defun c:plineConvert (/ ss ent obj) 
  (princ "\nSelect lines to convert to polyline: ")
  (setq ss (ssget '((0 . "LINE")))) ; Select only LINE entities
  (if ss 
    (progn 
      (command "._PEDIT" "_M") ; Start PEDIT command with Multiple option
      (setq i 0)
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (command ent) ; Add each selected entity to PEDIT selection
        (setq i (1+ i))
      )
      (command "" "_J" "" "") ; Confirm selection, Join, and exit PEDIT
      (princ "\nLines converted to polyline.")
    )
    (princ "\nNo lines selected.")
  )
  (princ)
)

(defun c:2` () (c:plineConvert) (princ))

(defun c:plineConvert (/ ss ent obj) 
  (princ "\nSelect lines to convert to polyline: ")
  (setq ss (ssget '((0 . "LINE")))) ; Select only LINE entities
  (if ss 
    (progn 
      (command "._PEDIT" "_M") ; Start PEDIT command with Multiple option
      (setq i 0)
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (command ent) ; Add each selected entity to PEDIT selection
        (setq i (1+ i))
      )
      (command "" "_J" "" "") ; Confirm selection, Join, and exit PEDIT
      (princ "\nLines converted to polyline.")
    )
    (princ "\nNo lines selected.")
  )
  (princ)
)

(defun c:2` () (c:plineConvert) (princ))

(defun c:plineConvert (/ ss ent obj) 
  (princ "\nSelect lines to convert to polyline: ")
  (setq ss (ssget '((0 . "LINE")))) ; Select only LINE entities
  (if ss 
    (progn 
      (command "._PEDIT" "_M") ; Start PEDIT command with Multiple option
      (setq i 0)
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (command ent) ; Add each selected entity to PEDIT selection
        (setq i (1+ i))
      )
      (command "" "_J" "" "") ; Confirm selection, Join, and exit PEDIT
      (princ "\nLines converted to polyline.")
    )
    (princ "\nNo lines selected.")
  )
  (princ)
)


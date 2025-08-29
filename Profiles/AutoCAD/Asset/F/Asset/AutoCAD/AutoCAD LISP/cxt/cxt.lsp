(defun c:cx (/ rectObj pts p1 p2 p3 p4 len1 len2 userVal longSide shortSide segVal 
              wireSpace
             ) 
  ;; Prompt user to draw the rectangle outline
  (prompt "\n画出外形轮廓: ")

  ;; Use the internal rectangle command to draw the rectangle
  (command "._rectang" pause pause)

  ;; Get the last entity (the rectangle) created
  (setq rectObj (vlax-ename->vla-object (entlast)))

  ;; Get the coordinates using VLA-get method
  (setq pts (vlax-get rectObj 'Coordinates))

  ;; Extract the first two corner points from the coordinates list
  (setq p1 (list (nth 0 pts) (nth 1 pts))) ; First corner
  (setq p2 (list (nth 2 pts) (nth 3 pts))) ; Second corner

  ;; Calculate the lengths of two adjacent sides of the rectangle
  (setq len1 (distance p1 (list (car p2) (cadr p1)))) ; Length of one side
  (setq len2 (distance (list (car p2) (cadr p1)) p2)) ; Length of adjacent side

  ;; Loop to prompt user for a value and check if it's shorter than both sides
  (princ (rtos len1))
  (princ (rtos len2))
  (while t 
    (prompt "\n输入最边上发热丝的边距: ")
    (setq userVal (getreal))

    ;; Check if the user value is shorter than both sides
    (if (and (< userVal len1) (< userVal len2)) 
      (progn 
        (prompt "\n值已接受.")
        (exit)
      )
      (prompt "\n该值应比外形轮廓短，请重试.")
    )
  )

  ;; Determine the longest and shortest sides
  (if (>= len1 len2) 
    (setq longSide  len1
          shortSide len2
    )
    (setq longSide  len2
          shortSide len1
    )
  )

  ;; Prompt user to enter the number of segments for the long side
  (prompt "\n输入长边的段数: ")
  (setq segVal (getint))

  ;; Calculate the space between wires along the long side
  (setq wireSpace (/ longSide (- segVal 1)))

  ;; Calculate the endpoints for the new lines
  (setq p3 (list (- (car p1) wireSpace) (+ (cadr p1) wireSpace)))
  (setq p4 (list (+ (car p2) wireSpace) (- (cadr p2) wireSpace)))
  (setq p5 (list (+ (car p1) wireSpace) (- (cadr p1) wireSpace)))
  (setq p6 (list (- (car p2) wireSpace) (+ (cadr p2) wireSpace)))

  ;; Draw the first line parallel to the short side
  (command "line" p3 p4 "")
  ;; Draw the second line parallel to the short side
  (command "line" p5 p6 "")

  ;; Exit the function
  (princ)
)

;; Initialize VLAX support
(vl-load-com)

;; Load the function
(princ "\n输入 cxt 绘制矩形并检查边长.")
(princ)
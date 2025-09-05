(defun c:HEATWIRE (/ width height start-x start-y wire-width spacing turns) 
  ;; Get user input
  (setq width (getdist "\nSpecify heating film width: "))
  (setq height (getdist "\nSpecify heating film height: "))
  (setq start-x (getpoint "\nSpecify start point: "))
  (setq wire-width (getdist "\nSpecify wire width (e.g., 0.5): "))
  (setq spacing (getdist "\nSpecify wire spacing (e.g., 1.0): "))

  ;; Calculate parameters
  (setq turns (fix (/ height (+ wire-width spacing))))

  ;; Draw labyrinth
  (command "_PLINE")
  (command start-x)

  (setq current-y (cadr start-x))
  (setq direction 1) ; 1 for right, -1 for left

  (repeat turns 
    ;; Draw horizontal segment
    (setq end-x (+ (car start-x) (* direction (- width wire-width))))
    (command (list end-x current-y))

    ;; Draw vertical connector
    (setq current-y (+ current-y wire-width spacing))
    (command (list end-x current-y))

    ;; Reverse direction
    (setq direction (* direction -1))
  )

  ;; Complete last horizontal segment
  (setq end-x (+ (car start-x) (* direction (- width wire-width))))
  (command (list end-x current-y) "")

  (princ "\nHeating wire labyrinth created!")
  (princ)
)

;; Register command
(princ "\nType HEATWIRE to start heating wire routing design")
(princ)
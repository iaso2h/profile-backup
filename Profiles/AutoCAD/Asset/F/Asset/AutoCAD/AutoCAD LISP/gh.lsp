(defun c:gh (/ foo bar)
  (setq factor 1.0)
  (repeat 18
    (setq factor (+ 0.5 factor))
  )
)
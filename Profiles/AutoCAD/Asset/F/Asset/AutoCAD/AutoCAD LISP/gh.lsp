(defun c:gg (/ insertionPoint centerPoint viewportSize textHeight)
  (setq insertionPoint (getpoint "\nSpecify insertion point: "))
  (setq centerPoint (midpt insertionPoint (getpoint "\nSpecify opposite corner of text window: ")))
  ;; Calculate the viewport size
  (setq viewportSize (distance insertionPoint centerPoint))
  ;; Determine an appropriate text height
  (setq textHeight (/ viewportSize 50))  ;; Adjust the divisor as needed
  ;; Insert the text entity
  (command "_text" insertionPoint "" "foo bar" "" "" textHeight "" "" "")
  (princ "\nText 'foo bar' inserted with adaptive height.")
)
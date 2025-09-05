(defun c:GetLayer0Color (/ acadDoc layers layer0 colorNumber colorName) 
  ;; Get the current AutoCAD document
  (setq acadDoc (vla-get-activedocument (vlax-get-acad-object)))

  ;; Get the layers collection
  (setq layers (vla-get-layers acadDoc))

  ;; Get layer "0"
  (setq layer0 (vla-item layers "0"))

  ;; Get the color number
  (setq colorNumber (vla-get-color layer0))

  ;; Display the result
  (princ (strcat "\nLayer 0 color number: " (itoa colorNumber)))

  ;; Get color name (optional)
  (setq colorName (GetColorName colorNumber))
  (princ (strcat "\nLayer 0 color name: " colorName))

  (princ)
)

;; Helper function to convert color number to color name
(defun GetColorName (colorNumber / colorName) 
  (cond 
    ((= colorNumber 1) "Red")
    ((= colorNumber 2) "Yellow")
    ((= colorNumber 3) "Green")
    ((= colorNumber 4) "Cyan")
    ((= colorNumber 5) "Blue")
    ((= colorNumber 6) "Magenta")
    ((= colorNumber 7) "White")
    ((= colorNumber 8) "Dark Gray")
    ((= colorNumber 9) "Light Gray")
    ((= colorNumber 250) "Orange")
    ((= colorNumber 251) "Light Orange")
    ((= colorNumber 252) "Dark Orange")
    ((= colorNumber 253) "Light Yellow")
    ((= colorNumber 254) "Dark Yellow")
    ((= colorNumber 255) "Light Green")
    ((= colorNumber 256) "Dark Green")
    ((= colorNumber 257) "Light Cyan")
    ((= colorNumber 258) "Dark Cyan")
    ((= colorNumber 259) "Light Blue")
    ((= colorNumber 260) "Dark Blue")
    ((= colorNumber 261) "Light Magenta")
    ((= colorNumber 262) "Dark Magenta")
    ((= colorNumber 263) "Light Red")
    ((= colorNumber 264) "Dark Red")
    (T (strcat "Color " (itoa colorNumber)))
  )
)

;; More robust version with error handling:
(defun c:GetLayer0ColorSafe (/ acadDoc layers layer0 colorNumber) 
  (vl-catch-all-apply 
    '(lambda (/) 
       ;; Get the current AutoCAD document
       (setq acadDoc (vla-get-activedocument (vlax-get-acad-object)))

       ;; Get the layers collection
       (setq layers (vla-get-layers acadDoc))

       ;; Get layer "0"
       (setq layer0 (vla-item layers "0"))

       ;; Get the color number
       (setq colorNumber (vla-get-color layer0))

       ;; Display the result
       (princ (strcat "\nLayer 0 color: " (itoa colorNumber)))

       ;; Also show RGB values if it's a true color
       (if (> colorNumber 255) 
         (princ (strcat " (True Color RGB: " (GetRGBFromColor colorNumber) ")"))
       )

       (princ)
     )
    (list)
  )

  ;; Handle errors
  (if (vl-catch-all-error-p (vl-catch-all-apply 'identity (list nil))) 
    (princ "\nError: Could not retrieve layer 0 color")
  )

  (princ)
)

;; Function to extract RGB values from true color number
(defun GetRGBFromColor (colorNumber / red green blue) 
  (if (> colorNumber 255) 
    (progn 
      (setq red (logand colorNumber 255))
      (setq green (logand (lsh colorNumber -8) 255))
      (setq blue (logand (lsh colorNumber -16) 255))
      (strcat "R:" (itoa red) " G:" (itoa green) " B:" (itoa blue))
    )
    ""
  )
)

;; Function to get any layer's color by name:
(defun c:GetLayerColor (/ layerName acadDoc layers layerObj colorNumber) 
  (setq layerName (getstring "\nEnter layer name: "))

  (if (/= layerName "") 
    (progn 
      (vl-catch-all-apply 
        '(lambda (/) 
           (setq acadDoc (vla-get-activedocument (vlax-get-acad-object)))
           (setq layers (vla-get-layers acadDoc))
           (setq layerObj (vla-item layers layerName))
           (setq colorNumber (vla-get-color layerObj))

           (princ (strcat "\nLayer \"" layerName "\" color: " (itoa colorNumber)))
           (princ (strcat "\nColor name: " (GetColorName colorNumber)))
         )
        (list)
      )

      (if (vl-catch-all-error-p (vl-catch-all-apply 'identity (list nil))) 
        (princ (strcat "\nError: Layer \"" layerName "\" not found"))
      )
    )
    (princ "\nInvalid layer name")
  )

  (princ)
)

;; Function that returns just the color number:
(defun GetLayerColorNumber (layerName / acadDoc layers layerObj colorNumber) 
  (vl-catch-all-apply 
    '(lambda (/) 
       (setq acadDoc (vla-get-activedocument (vlax-get-acad-object)))
       (setq layers (vla-get-layers acadDoc))
       (setq layerObj (vla-item layers layerName))
       (setq colorNumber (vla-get-color layerObj))
       colorNumber
     )
    (list)
  )
)

;; Usage example:
(defun c:TestLayerColor (/ layer0Color) 
  (setq layer0Color (GetLayerColorNumber "0"))

  (if (and layer0Color (not (vl-catch-all-error-p layer0Color))) 
    (progn 
      (princ (strcat "\nLayer 0 color number: " (itoa layer0Color)))
      (princ (strcat "\nColor name: " (GetColorName layer0Color)))
    )
    (princ "\nCould not get layer 0 color")
  )

  (princ)
)

;; Function to list all layer colors:
(defun c:ListAllLayerColors (/ acadDoc layers layerObj) 
  (setq acadDoc (vla-get-activedocument (vlax-get-acad-object)))
  (setq layers (vla-get-layers acadDoc))

  (princ "\n=== Layer Colors ===")

  (vlax-for layerObj layers 
    (princ 
      (strcat "\nLayer: " 
              (vla-get-name layerObj)
              ", Color: "
              (itoa (vla-get-color layerObj))
              ", Color Name: "
              (GetColorName (vla-get-color layerObj))
      )
    )
  )

  (princ)
)

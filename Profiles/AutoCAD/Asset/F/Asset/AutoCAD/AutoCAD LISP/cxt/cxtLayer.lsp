(defun setupLayers (/) 
  (setq layerInfo '('("参照" 6)
                    '("发热丝" 3)
                    '("菲林" 11)
                   )
  )
  (foreach layerList layerInfo 
    (terpri)
    (setq layerName (nth 0 (cadr layerList)))
    (setq layerColor (nth 1 (cadr layerList)))
    (if (not (tblsearch "layer" 
               (car (cadr layerList))
    ))
    (progn
      (command "._-layer" "m" layerName "c" layerColor "" ""))
    )
  )

  (princ)
)

(defun setupPara (/ heatingParamObj) 
  (setq heatingParamObj (vlax-make-object "Scripting.Dictionary")) ; Using Scripting.Dictionary as an example of a generic object

  ;; Setting properties
  (vlax-put-property heatingParamObj 'boundaryOffset 5.5)
  (vlax-put-property heatingParamObj 'heatingGrossLegnth 5.5)
  (vlax-put-property heatingParamObj 'heatingGrossWidth 5.5)
  (vlax-put-property heatingParamObj 'heatingNetLegnth 5.5)
  (vlax-put-property heatingParamObj 'heatingNetWidth 5.5)
  (vlax-put-property heatingParamObj 'resistance 5.5)
  (vlax-put-property heatingParamObj 'resistivity 5.5)
  (vlax-put-property heatingParamObj 'thickness 5.5)
  (vlax-put-property heatingParamObj 'heatingSet 5.5)
  (vlax-put-property heatingParamObj 'heatingWireGrossWidth 5.5)
  (vlax-put-property heatingParamObj 'heatingWireAlongLengthNetLength 5.5)
  (vlax-put-property heatingParamObj 'heatingWireAlongLengthNetWidth 5.5)
  (vlax-put-property heatingParamObj 'thinFilmResistivityMeasurement 5.5)
  (vlax-put-property heatingParamObj 'lengthOfHeatingWire 5.5)
  (vlax-put-property heatingParamObj 'heatingWireAlongLengthCount 5.5)
  (vlax-put-property heatingParamObj 'heatingWireAlongWidthCount 5.5)
  (vlax-put-property heatingParamObj 'heatingWireAlongLengthAxisSpacing 5.5)
  (vlax-put-property heatingParamObj 'heatingWireAlongWidthAxisSpacing 5.5)
  (vlax-put-property heatingParamObj 'heatingWireAlongLengthOutlineSpacing 5.5)
  (vlax-put-property heatingParamObj 'heatingWireAlongWidthOutlineSpacing 5.5)
  (vlax-put-property heatingParamObj 'heatingWireGrossArea 5.5)
  (vlax-put-property heatingParamObj 'heatingWireNetArea 5.5)

  ;; Getting and printing properties
  (princ "\nFilm Width: ")
  (princ (vlax-get-property heatingParamObj 'filmWidth))

  (princ "\nFile Height: ")
  (princ (vlax-get-property heatingParamObj 'fileHeight))

  (princ "\nFile Offset: ")
  (princ (vlax-get-property heatingParamObj 'fileOffset))

  heatingParamObj
)

;; To run the function, type CreateAndSetProperties in the command line and press Enter.

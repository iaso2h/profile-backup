(defun initPara (/ heatingParamObj)
  ; Store in Excel
  (setq heatingParamObj (vlax-create-object "Scripting.Dictionary")) ; Using Scripting.Dictionary as an example of a generic object

  ;; Setting properties
  (vlax-put-property heatingParamObj 'heatingBoundaryOffset nil) ;发热区边框偏移距离
  (vlax-put-property heatingParamObj 'heatingAreaGrossLegnth nil) ;发热区长度
  (vlax-put-property heatingParamObj 'heatingAreaGrossWidth nil) ;发热区宽度
  (vlax-put-property heatingParamObj 'heatingAreaNetLegnth nil) ;发热区净长度
  (vlax-put-property heatingParamObj 'heatingAreaNetWidth nil) ;发热区净宽度
  (vlax-put-property heatingParamObj 'heatingAreaGrossArea nil) ;发热区面积
  (vlax-put-property heatingParamObj 'heatingAreaNetArea nil) ;发热区净面积

  (vlax-put-property heatingParamObj 'heatingWireInitialWidth nil) ;发热丝初设线宽
  (vlax-put-property heatingParamObj 'heatingWireDesignWidth nil) ;发热丝设计线宽
  (vlax-put-property heatingParamObj 'heatingWireRealWidth nil) ;发热丝真实线宽
  (vlax-put-property heatingParamObj 'heatingWireSet nil) ;发热丝组数

  (vlax-put-property heatingParamObj 'resistance nil) ;电阻
  (vlax-put-property heatingParamObj 'resistivity nil) ;电阻率
  (vlax-put-property heatingParamObj 'thickness nil) ;发热丝厚度
  (vlax-put-property heatingParamObj 'thinFilmResistivityMeasurement nil) ;比值
  (vlax-put-property heatingParamObj 'heatingWireLength nil) ;线长

  (vlax-put-property heatingParamObj 'heatingWireAlongLengthCount nil) ;沿长边布线发热丝数
  (vlax-put-property heatingParamObj 'heatingWireAlongLengthAxisSpacing nil) ;沿长边布线发热丝中心距离
  (vlax-put-property heatingParamObj 
                     'heatingWireAlongLengthOutlineGrossSpacing
                     nil
  ) ;沿长边布线发热丝边缘距离
  (vlax-put-property heatingParamObj 
                     'heatingWireAlongLengthOutlineDeisgnSpacing
                     nil
  ) ;沿长边布线发热丝边缘设计距离
  (vlax-put-property heatingParamObj 
                     'heatingWireAlongLengthOutlineDeisgnSpacing
                     nil
  ) ;沿长边布线发热丝设计线宽间距比值

  (vlax-put-property heatingParamObj 'heatingWireAlongWidthCount nil) ;沿短边布线发热丝数
  (vlax-put-property heatingParamObj 'heatingWireAlongWidthAxisSpacing nil) ;沿短边布线发热丝中心距离
  (vlax-put-property heatingParamObj 
                     'heatingWireAlongWidthOutlineGrossSpacing
                     nil
  ) ;沿短边布线发热丝边缘距离
  (vlax-put-property heatingParamObj 
                     'heatingWireAlongWidthOutlineDeisgnSpacing
                     nil
  ) ;沿短边布线发热丝边缘设计距离
  (vlax-put-property heatingParamObj 
                     'heatingWireAlongWidthOutlineDeisgnSpacing
                     nil
  ) ;沿短边布线发热丝设计线宽间距比值

  heatingParamObj
)

(defun readFromCSV (heatingParamObj) 
  (if 
    (null 
      *IsLoadedCXTCSV*
    )
    (load "cxtCSV.lsp")
  )
  (setq csvData (readCSVFile))
  (vlax-put-property heatingParamObj 
                     'heatingBoundaryOffset
                     (getCellValue (csvData "H2"))
  )
  (vlax-put-property heatingParamObj 
                     'heatingAreaGrossLegnth
                     (getCellValue (csvData "I2"))
  )
  (vlax-put-property heatingParamObj 
                     'heatingAreaGrossWidth
                     (getCellValue (csvData "I3"))
  )
  (vlax-put-property heatingParamObj 
                     'heatingAreaNetLegnth
                     (getCellValue (csvData "F2"))
  )
  (vlax-put-property heatingParamObj 
                     'heatingAreaNetWidth
                     (getCellValue (csvData "F3"))
  )

  (vlax-put-property heatingParamObj 'resistance (getCellValue (csvData "C2")))
  (vlax-put-property heatingParamObj 'resistivity (getCellValue (csvData "C3")))
  (vlax-put-property heatingParamObj 'thickness (getCellValue (csvData "C4")))
  (vlax-put-property heatingParamObj 'heatingWireSet (getCellValue (csvData "F4")))
  (vlax-put-property heatingParamObj 
                     'heatingWireInitialWidth
                     (getCellValue (csvData "F5"))
  )
  ; (vlax-put-property heatingParamObj 'heatingWireAlongLengthNetLength (getCellValue (csvData "address")))
  ; (vlax-put-property heatingParamObj 'heatingWireAlongLengthNetWidth (getCellValue (csvData "address")))
  (vlax-put-property heatingParamObj 
                     'thinFilmResistivityMeasurement
                     (getCellValue (csvData "C5"))
  )
  (vlax-put-property heatingParamObj 
                     'heatingWireLength
                     (getCellValue (csvData "F6"))
  )

  (vlax-put-property heatingParamObj 
                     'heatingWireAlongLengthCount
                     (getCellValue (csvData "E9"))
  )
  (vlax-put-property heatingParamObj 
                     'heatingWireAlongLengthAxisSpacing
                     (getCellValue (csvData "E10"))
  )
  (vlax-put-property heatingParamObj 
                     'heatingWireAlongLengthOutlineGrossSpacing
                     (getCellValue (csvData "E11"))
  )
  (vlax-put-property heatingParamObj 
                     'heatingWireAlongWidthCount
                     (getCellValue (csvData "B9"))
  )
  (vlax-put-property heatingParamObj 
                     'heatingWireAlongWidthAxisSpacing
                     (getCellValue (csvData "B10"))
  )
  (vlax-put-property heatingParamObj 
                     'heatingWireAlongWidthOutlineGrossSpacing
                     (getCellValue (csvData "B11"))
  )

  (vlax-put-property heatingParamObj 
                     'heatingAreaGrossArea
                     (getCellValue (csvData "E14"))
  )
  (vlax-put-property heatingParamObj 
                     'heatingAreaNetArea
                     (getCellValue (csvData "E15"))
  )
  (vlax-put-property heatingParamObj 
                     'heatingWireNetAreaProportion
                     (getCellValue (csvData "E16"))
  )



  (princ (vlax-put-property heatingParamObj 'heatingBoundaryOffset))
  (terpri)

  (princ (vlax-put-property heatingParamObj 'heatingAreaGrossLegnth))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'heatingAreaGrossWidth))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'heatingAreaGrossLegnth))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'heatingAreaGrossWidth))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'heatingAreaNetLegnth))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'heatingAreaNetWidth))
  (terpri)

  (princ (vlax-put-property heatingParamObj 'resistance))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'resistivity))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'thickness))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'heatingWireSet))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'heatingWireInitialWidth))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'thinFilmResistivityMeasurement))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'heatingWireLength))
  (terpri)

  (princ (vlax-put-property heatingParamObj 'heatingWireAlongLengthCount))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'heatingWireAlongWidthCount))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'heatingWireAlongLengthAxisSpacing))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'heatingWireAlongWidthAxisSpacing))
  (terpri)
  (princ 
    (vlax-put-property heatingParamObj 'heatingWireAlongLengthOutlineGrossSpacing)
  )
  (terpri)
  (princ 
    (vlax-put-property heatingParamObj 'heatingWireAlongWidthOutlineGrossSpacing)
  )
  (terpri)

  (princ (vlax-put-property heatingParamObj 'heatingAreaGrossArea))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'heatingAreaNetArea))
  (terpri)
  (princ (vlax-put-property heatingParamObj 'heatingWireNetAreaProportion))
  (terpri)
)

(defun c:gh () 
  (setq heatingParamObj (initPara))
  (readFromCSV heatingParamObj)
  (princ)
)
(defun CXTInitPara (/) 
  ; Return T if successfully initialized, otherwise return nil.
  (if (not *IsLoadedCsvParser*) 
    (load "csvParser")
  )
  (setq useEvenNumber T)
  (setq csvData (readCSVFile))
  (if csvData 
    (progn 
      ;; Setting properties
      (setq *CXTHeatingBoundaryOffset* (atof (getCellValue csvData "H2"))) ;发热区边框偏移距离
      (setq *CXTHeatingAreaGrossLegnth* (atof (getCellValue csvData "I2"))) ;发热区长度
      (setq *CXTHeatingAreaGrossWidth* (atof (getCellValue csvData "I3"))) ;发热区宽度
      (setq *CXTHeatingAreaNetLength* (- *CXTHeatingAreaGrossLegnth* 
                                         (* 2 *CXTHeatingBoundaryOffset*)
                                      )
      ) ;发热区净长度
      (setq *CXTHeatingAreaNetWidth* (- *CXTHeatingAreaGrossWidth* 
                                        (* 2 *CXTHeatingBoundaryOffset*)
                                     )
      ) ;发热区净宽度
      (setq *CXTHeatingAreaGrossArea* (* *CXTHeatingAreaGrossLegnth* 
                                         *CXTHeatingAreaGrossWidth*
                                      )
      ) ;发热区面积
      (setq *CXTHeatingAreaRealArea* (atof (getCellValue csvData "E15"))) ;发热区实际面积
      (setq *CXTHeatingWireLossBasic* (atof (getCellValue csvData "F17")))
      (setq *CXTHeatingWireLossTotal* (+ 
                                        (- 1 
                                           (/ 
                                             *CXTHeatingAreaGrossArea*
                                             *CXTHeatingAreaRealArea*
                                           )
                                        )
                                        *CXTHeatingWireLossBasic*
                                      )
      ) ;发热丝损耗

      (setq *CXTHeatingWireInitialWidth* (atof (getCellValue csvData "F5"))) ;发热丝初设线宽
      (setq *CXTHeatingWireDesignWidth* (* (- 1 *CXTHeatingWireLossTotal*) 
                                           *CXTHeatingWireInitialWidth*
                                        )
      ) ;发热丝设计线宽
      (setq *CXTHeatingWireRealWidth* nil) ;发热丝真实线宽
      (setq *CXTHeatingWireSet* (atoi (getCellValue csvData "F4"))) ;发热丝组数
      (setq *CXTHeatingWireResistance* (atof (getCellValue csvData "C2"))) ;电阻
      (setq *CXTHeatingWireResistivity* (atof (getCellValue csvData "C3"))) ;电阻率
      (setq *CXTHeatingWireThickness* (atof (getCellValue csvData "C4"))) ;发热丝厚度
      (setq *CXTHeatingWireThinFilmResistivityMeasurement* (* 1000 
                                                              (/ 
                                                                (* 
                                                                  *CXTHeatingWireResistance*
                                                                  *CXTHeatingWireThickness*
                                                                )
                                                                *CXTHeatingWireResistivity*
                                                              )
                                                           )
      ) ;比值
      (setq *CXTHeatingWireLength* (* 
                                     (* (* *CXTHeatingWireSet* *CXTHeatingWireSet*) 
                                        *CXTHeatingWireInitialWidth*
                                     )
                                     *CXTHeatingWireThinFilmResistivityMeasurement*
                                   )
      ) ;线长
      (if useEvenNumber 
        (setq *CXTHeatingWireAlongAreaLengthCount* (iaso2h:biggerEven 
                                                     (/ 
                                                       *CXTHeatingWireLength*
                                                       *CXTHeatingAreaNetWidth*
                                                     )
                                                   )
        ) ;沿长边布线发热丝数(偶数)
        (setq *CXTHeatingWireAlongAreaLengthCount* (iaso2h:biggerOdd 
                                                     (/ 
                                                       *CXTHeatingWireLength*
                                                       *CXTHeatingAreaNetWidth*
                                                     )
                                                   )
        ) ;沿长边布线发热丝数(奇数)
      )
      (setq *CXTHeatingWireAlongAreaLengthAxisSpacing* (/ 
                                                         *CXTHeatingAreaNetLength*
                                                         (- *CXTHeatingWireAlongAreaLengthCount* 
                                                            1
                                                         )
                                                       )
      ) ;沿长边布线发热丝中心线距离

      (setq *CXTHeatingWireAlongAreaLengthOutlineGrossSpacing* (- *CXTHeatingWireAlongAreaLengthAxisSpacing* 
                                                                  *CXTHeatingWireInitialWidth*
                                                               )
      ) ;沿长边布线发热丝边缘距离
      (setq *CXTHeatingWireAlongAreaLengthOutlineDesignSpacing* (- *CXTHeatingWireAlongAreaLengthAxisSpacing* 
                                                                   *CXTHeatingWireDesignWidth*
                                                                )
      ) ;沿长边布线发热丝边缘设计距离
      (setq *CXTHeatingWireAlongAreaLengthWireWidthSpacingRatio* (/ 
                                                                   *CXTHeatingWireDesignWidth*

                                                                   *CXTHeatingWireAlongAreaLengthOutlineDesignSpacing*
                                                                 )
      ) ;沿长边布线发热丝设计线宽间距比值

      (if useEvenNumber 
        (setq *CXTHeatingWireAlongAreaWidthCount* (iaso2h:biggerEven 
                                                    (/ 
                                                      *CXTHeatingWireLength*
                                                      *CXTHeatingAreaNetLength*
                                                    )
                                                  )
        ) ;沿短边布线发热丝数(偶数)
        (setq *CXTHeatingWireAlongAreaWidthCount* (iaso2h:biggerOdd 
                                                    (/ 
                                                      *CXTHeatingWireLength*
                                                      *CXTHeatingAreaNetLength*
                                                    )
                                                  )
        ) ;沿短边布线发热丝数(奇数)
      )
      (setq *CXTHeatingWireAlongAreaWidthAxisSpacing* (/ 
                                                        *CXTHeatingAreaNetWidth*
                                                        (- *CXTHeatingWireAlongAreaWidthCount* 
                                                           1
                                                        )
                                                      )
      ) ;沿短边布线发热丝中心线距离
      (setq *CXTHeatingWireAlongAreaWidthOutlineGrossSpacing* (- *CXTHeatingWireAlongAreaWidthAxisSpacing* 
                                                                 *CXTHeatingWireInitialWidth*
                                                              )
      ) ;沿短边布线发热丝边缘距离
      (setq *CXTHeatingWireAlongAreaWidthOutlineDesignSpacing* (- *CXTHeatingWireAlongAreaWidthAxisSpacing* 
                                                                  *CXTHeatingWireDesignWidth*
                                                               )
      ) ;沿短边布线发热丝边缘设计距离
      (setq *CXTHeatingWireAlongAreaWidthWireWidthSpacingRatio* (/ 
                                                                  *CXTHeatingWireDesignWidth*

                                                                  *CXTHeatingWireAlongAreaWidthOutlineDesignSpacing*
                                                                )
      ) ;沿短边布线发热丝设计线宽间距比值
      (CXTPrintPrimaryPara)
      T ; Return T if successfully initialized.
    )
    nil
  )
  
  
)

(defun CXTPrintPrimaryPara (/) 
  ; Print primary parameters.
  (terpri)
  (princ "发热区长x宽: ")
  (princ *CXTHeatingAreaGrossLegnth*)
  (princ "x ")
  (princ *CXTHeatingAreaGrossWidth*)
  (princ "mm, 实际面积: ")
  (princ (rtos *CXTHeatingAreaRealArea* 2))
  (princ "mm^2\n")

  (princ "\n")
  (princ "电阻: ")
  (princ *CXTHeatingWireResistance*)
  (princ ", ")
  (princ "电阻率: ")
  (princ *CXTHeatingWireResistivity*)
  (princ ", ")
  (princ "发热丝厚度: ")
  (princ *CXTHeatingWireThickness*)
  (princ "\n")

  (princ "发热区边框偏移距离: ")
  (princ *CXTHeatingBoundaryOffset*)
  (princ "\n")
  (princ "发热丝组数: ")
  (princ *CXTHeatingWireSet*)
  (princ "\n")
  (princ "发热丝假设线宽: ")
  (princ (rtos *CXTHeatingWireInitialWidth* 2 12))
  (princ "\n")
  (princ "发热丝设变线宽: ")
  (princ (rtos *CXTHeatingWireDesignWidth* 2 12))
  (princ "\n")
  (princ "发热丝设变间距(偶数、沿长边): ")
  (princ (rtos *CXTHeatingWireAlongAreaLengthWireWidthSpacingRatio* 2 12))
  (princ ", 发热丝设变间距(偶数、沿短边): ")
  (princ (rtos *CXTHeatingWireAlongAreaWidthWireWidthSpacingRatio* 2 12))
  (princ "\n")
  (princ "发热丝设变线宽/间距比值(偶数、沿长边): ")
  (princ (rtos *CXTHeatingWireAlongAreaLengthWireWidthSpacingRatio* 2 12))
  (princ ", 发热丝设变线宽/间距比值(偶数、沿短边): ")
  (princ (rtos *CXTHeatingWireAlongAreaWidthWireWidthSpacingRatio* 2 12))
  (princ "\n")
  
  
  (princ)
)

(defun CXTTestPara (/) 
  (princ "发热区边框偏移距离: ")
  (princ *CXTHeatingBoundaryOffset*)
  (princ "\n")
  (princ "发热区长度: ")
  (princ *CXTHeatingAreaGrossLegnth*)
  (princ "\n")
  (princ "发热区宽度: ")
  (princ *CXTHeatingAreaGrossWidth*)
  (princ "\n")
  (princ "发热区净长度: ")
  (princ *CXTHeatingAreaNetLength*)
  (princ "\n")
  (princ "发热区净宽度: ")
  (princ *CXTHeatingAreaNetWidth*)
  (princ "\n")
  (princ "发热区面积: ")
  (princ *CXTHeatingAreaGrossArea*)
  (princ "\n")
  (princ "发热区净面积: ")
  (princ *CXTHeatingAreaRealArea*)
  (princ "\n")
  (princ "发热丝基本损耗: ")
  (princ *CXTHeatingWireLossBasic*)
  (princ "\n")
  (princ "发热丝总损耗: ")
  (princ *CXTHeatingWireLossTotal*)
  (princ "\n")
  (princ "发热丝初设线宽: ")
  (princ *CXTHeatingWireInitialWidth*)
  (princ "\n")
  (princ "发热丝设计线宽: ")
  (princ *CXTHeatingWireDesignWidth*)
  (princ "\n")
  (princ "发热丝真实线宽: ")
  (princ *CXTHeatingWireRealWidth*)
  (princ "\n")
  (princ "发热丝组数: ")
  (princ *CXTHeatingWireSet*)
  (princ "\n")
  (princ "电阻: ")
  (princ *CXTHeatingWireResistance*)
  (princ "\n")
  (princ "电阻率: ")
  (princ *CXTHeatingWireResistivity*)
  (princ "\n")
  (princ "发热丝厚度: ")
  (princ *CXTHeatingWireThickness*)
  (princ "\n")
  (princ "比值: ")
  (princ *CXTHeatingWireThinFilmResistivityMeasurement*)
  (princ "\n")
  (princ "线长: ")
  (princ *CXTHeatingWireLength*)
  (princ "\n")
  (princ "沿长边布线发热丝数: ")
  (princ *CXTHeatingWireAlongAreaLengthCount*)
  (princ "\n")
  (princ "沿长边布线发热丝中心距离: ")
  (princ *CXTHeatingWireAlongAreaLengthAxisSpacing*)
  (princ "\n")
  (princ "沿长边布线发热丝边缘距离: ")
  (princ *CXTHeatingWireAlongAreaLengthOutlineGrossSpacing*)
  (princ "\n")
  (princ "沿长边布线发热丝边缘设计距离: ")
  (princ *CXTHeatingWireAlongAreaLengthOutlineDesignSpacing*)
  (princ "\n")
  (princ "沿长边布线发热丝设计线宽间距比值: ")
  (princ *CXTHeatingWireAlongAreaLengthWireWidthSpacingRatio*)
  (princ "\n")
  (princ "沿短边布线发热丝数: ")
  (princ *CXTHeatingWireAlongAreaWidthCount*)
  (princ "\n")
  (princ "沿短边布线发热丝中心距离: ")
  (princ *CXTHeatingWireAlongAreaWidthAxisSpacing*)
  (princ "\n")
  (princ "沿短边布线发热丝边缘距离: ")
  (princ *CXTHeatingWireAlongAreaWidthOutlineGrossSpacing*)
  (princ "\n")
  (princ "沿短边布线发热丝边缘设计距离: ")
  (princ *CXTHeatingWireAlongAreaWidthOutlineDesignSpacing*)
  (princ "\n")
  (princ "沿短边布线发热丝设计线宽间距比值: ")
  (princ *CXTHeatingWireAlongAreaWidthWireWidthSpacingRatio*)
  (princ "\n")
)

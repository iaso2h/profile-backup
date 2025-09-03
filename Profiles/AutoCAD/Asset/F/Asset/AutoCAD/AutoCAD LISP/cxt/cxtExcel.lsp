(defun c:gh (/ filename excelApp objBook objSheet objRng cellVal) 
  ; https://qiita.com/tamarinn_x/items/753e16221d2324b848b0
  (vl-load-com)

  (setq filename (getfiled "选择异型线路设计文件" "" "xls;xlsx" 16))
  (if filename 
    (progn 
      ; Get excel application
      ; (setq excelApp (vlax-get-or-create-object "Ket.Application"))
      (setq excelApp (vlax-get-object "Excel.Application"))
      (if (not excelApp) 
        (progn 
          (vla-put-visible excelApp 1)
          (vlax-invoke-method (vlax-get-property excelApp 'workbooks) 
                              'open
                              filename
          )
          (setq objBook  (vlax-get-property excelApp 'ActiveWorkbook)
                objSheet (vlax-get-property (vlax-get-property objBook 'Worksheets) 
                                            'Item
                                            1
                         )
                objRng   (vlax-get-property objSheet 'Range "A2")
                cellVal  (vlax-get-property objRng 'Value)
          )

          (vlax-release-object objBook)
          (vlax-release-object objSheet)
          (vlax-release-object objRng)
          
          (princ (vlax-variant-value cellVal))
          (terpri)
        )
      )
    )
    (princ "\nError: Could not create Excel application object.")
  )

  (princ)
)
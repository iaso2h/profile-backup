(defun c:cxtf (/ filename excel-app workbook worksheet cell-value) 
  ;; Step 1: Open file dialog to select Excel file
  (setq filename (getfiled "选择异型线路设计文件" "" "xls" 0))

  (if (not filename) 
    (progn 
      (princ "\n没有文件被选择。")
      (princ)
      (return)
    )
  )

  ;; Step 2: Start Excel Application via ActiveX
  (setq excel-app (vlax-create-object "Excel.Application"))
  (if (not excel-app) 
    (progn 
      (princ "\nError: Could not create Excel application object.")
      (princ)
      (return)
    )
  )

  ;; Optional: Make Excel visible (set to T if you want to see it)
  (vlax-put-property excel-app 'Visible 0) ; 0 = False, 1 = True

  (vl-catch-all-apply 
    '(lambda () 
       ;; Step 3: Open the workbook
       (setq workbook (vlax-invoke-method excel-app 'Workbooks 'Open filename))
       (princ "123\n")

       ;; Step 4: Get the first worksheet
       (setq worksheet (vlax-get-property workbook 'Sheets 1))
       (princ "124\n")

       ;; Step 5: Read a cell (e.g., A1)
       (setq cell-value (vlax-variant-value 
                          (vlax-get-property worksheet 'Range "A1" 'Value)
                        )
       )
       (princ "125\n")

       ;; Step 6: Print result
       (if (null cell-value) 
         (princ "\nCell A1 is empty.")
         (princ (strcat "\nValue from A1: " (vl-princ-to-string cell-value)))
       )
     )
  )

  ;; Step 7: Error handling and cleanup
  (vl-catch-all-apply 
    '(lambda () 
       ;; You can choose to close Excel or leave it open
       ;; (vlax-invoke-method workbook 'Close)
       ;; (vlax-release-object workbook)
       ;; (vlax-invoke-method excel-app 'Quit)
       ;; (vlax-release-object excel-app)

       ;; Optional: Release objects (recommended)
       (vlax-release-object worksheet)
       (vlax-release-object workbook)
       (vlax-release-object excel-app)
     )
  )

  (princ)
)
(defun c:cxtf (/ filename excel-app workbook worksheet cell-value) 
  ;; Step 1: Open file dialog to select Excel file
  (setq filename (getfiled "选择异型线路设计文件" "" "xls" 0))

  (if (not filename) 
    (progn 
      (princ "\n没有文件被选择。")
      (princ)
      (return)
    )
  )

  ;; Step 2: Start Excel Application via ActiveX
  (setq excel-app (vlax-create-object "Excel.Application"))
  (if (not excel-app) 
    (progn 
      (princ "\nError: Could not create Excel application object.")
      (princ)
      (return)
    )
  )

  ;; Optional: Make Excel visible (set to T if you want to see it)
  (vlax-put-property excel-app 'Visible 0) ; 0 = False, 1 = True

  (vl-catch-all-apply 
    '(lambda () 
       ;; Step 3: Open the workbook
       (setq workbook (vlax-invoke-method excel-app 'Workbooks 'Open filename))
       (princ "123\n")

       ;; Step 4: Get the first worksheet
       (setq worksheet (vlax-get-property workbook 'Sheets 1))
       (princ "124\n")

       ;; Step 5: Read a cell (e.g., A1)
       (setq cell-value (vlax-variant-value 
                          (vlax-get-property worksheet 'Range "A1" 'Value)
                        )
       )
       (princ "125\n")

       ;; Step 6: Print result
       (if (null cell-value) 
         (princ "\nCell A1 is empty.")
         (princ (strcat "\nValue from A1: " (vl-princ-to-string cell-value)))
       )
     )
  )

  ;; Step 7: Error handling and cleanup
  (vl-catch-all-apply 
    '(lambda () 
       ;; You can choose to close Excel or leave it open
       ;; (vlax-invoke-method workbook 'Close)
       ;; (vlax-release-object workbook)
       ;; (vlax-invoke-method excel-app 'Quit)
       ;; (vlax-release-object excel-app)

       ;; Optional: Release objects (recommended)
       (vlax-release-object worksheet)
       (vlax-release-object workbook)
       (vlax-release-object excel-app)
     )
  )

  (princ)
)
(defun c:cxtf (/ filename excel-app workbook worksheet cell-value) 
  ;; Step 1: Open file dialog to select Excel file
  (setq filename (getfiled "选择异型线路设计文件" "" "xls" 0))

  (if (not filename) 
    (progn 
      (princ "\n没有文件被选择。")
      (princ)
      (return)
    )
  )

  ;; Step 2: Start Excel Application via ActiveX
  (setq excel-app (vlax-create-object "Excel.Application"))
  (if (not excel-app) 
    (progn 
      (princ "\nError: Could not create Excel application object.")
      (princ)
      (return)
    )
  )

  ;; Optional: Make Excel visible (set to T if you want to see it)
  (vlax-put-property excel-app 'Visible 0) ; 0 = False, 1 = True

  (vl-catch-all-apply 
    '(lambda () 
       ;; Step 3: Open the workbook
       (setq workbook (vlax-invoke-method excel-app 'Workbooks 'Open filename))
       (princ "123\n")

       ;; Step 4: Get the first worksheet
       (setq worksheet (vlax-get-property workbook 'Sheets 1))
       (princ "124\n")

       ;; Step 5: Read a cell (e.g., A1)
       (setq cell-value (vlax-variant-value 
                          (vlax-get-property worksheet 'Range "A1" 'Value)
                        )
       )
       (princ "125\n")

       ;; Step 6: Print result
       (if (null cell-value) 
         (princ "\nCell A1 is empty.")
         (princ (strcat "\nValue from A1: " (vl-princ-to-string cell-value)))
       )
     )
  )

  ;; Step 7: Error handling and cleanup
  (vl-catch-all-apply 
    '(lambda () 
       ;; You can choose to close Excel or leave it open
       ;; (vlax-invoke-method workbook 'Close)
       ;; (vlax-release-object workbook)
       ;; (vlax-invoke-method excel-app 'Quit)
       ;; (vlax-release-object excel-app)

       ;; Optional: Release objects (recommended)
       (vlax-release-object worksheet)
       (vlax-release-object workbook)
       (vlax-release-object excel-app)
     )
  )

  (princ)
)
(defun c:cxtf (/ filename excel-app workbook worksheet cell-value) 
  ;; Step 1: Open file dialog to select Excel file
  (setq filename (getfiled "选择异型线路设计文件" "" "xls" 0))

  (if (not filename) 
    (progn 
      (princ "\n没有文件被选择。")
      (princ)
      (return)
    )
  )

  ;; Step 2: Start Excel Application via ActiveX
  (setq excel-app (vlax-create-object "Excel.Application"))
  (if (not excel-app) 
    (progn 
      (princ "\nError: Could not create Excel application object.")
      (princ)
      (return)
    )
  )

  ;; Optional: Make Excel visible (set to T if you want to see it)
  (vlax-put-property excel-app 'Visible 0) ; 0 = False, 1 = True

  (vl-catch-all-apply 
    '(lambda () 
       ;; Step 3: Open the workbook
       (setq workbook (vlax-invoke-method excel-app 'Workbooks 'Open filename))
       (princ "123\n")

       ;; Step 4: Get the first worksheet
       (setq worksheet (vlax-get-property workbook 'Sheets 1))
       (princ "124\n")

       ;; Step 5: Read a cell (e.g., A1)
       (setq cell-value (vlax-variant-value 
                          (vlax-get-property worksheet 'Range "A1" 'Value)
                        )
       )
       (princ "125\n")

       ;; Step 6: Print result
       (if (null cell-value) 
         (princ "\nCell A1 is empty.")
         (princ (strcat "\nValue from A1: " (vl-princ-to-string cell-value)))
       )
     )
  )

  ;; Step 7: Error handling and cleanup
  (vl-catch-all-apply 
    '(lambda () 
       ;; You can choose to close Excel or leave it open
       ;; (vlax-invoke-method workbook 'Close)
       ;; (vlax-release-object workbook)
       ;; (vlax-invoke-method excel-app 'Quit)
       ;; (vlax-release-object excel-app)

       ;; Optional: Release objects (recommended)
       (vlax-release-object worksheet)
       (vlax-release-object workbook)
       (vlax-release-object excel-app)
     )
  )

  (princ)
)

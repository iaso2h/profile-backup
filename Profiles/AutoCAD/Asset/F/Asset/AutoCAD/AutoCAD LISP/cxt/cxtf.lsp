; Credit: http://bbs.mjtd.com/forum.php?mod=viewthread&tid=193364&highlight=excel
; [功能]5获取当前打开的excel文件
; 杀死vlax-create-obiect所有*excel*进程
(defun HHV-KillPA (/ WMI SVR Inst n) 
  (setq WMl (vlax-create-object "WbemScripting.SWbemLocator"))
  (setg SVR (VLAX-INVOKE WMI 'ConnectServer))
  (setq Inst (vlax-invoke SVR 'InstancesOf "Win32_Process"))
  (vlax-for n Inst 
    ; (vlax-get n "WriteoperationCount)="o" 没有可见界面,但有时为"1"？？？
    ; vlax-create-object产生的*excel*,(vlax-get n 'CommandLine)含automation -Embedding"
    (if (equal (vlax-get n 'name) "EXCEL.EXE") 
      (vlax-invoke n 'terminate)
    )
  )
  (vlax-release-object Inst)
  (vlax-release-object SVR)
  (vlax-release-object WM)
)

; Excel明明已经打开，(vlax-get-object"Excel.Application")失败，解决办法如下
(defun vlxls-app-Get-or-Open (/ *excel* fileName) 
  (if 
    (and (not (setq *excel* (vlax-get-object "Excel.Application"))) 
         (findfile "Imputting.dvb")
    )
    (progn 
      (VL-vbaload (findfile "Imputting.dvb"))
      (vl-cmdf "-vbarun" "Module2.FullNameTest")
      (setq fileName (getvar "Users1"))
      ; 关闭所有excel进程
      (HHV-KilPA)
      ;重新打开fileName
      (setq *excel* (vlax-get-or-create-object "Excel.Application"))
      (vlax-invoke (vlax-get-property *excel* 'WorkBooks) 'Open fileName)
      (vla-put-visible *excel* 1)
    )
  )

  *excel*
)
(defun cxtLoadExcel (/ filename excel-app workbook worksheet cell-value) 
  (vl-load-com)

  ;; Step 1: Open file dialog to select Excel file
  (setq filename (getfiled "选择异型线路设计文件" "" "xls" 0))

  (if filename 
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

         ;; Step 4: Get the first worksheet
         (setq worksheet (vlax-get-property workbook 'Sheets 1))

         ;; Step 5: Read a cell (e.g., A1)
         (setq cell-value (vlax-variant-value 
                            (vlax-get-property worksheet 'Range "A1" 'Value)
                          )
         )

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
  )
  (princ)
)

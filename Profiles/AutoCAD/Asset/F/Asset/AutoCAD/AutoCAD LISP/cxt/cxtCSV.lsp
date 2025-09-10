(defun readCSVFile (/ filename fileHandle lineData dataList processedData row col value) 
  (princ "\n")
  (setq filename (getfiled "选择CSV文件" "" "csv" 2))



  (if (not filename) (exit))

  ;; Open file for reading
  (setq fileHandle (open filename "r"))
  (if (not fileHandle) 
    (progn 
      (princ "\nError: 无法打开文件CSV文件。")
      (exit)
    )
  )

  (princ "正从: \"")
  (princ filename)
  (princ "\"中读取数据\n")

  ;; Initialize data storage

  ;; Read all lines from file
  (setq lineCount 0)
  (setq cvsLineReadChk nil)
  (setq dataList '())
  (setq parsedLastLine '())
  (setq isInQuotes nil)
  (while 
    (or 
      (not cvsLineReadChk)
      (not (null (setq lineData (read-line fileHandle))))
    )
    (if (and (/= lineData "") (not (null lineData))) 
      (progn 
        ;; Parse CSV line
        (setq parsedResult (ParseCSVLine lineData isInQuotes)
              isInQuotes   (cadr parsedResult)
        )

        (setq parsedLine (car parsedResult))

        ; Append current line to previous line if it's still being parsed by checking if the last line is not an empty list
        (if (/= (length parsedLastLine) 0) 
          (progn 
            ; Conconate last field with current field with a "\n" character in between
            (setq parsedLastLine (reverse parsedLastLine))
            (setq lastField (car parsedLastLine))
            (setq parsedLastLine (reverse (cdr parsedLastLine)))
            (setq lastField (strcat 
                              lastField
                              "\n"
                              (car parsedLine)
                            )
            )
            (setq parsedLastLine (append parsedLastLine (list lastField)))

            (setq parsedLine (append parsedLastLine (cdr parsedLine)))

            ; Reset last line to empty list
            (setq parsedLastLine '())
          )
        )

        (if (null isInQuotes) 
          (setq dataList (append dataList (list parsedLine)))
          (setq parsedLastLine (append parsedLastLine parsedLine)) ; The quoted field continues on the next line, we'll deal with it in the next loop
        )
      )
    )

    (if (null cvsLineReadChk) (setq cvsLineReadChk T))
    (setq lineCount (1+ lineCount))
  )

  ;; Close file
  (close fileHandle)

  ;; Process and display the data
  dataList
)

  ;; Function to parse CSV line
(defun ParseCSVLine (line isInQuotes / result currentField char i) 
  ;; Returns a list of fields and a flag indicating whether the field is quoted
  ; In most cases, the value of isInQuotes is nil, but if the the last line ended with a quoted field open, it will be T.
  (setq result '())
  (setq currentField "")

  (setq i 0)
  (while (< i (strlen line)) 
    (setq char (substr line (1+ i) 1))

    (cond 
      ((and (= char "\"") (not isInQuotes))
       ;; Start of quoted field
       (setq isInQuotes T)
      )
      ((and (= char "\"") isInQuotes)
       ;; End of quoted field or escaped quote
       (if (and (< (1+ i) (strlen line)) (= (substr line (+ 2 i) 1) "\"")) 
         ;; Escaped quote - add single quote and skip next character
         (progn 
           (setq currentField (strcat currentField "\""))
           (setq i (1+ i))
         )
         ;; End of quoted field
         (setq isInQuotes nil)
       )
      )
      ((and (= char ",") (not isInQuotes))
       ;; Field separator
       (setq result (append result (list currentField)))
       (setq currentField "")
      )
      (T
       ;; Regular character
       (setq currentField (strcat currentField char))
      )
    )

    (setq i (1+ i))
  )

  ;; Add last field
  (setq result (append result (list currentField)))

  (list result isInQuotes)
)

  ;; Simple version for basic use (A-Z columns only)
(defun getCellValue (dataList address / colChar rowNumber colIndex rowIndex) 
  ; Return nil if address is invalid

  ;; Parse address like "A2"
  (setq colChar (substr address 1 1))
  (setq rowNumber (atoi (substr address 2)))

  ;; Convert to zero-based indices
  (setq colIndex (- (ascii colChar) 65)) ; A=0, B=1, etc.
  (setq rowIndex (1- rowNumber))

  ;; Bounds checking
  (if (and (>= rowIndex 0) (< rowIndex (length dataList))) 
    (progn 
      (setq rowData (nth rowIndex dataList))
      (if (and (>= colIndex 0) (< colIndex (length rowData))) 
        (nth colIndex rowData)
        nil ; Column out of range
      )
    )
    nil ; Row out of range
  )
)

(setq *IsLoadedCXTCSV* T)
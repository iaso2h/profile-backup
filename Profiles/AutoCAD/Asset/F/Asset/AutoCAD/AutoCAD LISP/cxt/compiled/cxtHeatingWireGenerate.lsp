(defun c:cxt_frs () (c:cxtHeatingWireGenerate) (princ))
(defun c:cxtHeatingWireGenerate (/ *error* currentPoint grData grCode grVal loopChk 
                                 numLines rotationAngle rotationQuadrantOffset 
                                 lastRotationAngle p1 p2 p3 p4 p5 p6 endAxisLength i 
                                 currentOffset lineP1 lineP2 oldCmdEcho rawAngle 
                                 entlastSaved turnLineCount drawInwardChk 
                                 drawShortLineChk drawFlipChk ssAxes idxSet
                                ) 
  (vl-load-com)
  (terpri)
  (if (not *CXTHeatingWireLoaded*) 
    (progn 
      (load "cxtPara")
      (CXTInitPara)
    )
    (progn 
      (prompt (strcat "已加载CSV参数文件: " *CXTHeatingWireCSVFile*))
      (terpri)
      (initget "Jimbo File")
      (if 
        (= 
          (getkword "已载有发热丝参数，请做选择: [继续生成\(J\)/选择新文件\(F\)]:<继续生成\(J\)>")
          "File"
        )
        (CXTInitPara)
      )
    )
  )
  (if (not *IsLoadedSetup*) (load "setup"))

  ;; --- Error Handling Function ---
  ;; This function ensures that system variables are restored if the user cancels.
  (defun *error* (msg) 
    (if oldCmdEcho (setvar "CMDECHO" oldCmdEcho))
    (if oldFilletRad (setvar "FILLETRAD" oldFilletRad))
    (if oldCLayer (setvar "CLAYER" oldCLayer))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*BREAK*,*EXIT*"))) 
      (princ (strcat "\nError: " msg))
    )
    (princ) ; Suppress error message on quiet exit
  )
  (defun id2r (degrees) 
    (+ (iaso2h:d2r degrees) 
       (* rotationQuadrantOffset (/ PI 2.0))
    )
  )
  (defun drawTurnLine (reverseChk / directionDegree lineLength) 
    (if loopChk 
      (progn 
        (if reverseChk 
          (setq directionDegree 180)
          (setq directionDegree 0)
        )

        (if (= (rem turnLineCount 2) 0) 
          (setq lineLength (* 
                             (- 
                               (* 2 
                                  (- 
                                    *CXTHeatingWireSet*
                                    idxSet
                                  )
                               )
                               1
                             )
                             *CXTHeatingWireAlongAreaLengthAxisSpacing*
                           )
          )
          (setq lineLength (* 
                             (+ 
                               (* 2 
                                  idxSet
                               )
                               1
                             )
                             *CXTHeatingWireAlongAreaLengthAxisSpacing*
                           )
          )
        )

        (entmakex 
          (list '(0 . "LINE") 
                (cons 10 p5)
                (cons 11 
                      (setq p5 (polar p5 
                                      (id2r directionDegree)
                                      lineLength
                               )
                      )
                )
          )
        )

        (setq turnLineCount (1+ turnLineCount))

        ; Check terminating loop
        (if 
          (and 
            (>= turnLineCount 
                (- 
                  (/ 
                    *CXTHeatingWireAlongAreaLengthCount*
                    *CXTHeatingWireSet*
                  )
                  1
                )
            )
            (= (rem turnLineCount 2) 1)
          )
          (progn 
            (terpri)
            (setq loopChk nil)
          )
        )
      )
    )
    (princ)
  )

  (defun drawLine (/ directionDegree multipliedFactor additionalGap lineLength) 
    (if loopChk 
      (progn 
        (if (not drawFlipChk) 
          (if drawInwardChk 
            (setq directionDegree 90)
            (setq directionDegree 270)
          )
          (if drawInwardChk 
            (setq directionDegree 270)
            (setq directionDegree 90)
          )
        )
        ; Determine draw a short line or a long line.
        (if 
          (or 
            (and (= turnLineCount 0) (not drawFlipChk) drawInwardChk) ; This make sure the very first line is always a short line.
            drawShortLineChk
          )
          (progn 
            ; Draw a short line in this function calling stack.
            (setq multipliedFactor 1)
            (setq additionalGap 0)
            ; Determine the direction of the next line to draw in next function calling stack.
            (if drawInwardChk 
              (setq drawShortLineChk T)
              (setq drawShortLineChk nil)
            )
          )
          (progn 
            ; Draw a long line in this function calling stack.
            (setq multipliedFactor 2)
            (setq additionalGap *CXTHeatingWireAlongAreaLengthAxisSpacing*)

            ; Determine the direction of the next line to draw in next function calling stack.
            (if drawInwardChk 
              (setq drawShortLineChk nil)
              (setq drawShortLineChk T)
            )
          )
        )

        ; Always set the next line to draw in the inverse direction in next function calling stack.
        (setq drawInwardChk (not drawInwardChk))

        ; Compute the line length
        (setq lineLength (+ 
                           (* multipliedFactor 
                              (/ 
                                (- *CXTHeatingAreaNetWidth* 
                                   (* 2.0 
                                      *CXTHeatingWireAlongAreaLengthAxisSpacing*
                                   )
                                )
                                3.0
                              )
                           )
                           additionalGap
                         )
        )

        ; Minus addtional gap in multiple heating wire sets mode.
        (setq lineLength (- lineLength 
                            (* (- *CXTHeatingWireSet* 1) 
                               *CXTHeatingWireAlongAreaLengthAxisSpacing*
                            )
                         )
        )


        (entmakex 
          (list '(0 . "LINE") 
                (cons 10 p5)
                (cons 11 
                      (setq p5 (polar p5 
                                      (id2r directionDegree)
                                      lineLength
                               )
                      )
                )
          )
        )
      )
    ) ; End of progn block

    (princ)
  ) ; End of drawLine function

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq releaseVersion T)
  ;; --- Setup and Initial Variables ---
  (setq oldCmdEcho (getvar "CMDECHO"))
  (setq oldFilletRad (getvar "FILLETRAD"))
  (setvar "CMDECHO" 0)
  ;; Non-graphcial element setup
  (c:setupLayer)
  (setq oldCLayer (getvar "CLAYER"))
  (setvar "CLAYER" "0")


  ;; Define the geometry constants
  (setq numLines 10)

  ;; Prompt the user for the first corner point of the rectangle.
  (setq p1 (getpoint "\n指定发热区的定位点: "))

  (if (not p1) (exit))

  (setq loopChk           T
        lastRotationAngle nil
  )

  (princ "\n移动鼠标按左键确认放置定位点，按Esc键取消。\n")
  (while loopChk 
    ;; grread arguments:
    ;; T: Allows tracking of mouse movement without requiring a point click.
    ;; 15: A bit-coded value to control grread's behavior.
    (setq grData (grread T 15))
    (setq grCode (car grData)) ; The event type (e.g., 5 for mouse move, 3 for click)
    (setq grVal (cadr grData)) ; The event data (e.g., cursor coordinates)

    (cond 
      ;; grCode 5: Mouse Movement (Update Preview)
      ((= grCode 5)
       (setq currentPoint grVal)

       ;; Calculate the raw angle from the first point to the current cursor position.
       (setq rawAngle (angle p1 currentPoint))
       ;; Snap the angle to the nearest 90-degree increment (0, 90, 180, 270) to create the "ortho-mode" rotation effect.
       (setq rotationAngle (* (/ PI 2.0) (fix (+ 0.5 (/ rawAngle (/ PI 2.0))))))
       (setq rotationQuadrantOffset (fix (/ rotationAngle (/ PI 2.0))))
       (if (> rotationQuadrantOffset 4) 
         (setq rotationQuadrantOffset (rem rotationQuadrantOffset 4))
       )

       ;; --- Redraw Preview ONLY if the angle has changed ---
       (if (/= rotationAngle lastRotationAngle) 
         (progn 
           (if lastRotationAngle (redraw))

           ;; Calculate points for the new rectangle
           (setq p2 (polar p1 rotationAngle *CXTHeatingAreaGrossLegnth*))
           (setq p4 (polar p1 
                           (+ rotationAngle (/ PI 2.0))
                           *CXTHeatingAreaGrossWidth*
                    )
           )
           (setq p3 (polar p2 
                           (+ rotationAngle (/ PI 2.0))
                           *CXTHeatingAreaGrossWidth*
                    )
           )

           ;; Draw the new main rectangle outline
           (grdraw p1 p2 7 -1)
           (grdraw p2 p3 7 -1)
           (grdraw p3 p4 7 -1)
           (grdraw p4 p1 7 -1)

           ;; Draw the new internal lines
           ;  (setq i 1)
           ;  (while (<= i numLines)
           ;    (setq currentOffset (* i boundaryOffset))
           ;    (setq lineP1 (polar (polar p1 rotationAngle currentOffset)
           ;                        (+ rotationAngle (/ PI 2.0))
           ;                        currentOffset
           ;                 )
           ;    )
           ;    (setq lineP2 (polar (polar p1 rotationAngle currentOffset)
           ;                        (+ rotationAngle (/ PI 2.0))
           ;                        (- rectHeight currentOffset)
           ;                 )
           ;    )
           ;    (grdraw lineP1 lineP2 1 -1)
           ;    (setq i (1+ i))
           ;  )

           (setq lastRotationAngle rotationAngle)
         ) ; end progn for redraw
       ) ; end if angle changed
      ) ; End of mouse movement case

      ;; grCode 3: Left Mouse Click (Finalize and Draw)
      ((= grCode 3)
       ;; The user has clicked to confirm the placement.
       ;; We use the last calculated rotationAngle from the preview.

       ;; Draw the heating wire area
       (entmakex 
         (append 
           (list '(0 . "LWPOLYLINE") 
                 '(100 . "AcDbEntity")
                 '(100 . "AcDbPolyline")
                 '(90 . 4)
                 '(70 . 1) ; Closed polyline
           )
           (mapcar '(lambda (pt) (cons 10 pt)) (list p1 p2 p3 p4))
         )
       )
       (command "_.fillet" "_r" CXTHeatingAreaFilletRaidus "")
       (command "_.fillet" "_p" "_l")

       ;; Draw the 10 permanent internal lines using the LINE command.
       ;  (setq i 1)
       ;  (while (<= i numLines)
       ;    (setq currentOffset (* i boundaryOffset))
       ;    (setq lineP1 (polar (polar p1 rotationAngle currentOffset)
       ;                        (+ rotationAngle (/ PI 2.0))
       ;                        currentOffset
       ;                 )
       ;    )
       ;    (setq lineP2 (polar (polar p1 rotationAngle currentOffset)
       ;                        (+ rotationAngle (/ PI 2.0))
       ;                        (- rectHeight currentOffset)
       ;                 )
       ;    )
       ;    (command "_.line" lineP1 lineP2 "")
       ;    (setq i (1+ i))
       ;  )
       (setq idxSet 0)
       (while (< idxSet *CXTHeatingWireSet*) 
         ;; Parameter Initialization
         (setq loopChk T)
         (setq CXTHeatingAreaFilletRaidus 2) ; Constant value
         (setq turnLineCount 0) ; Reset the turn line count. Used to to track wether the heating wire axes meet the threshold, namingly `*CXTHeatingWireAlongAreaLengthAxisSpacing*` or `*CXTHeatingWireAlongAreaWidthAxisSpacing*`. Termination check is done within every function calling of `drawTurnLine()`
         (setq drawInwardChk T) ; To track and determine the the next short line or long line is drawing inward or outward.
         (setq drawShortLineChk T) ; To track and determine the the next line is a short line or a long line.
         (setq drawFlipChk nil) ; To track and determine wether the following lines is drawn in the filp direction or not.
         (setq entlastSaved (entlast))


         (setq p5 (polar 
                    (polar p1 
                           (id2r 0)
                           (+ 
                             *CXTHeatingBoundaryOffset*
                             (* 
                               idxSet
                               *CXTHeatingWireAlongAreaLengthAxisSpacing*
                             )
                           )
                    )
                    (id2r 90)
                    (+ 
                      *CXTHeatingBoundaryOffset*
                      (* 
                        idxSet
                        *CXTHeatingWireAlongAreaLengthAxisSpacing*
                      )
                    )
                  )
         )
         (setq endAxisLength (- *CXTHeatingAreaNetWidth* 
                                (* 
                                  (* idxSet 2)
                                  *CXTHeatingWireAlongAreaLengthAxisSpacing*
                                )
                             )
         )
         (setq p6 (polar 
                    p5
                    (id2r 90)
                    endAxisLength
                  )
         )
         (setvar "CLAYER" "参照")
         (entmake (list '(0 . "LINE") (cons 10 p5) (cons 11 p6)))
         ; Draw forward
         (while loopChk 
           (drawTurnLine drawFlipChk)
           (drawLine)
           (drawTurnLine drawFlipChk)
           (drawLine)
           (drawTurnLine drawFlipChk)
           (drawLine)
           (drawTurnLine drawFlipChk)
           (drawLine)
         )
         ; Draw backward
         (setq p6 (polar p5 (id2r 90) endAxisLength))
         (entmake (list '(0 . "LINE") (cons 10 p5) (cons 11 p6)))
         (setq p5 (list (car p6) (cadr p6)))
         (setq loopChk T) ; Reset the loop flag to continue the drawing in flip direction.
         (setq drawFlipChk T) ; The following lines will be drawn in the flip direction.
         (setq turnLineCount 0) ; Reset the turn line count. Used to to track wether the heating wire axes meet the threshold, namingly `*CXTHeatingWireAlongAreaLengthAxisSpacing*` or `*CXTHeatingWireAlongAreaWidthAxisSpacing*`. Termination check is done within every function calling of `drawTurnLine()`
         (while loopChk 
           (drawTurnLine drawFlipChk)
           (drawLine)
           (drawTurnLine drawFlipChk)
           (drawLine)
           (drawTurnLine drawFlipChk)
           (drawLine)
           (drawTurnLine drawFlipChk)
           (drawLine)
         )

         ; Join Heating Wires
         (setq ssAxes (iaso2h:entlastTillNow entlastSaved))
         (setq entlastSaved (entlast))
         (if (wcmatch (getvar "PRODUCT") "AutoCAD*") 
           ; As for AutoCAD, check if "LWPolyline" entities exist in new selection set to determine whether there is an extra step when executing the `pedit` command
           (command "._pedit" "m" ssAxes "" "Y" "J" "") ; There's an extra for AutoCAD to prompt user whether to convert entities to polylines.
           (command "._pedit" "m" ssAxes "" "J" "") ; For ZWCAD
         )
         (command)
         (command "._fillet" 
                  "R"
                  (- 
                    (iaso2h:decimalTruncate 
                      (/ *CXTHeatingWireAlongAreaLengthAxisSpacing* 2.0)
                      1
                    )
                    0.1
                  )
                  ""
         )
         (command "_.fillet" "_p" "_l")

         ; Before entering into next loop
         (setq idxSet (1+ idxSet))
       )


       (setq loopChk nil)
       (redraw)
      ) ; End of left mouse click case

      ;; grCode 2 or 11: Keyboard Input (Cancel)
      ((or (= grCode 2) (= grCode 11))
       (princ "\n*Cancel*")
       (setq loopChk nil) ; Exit the loop on key press (e.g., Escape).
      ) ; End of Cancle input case
    ) ; End cond
  ) ; End while

  ;; --- Cleanup ---
  (setvar "CMDECHO" oldCmdEcho) ; Restore original command echo setting.
  (setvar "FILLETRAD" oldFilletRad)
  (setvar "CLAYER" oldCLayer)
  (princ) ; Suppress the echo of the last evaluation in the command line.
)

;;; --- Load Message ---
(princ)
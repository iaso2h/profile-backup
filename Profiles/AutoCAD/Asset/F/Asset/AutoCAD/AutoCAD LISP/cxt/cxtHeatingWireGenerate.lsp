(defun c:cxt_frs () (cxtHeatingWireInit) (princ))
(defun cxtHeatingWireInit (/ ans paraInitChk oldDynamicInput loopChk generationResult) 
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
      (princ)
    )
    (if oldDynamicInput 
      (setvar "DYNMODE" oldDynamicInput)
    )

    (princ)
  )
  (defun initDefualtSettings () 
    (setq *CXTHeatingAreaFilletRaidus* -1)
    (setq *CXTHeatingWireAxisSpacing* *CXTHeatingWireAlongAreaLengthAxisSpacing*)
    (setq *CXTHeatingWireCount* *CXTHeatingWireAlongAreaLengthCount*)
    (setq *CXTHeatingWireFullSegmentLength* *CXTHeatingAreaNetWidth*)
    (setq *CXTHeatingWireAlongAreaDirectionFix* 0)
    (setq *CXTHeatingWireWitdhGenerateChk* T)
    (setq *CXTHeatingRoundAboutSegment* 3)

    (setq *IsLoadedCXTHeatingWire* T)
  )


  (vl-load-com)
  (terpri)
  (if (not *IsLoadedCXTHeatingWire*) 
    (progn 
      (load "cxtPara")
      (setq paraInitChk (CXTInitPara))
    )
  )
  ; Only proceed when parameter is read successfully.
  (if 
    (and 
      (not paraInitChk)
      (not *IsLoadedCXTHeatingWire*)
    )
    (exit)
  )


  ; Initialize defualt settings
  (if (not *IsLoadedCXTHeatingWire*) 
    (initDefualtSettings)
  )
  (setq loopChk T)
  (setq oldDynamicInput (getvar "DYNMODE"))
  (setvar "DYNMODE" 1)
  (while loopChk 
    (prompt (strcat "已加载CSV参数文件: " *CXTHeatingWireCSVFile* "\n"))
    (initget "eXit Reset Generate File filLet Paibu wireWidth roundAbout")
    (setq ans (getkword 
                "诚兴泰发热丝生成: [开始生成\(G\)/读取CSV参数文件\(F\)/切换外形框倒圆\(L\)/切换排布方向\(P\)/切换线宽生成\(W\)/设置回路分段数\(A\)/恢复默认布线偏好设置\(R\)/退出\(X\)]:<开始生成\(G\)>\n"
              )
    )
    (cond 
      ((= ans "eXit")
       (exit)
      )
      ((= ans "Reset")
       (initDefualtSettings)
      )
      ((= ans "File")
       (setq paraInitChk (CXTInitPara))
       (if (not paraInitChk) 
         (princ "CSV参数文件加载失败或被取消。\n")
         (progn 
           (princ (strcat "已加载CSV参数文件: " *CXTHeatingWireCSVFile* "\n"))
           (initDefualtSettings)
         )
       )
      ) ; End of heating wire parameter initialization case
      ((or (null ans) (= ans "Generate"))
       (setq loopChk nil)
       (setvar "DYNMODE" oldDynamicInput)
       (cxtHeatingWireGenerate)
      ) ; End of Generate case(Default)
      ((= ans "filLet")
       (initget "Yes No")
       (setq ans (getkword 
                   "设置外形框是否倒圆: [倒圆\(Y\)/不倒圆\(N\)]:<不倒圆\(N\)>\n"
                 )
       )
       (if (= ans "Yes") 
         (setq *CXTHeatingAreaFilletRaidus* 2)
         (setq *CXTHeatingAreaFilletRaidus* -1)
       )
      ) ; End of filLet case
      ((= ans "Paibu")
       (initget "Long Short")
       (setq ans (getkword 
                   "设置发热丝排布方向: [沿长边排布\(L\)/沿短边排布\(S\)]:<沿长边排布\(L\)>\n"
                 )
       )
       (if (= ans "Short") 
         (progn 
           (setq *CXTHeatingWireAxisSpacing* *CXTHeatingWireAlongAreaWidthAxisSpacing*)
           (setq *CXTHeatingWireCount* *CXTHeatingWireAlongAreaWidthCount*)
           (setq *CXTHeatingWireFullSegmentLength* *CXTHeatingAreaNetLength*)
           (setq *CXTHeatingWireAlongAreaDirectionFix* 90)
         )
         (progn 
           (setq *CXTHeatingWireAxisSpacing* *CXTHeatingWireAlongAreaLengthAxisSpacing*)
           (setq *CXTHeatingWireCount* *CXTHeatingWireAlongAreaLengthCount*)
           (setq *CXTHeatingWireFullSegmentLength* *CXTHeatingAreaNetWidth*)
           (setq *CXTHeatingWireAlongAreaDirectionFix* 0)
         )
       )
      ) ; End of Paibu case
      ((= ans "jiOu")
       (initget "Ji Ou")
       (setq ans (getkword 
                   "设置外形数量奇偶性: [奇数\(J\)/偶数\(O\)]:<偶数\(O\)>\n"
                 )
       )
       (if (= ans "Ji") 
         (if 
           (= *CXTHeatingWireAxisSpacing* 
              *CXTHeatingWireAlongAreaLengthAxisSpacing*
           )
           (progn 
             (setq *CXTHeatingWireAlongAreaLengthCount* (iaso2h:biggerOdd 
                                                          (/ 
                                                            *CXTHeatingWireLength*
                                                            *CXTHeatingAreaNetWidth*
                                                          )
                                                        )
             ) ;沿长边布线发热丝数(奇数)
             (princ "当前发热丝数量为(沿长边): ")
             (pp *CXTHeatingWireAlongAreaLengthCount*)
           )
           (progn 
             (setq *CXTHeatingWireAlongAreaWidthCount* (iaso2h:biggerOdd 
                                                         (/ 
                                                           *CXTHeatingWireLength*
                                                           *CXTHeatingAreaNetLength*
                                                         )
                                                       )
             ) ;沿短边布线发热丝数(奇数)
             (princ "当前发热丝数量为(沿短边): ")
             (pp *CXTHeatingWireAlongAreaWidthCount*)
           )
         )
         (if 
           (= *CXTHeatingWireAxisSpacing* 
              *CXTHeatingWireAlongAreaLengthAxisSpacing*
           )
           (progn 
             (setq *CXTHeatingWireAlongAreaLengthCount* (iaso2h:biggerEven 
                                                          (/ 
                                                            *CXTHeatingWireLength*
                                                            *CXTHeatingAreaNetWidth*
                                                          )
                                                        )
             ) ;沿长边布线发热丝数(偶数)
             (princ "当前发热丝数量为(沿长边): ")
             (pp *CXTHeatingWireAlongAreaLengthCount*)
           )
           (progn 
             (setq *CXTHeatingWireAlongAreaWidthCount* (iaso2h:biggerEven 
                                                         (/ 
                                                           *CXTHeatingWireLength*
                                                           *CXTHeatingAreaNetLength*
                                                         )
                                                       )
             ) ;沿短边布线发热丝数(偶数)
             (princ "当前发热丝数量为(沿短边): ")
             (pp *CXTHeatingWireAlongAreaWidthCount*)
           )
         )
       )
      ) ; End of jiOu case
      ((= ans "wireWidth")
       (initget "Yes No")
       (setq ans (getkword 
                   "设置是否生成发热丝线宽: [生成\(Y\)/不生成\(N\)]:<生成\(Y\)>\n"
                 )
       )
       (if (= ans "No") 
         (setq *CXTHeatingWireWitdhGenerateChk* nil)
         (setq *CXTHeatingWireWitdhGenerateChk* T)
       )
      ) ; End of wrie width case
      ((= ans "roundAbout")
       (initget 6)
       (setq ans (getint "请输入回路分段数: "))
       (if (<= ans 1) 
         (alert "回路分段数必须大于等于2。")
         (setq *CXTHeatingRoundAboutSegment* ans)
       )
      )
    )
  ) ; End of while loop


  (princ)
)
(defun cxtHeatingWireGenerate (/ *error* currentPoint grData grCode grVal loopChk 
                               numLines rotationAngle rotationQuadrantOffset 
                               lastRotationAngle p1 p2 p3 p4 p5 p6 fullSegmentLength i 
                               currentOffset lineP1 lineP2 oldCmdEcho oldCEColor 
                               rawAngle entlastSaved turnLineCount drawInwardChk 
                               drawFlipChk ssAxes idxSet startTime endTime axisObjs 
                               multipliedFactor lineSetLast
                              ) 
  (terpri)

  ;; --- Error Handling Function ---
  ;; This function ensures that system variables are restored if the user cancels.
  (defun *error* (msg) 
    (if oldCmdEcho (setvar "CMDECHO" oldCmdEcho))
    (if oldFilletRad (setvar "FILLETRAD" oldFilletRad))
    (if oldCLayer (setvar "CLAYER" oldCLayer))
    (if oldCEColor (setvar "CECOLOR" oldCEColor))
    (if 
      (and msg (not (wcmatch (strcase msg) "*CANCEL*,*BREAK*,*EXIT*" , "*函数已取消*")))
      (princ (strcat "\nError: " msg))
    )
    (redraw)
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
                             *CXTHeatingWireAxisSpacing*
                           )
          )
          (setq lineLength (* 
                             (+ 
                               (* 2 
                                  idxSet
                               )
                               1
                             )
                             *CXTHeatingWireAxisSpacing*
                           )
          )
        )

        (entmakex 
          (list '(0 . "LINE") 
                (cons 10 p5)
                (cons 11 
                      (setq p5 (polar p5 

                                      ; *CXTHeatingWireAlongAreaDirectionFix* is either 0 or 90 to control the direction of the along-area direction.
                                      (id2r 
                                        (- directionDegree 
                                           *CXTHeatingWireAlongAreaDirectionFix*
                                        )
                                      )
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
                    *CXTHeatingWireCount*
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

  (defun drawLine (/ directionDegree lineSetOffset roundAboutContainCount lineLength) 
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
        (if (/= (rem turnLineCount 2) 0) 
          (setq lineSetOffset (+ (/ (+ turnLineCount 1) 2) 
                                 lineSetLast
                              )
          )
          (setq lineSetOffset (+ (/ turnLineCount 2) 
                                 lineSetLast
                              )
          )
        )

        (setq multipliedFactor (rem lineSetOffset 
                                    (- *CXTHeatingRoundAboutSegment* 1)
                               )
        )
        (if (= multipliedFactor 0) 
          (setq multipliedFactor (- *CXTHeatingRoundAboutSegment* 1))
        )

        (cond 
          ((= 
             (rem 
               lineSetOffset
               (- *CXTHeatingRoundAboutSegment* 1)
             )
             1
           )
           (setq roundAboutContainCount 0)
          )
          ((= 
             (rem 
               lineSetOffset
               (- *CXTHeatingRoundAboutSegment* 1)
             )
             0
           )
           (setq roundAboutContainCount (- *CXTHeatingRoundAboutSegment* 2))
          )
          (T
           (setq roundAboutContainCount (- 
                                          (rem lineSetOffset 
                                               (- *CXTHeatingRoundAboutSegment* 
                                                  1
                                               )
                                          )
                                          1
                                        )
           )
          )
        )

        ; Always set the next line to draw in the inverse direction in next function calling stack.
        (setq drawInwardChk (not drawInwardChk))

        ; Compute the line length
        (setq lineLength (+ 
                           (* multipliedFactor 
                              (/ 
                                (- *CXTHeatingWireFullSegmentLength* 
                                   (* (- *CXTHeatingRoundAboutSegment* 1) 
                                      *CXTHeatingWireAxisSpacing*
                                   )
                                )
                                (float *CXTHeatingRoundAboutSegment*)
                              )
                           )
                           (* roundAboutContainCount *CXTHeatingWireAxisSpacing*)
                         )
        )

        ; Minus addtional gap in multiple heating wire sets mode.
        (setq lineLength (- lineLength 
                            (* (- *CXTHeatingWireSet* 1) 
                               *CXTHeatingWireAxisSpacing*
                            )
                         )
        )
        (entmakex 
          (list '(0 . "LINE") 
                (cons 10 p5)
                (cons 11 
                      (setq p5 (polar p5 
                                      ; *CXTHeatingWireAlongAreaDirectionFix* is either 0 or 90 to control the direction of the along-area direction.
                                      (id2r 
                                        (- directionDegree 
                                           *CXTHeatingWireAlongAreaDirectionFix*
                                        )
                                      )
                                      lineLength
                               )
                      )
                )
          )
        )
      ) ; End of progn block
    )
  ) ; End of drawLine function
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  ;; --- Setup and Initial Variables ---
  (setq oldCmdEcho (getvar "CMDECHO"))
  (setq oldFilletRad (getvar "FILLETRAD"))
  (setq oldCEColor (getvar "CECOLOR"))
  (setq oldCLayer (getvar "CLAYER"))
  (setvar "CMDECHO" 0)
  (setvar "CECOLOR" "BYLAYER")
  ;; Non-graphcial element setup
  (c:setupLayer)


  ;; Define the geometry constants
  ; (setq numLines 10)
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

       (command "undo" "be")
       (setq startTime (getvar "DATE"))


       ;; Draw the heating wire area
       (setvar "CLAYER" "0")
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
       ; Whether to fillet the heating wire area or not.
       (if (> *CXTHeatingAreaFilletRaidus* 0) 
         (progn 
           (command "_.fillet" "_r" *CXTHeatingAreaFilletRaidus* "")
           (command "_.fillet" "_p" "_l")
         )
       )

       ;; We use the last calculated rotationAngle from the preview.

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

       ; Draw the heating wire axes
       (setq idxSet 0)
       (setq axisObjs '())
       (while (< idxSet *CXTHeatingWireSet*) 
         ;; Parameter Initialization
         (setvar "CLAYER" "参照")
         (setq loopChk T)
         (setq turnLineCount 0) ; Reset the turn line count. Used to to track wether the heating wire axes arrive at the boundary of the heating area, namingly. Termination check is done within every function calling of `drawTurnLine()`
         (setq lineSetLast 0)
         (setq drawInwardChk T) ; To track and determine the the next short line or long line is drawing inward or outward.
         (setq drawFlipChk nil) ; To track and determine wether the following lines is drawn in the filp direction or not.
         (setq entlastSaved (entlast))
         (setq fullSegmentLength (- *CXTHeatingWireFullSegmentLength* 
                                    (* 
                                      (* idxSet 2)
                                      *CXTHeatingWireAxisSpacing*
                                    )
                                 )
         )

         (if 
           (/= *CXTHeatingWireAxisSpacing* 
               *CXTHeatingWireAlongAreaLengthAxisSpacing*
           )

           (setq p5 (polar 
                      (polar p4 
                             (id2r 0)
                             (+ 
                               *CXTHeatingBoundaryOffset*
                               (* 
                                 idxSet
                                 *CXTHeatingWireAxisSpacing*
                               )
                             )
                      )
                      (id2r 270)
                      (+ 
                        *CXTHeatingBoundaryOffset*
                        (* 
                          idxSet
                          *CXTHeatingWireAxisSpacing*
                        )
                      )
                    )
           )
           (setq p5 (polar 
                      (polar p1 
                             (id2r 0)
                             (+ 
                               *CXTHeatingBoundaryOffset*
                               (* 
                                 idxSet
                                 *CXTHeatingWireAxisSpacing*
                               )
                             )
                      )
                      (id2r 90)
                      (+ 
                        *CXTHeatingBoundaryOffset*
                        (* 
                          idxSet
                          *CXTHeatingWireAxisSpacing*
                        )
                      )
                    )
           )
         )

         (setq p6 (polar 
                    p5
                    (id2r (- 90 *CXTHeatingWireAlongAreaDirectionFix*))
                    fullSegmentLength
                  )
         )
         ; Draw the first heating wire axis without break roundabout route in the middle.
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


         ; Drawing backward
         (setq p6 (polar p5 
                         (id2r (- 90 *CXTHeatingWireAlongAreaDirectionFix*))
                         fullSegmentLength
                  )
         )
         (entmake (list '(0 . "LINE") (cons 10 p5) (cons 11 p6)))
         (setq lineSetLast (- 
                             (- *CXTHeatingRoundAboutSegment* multipliedFactor)
                             1
                           )
         )
         (setq p5 p6)
         (setq loopChk T) ; Reset the loop flag to continue the drawing in flip direction.
         (setq drawFlipChk T) ; The following lines will be drawn in the flip direction.
         (setq turnLineCount 0) ; Reset the turn line count. Used to to track wether the heating wire axes arrive at the boundary of the heating area, namingly. Termination check is done within every function calling of `drawTurnLine()`
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
         (setq ssAxes (iaso2h:entlastTillNow entlastSaved T))

         (if (= (getvar "PEDITACCEPT") 0) 
           (progn 
             (command "._pedit" "_M" ssAxes "" "_Y" "_J" "_J" "_E")
             (command "")
           )
           (progn 
             (command "._pedit" "_M" ssAxes "" "_J" "_J" "_E")
             (command "")
           )
         )
         (setq axisObjs (cons 
                          (vlax-ename->vla-object (entlast))
                          axisObjs
                        )
         )
         ; Alternative Join Method
         ;  (command "._join" ssAxes "")

         ; Since ZWCAD(AutoCAD?) will always set cmdecho to 1 after invoking the `pedit` command, we need to set it back to 0 again.
         (setvar "CMDECHO" 0)

         (command) ; Emulate the escape key

         (command "._fillet" 
                  "R"
                  (- 
                    (iaso2h:decimalTruncate 
                      (/ *CXTHeatingWireAxisSpacing* 2.0)
                      1
                    )
                    0.1
                  )
         )
         (command "_.fillet" "_p" "_l")


         ; Before entering into next loop
         (setq idxSet (1+ idxSet))
       ) ; End of drawing heating multiple wire axes.

       ; Draw the outline of heating wires
       (if *CXTHeatingWireWitdhGenerateChk* 
         (progn 
           (setq *CXTHeatingWireRealWidth* (/ 
                                             (/ 
                                               (/ 
                                                 (* 
                                                   (/ 
                                                     (/ 
                                                       (apply '+ 
                                                              (mapcar 'vla-get-length 
                                                                      axisObjs
                                                              )
                                                       )
                                                       (float *CXTHeatingWireSet*)
                                                     )
                                                     1000.0
                                                   )
                                                   *CXTHeatingWireResistivity*
                                                 )
                                                 *CXTHeatingWireResistance*
                                               )
                                               *CXTHeatingWireThickness*
                                             )
                                             *CXTHeatingWireSet*
                                           )
           )
           (setvar "CLAYER" "发热丝")

           (foreach obj axisObjs 
             (cxtDoubleOffset obj *CXTHeatingWireRealWidth*)
           )
         )
         (setq entlastSaved (entlast))
       ) ; End of heating wire width generation
       (foreach ent (setq ssAxes (iaso2h:entlastTillNow entlastSaved nil)) 
         (if 
           (/= *CXTHeatingWireAxisSpacing* 
               *CXTHeatingWireAlongAreaLengthAxisSpacing*
           )
           (progn 

             (setq *CXTHeatingWireAlongAreaWidthOutlineRealSpacing* (- *CXTHeatingWireAxisSpacing* 
                                                                       *CXTHeatingWireRealWidth*
                                                                    )
             )
             (command "._fillet" 
                      "R"
                      (- 
                        (iaso2h:decimalTruncate 
                          (/ 
                            *CXTHeatingWireAlongAreaWidthOutlineRealSpacing*
                            2.0
                          )
                          1
                        )
                        0.1
                      )
             )
           )
           (progn 
             (setq *CXTHeatingWireAlongAreaLengthOutlineRealSpacing* (- *CXTHeatingWireAxisSpacing* 
                                                                        *CXTHeatingWireRealWidth*
                                                                     )
             )
             (command "._fillet" 
                      "R"
                      (- 
                        (iaso2h:decimalTruncate 
                          (/ 
                            *CXTHeatingWireAlongAreaLengthOutlineRealSpacing*
                            2.0
                          )
                          1
                        )
                        0.1
                      )
             )
           )
         )

         (command "_.fillet" "_p" ent)
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
  (command "undo" "e")
  (setvar "FILLETRAD" oldFilletRad)
  (setvar "CECOLOR" oldCEColor)
  (setvar "CLAYER" oldCLayer)
  (setvar "CMDECHO" oldCmdEcho)
  (setq endTime (getvar "DATE"))
  (terpri)
  (princ 
    (strcat 
      "发热丝生成成功，用时"
      (rtos (* 86400 (- endTime startTime)) 2 4)
      "秒。\n"
    )
  )

  T ; Suppress the echo of the last evaluation in the command line.
)

  ;;; --- Load Message ---
(terpri)
(princ "诚兴泰工具箱 V0.0.6已加载，更新时间: 2025-09-15\n")
(load "util")
(load "setup")
(load "cxtDoubleOffset")
(load "cxtToggleHidden")
(load "cxtHeatingBoardSection")
(princ)
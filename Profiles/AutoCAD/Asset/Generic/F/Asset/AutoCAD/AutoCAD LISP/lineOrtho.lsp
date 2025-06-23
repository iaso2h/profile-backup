(defun c:lineOrtho (/ ssetLines ssetHLines ssetVLines ssetLinesCnt hLines vLines 
                    entLine entData hPts vPts vlaLine i entType startPt endPt pLinePts 
                    pLinePtsCnt pLinePtPrev pLinePtCur pLineHoriChk pLineVertChk 
                    debugMode promptSelectAllLines
                   ) 
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )

  (princ "\n")
  (setq debugMode t)
  (setq promptSelectAllLines t)
  (setq ssetLines (ssget "_X" '((0 . "LINE,LWPOLYLINE"))))
  (setq hLines '()) ; Horizontal line
  (setq vLines '()) ; Vertical line
  (setq hPts '()) ; Horizontal points
  (setq vPts '()) ; Vertical points
  (setq ssetHLines (ssadd))
  (setq ssetVLines (ssadd))
  (if ssetLines 
    (progn 
      (setq ssetLinesCnt (sslength ssetLines))
      (repeat ssetLinesCnt 
        (setq entLine (ssname ssetLines (setq ssetLinesCnt (1- ssetLinesCnt))))
        (setq entData (entget entLine))
        (setq entType (cdr (assoc 0 entData)))
        (cond 
          ;; Check if the entity is a LINE
          ((= entType "LINE")
           (progn 
             (setq startPt (cdr (assoc 10 entData)))
             (setq endPt (cdr (assoc 11 entData)))
             (if (= (car startPt) (car endPt)) 
               (progn 
                 (setq vLines (cons entLine vLines))
                 (setq vPts (cons 
                              (list startPt endPt)
                              vPts
                            )
                 )
                 (ssadd entLine ssetVLines)
               ) ; Vertical
               (if (= (cadr startPt) (cadr endPt)) 
                 (progn 
                   (setq hLines (cons entLine hLines))
                   (setq hPts (cons 
                                (list startPt endPt)
                                hPts
                              )
                   )
                   (ssadd entLine ssetHLines)
                 )
               ) ; Horizontal
             )
           )
          )
          ((= entType "LWPOLYLINE")
           (setq vlaLine (vlax-ename->vla-object entLine))
           (setq pLinePts (vlax-safearray->list 
                            (vlax-variant-value (vla-get-coordinates vlaLine))
                          )
           )
           (setq pLinePtsCnt (length pLinePts))
           (setq i 0)
           (setq pLinePtPrev nil)
           (setq pLinePtLoopBreak nil)
           (setq pLineHoriChk nil)
           (setq pLineVertChk nil)

           (repeat pLinePtsCnt 
             (if 
               (and (< i (/ pLinePtsCnt 2)) 
                    (null pLinePtLoopBreak)
               )
               (progn 
                 (setq pLinePtCur (list (nth (* 2 i) pLinePts) 
                                        (nth (1+ (* 2 i)) pLinePts)
                                  )
                 )
                 (if pLinePtPrev 
                   (if (= (car pLinePtPrev) (car pLinePtCur))  ; Vertical
                     (setq pLineVertChk t)
                     (if (= (cadr pLinePtPrev) (cadr pLinePtCur))  ; Horizontal
                       (setq pLineHoriChk t)
                       (progn  ; Not vertical nor horizontal
                              (if pLineHoriChk 
                                ; Partially horizontal
                                (progn 
                                  (setq hPts (cons 
                                               (list 
                                                 (list (car pLinePts) 
                                                       (cadr pLinePts)
                                                 )
                                                 pLinePtPrev
                                               )
                                               hPts
                                             )
                                  )
                                  (setq pLinePtLoopBreak t)
                                )
                              )
                              (if pLineVertChk 
                                ; Partially vertical
                                (progn 
                                  (setq vPts (cons 
                                               (list 
                                                 (list (car pLinePts) 
                                                       (cadr pLinePts)
                                                 )
                                                 pLinePtPrev
                                               )
                                               vPts
                                             )
                                  )
                                  (setq pLinePtLoopBreak t)
                                )
                              )
                       )
                     )
                   )
                 )
               )
             )

             ; Entering next loop
             (if (and (= i 1) (null pLineHoriChk) (null pLineVertChk)) 
               (setq pLinePtLoopBreak t)
             )
             (if (and (= i (/ pLinePtsCnt 2)) (or pLineHoriChk pLineVertChk)) 
               (progn 
                 (if pLineHoriChk 
                   (setq vPts (cons 
                                (list 
                                  (list (car pLinePts) 
                                        (cadr pLinePts)
                                  )
                                  pLinePtCur
                                )
                                vPts
                              )
                   )
                   (setq hPts (cons 
                                (list 
                                  (list (car pLinePts) 
                                        (cadr pLinePts)
                                  )
                                  pLinePtCur
                                )
                                hPts
                              )
                   )
                 )
                 (setq pLinePtLoopBreak t)
               )
             )

             (setq pLinePtPrev pLinePtCur)
             (setq i (1+ i))
           )


           (if pLinePtLoopBreak 
             (if pLineHoriChk 
               (progn 
                 (setq hLines (cons entLine hLines))
                 (ssadd entLine ssetHLines)
               )
               (if pLineVertChk 
                 (progn 
                   (setq vLines (cons entLine vLines))
                   (ssadd entLine ssetVLines)
                 )
               )
             )
           )
          )
        )
      )
    )
  )

  (if (and debugMode promptSelectAllLines) 
    (while (or ssetHLines ssetVLines) 
      (if (null ans) 
        (setq ans "h")
      )
      (initget "h v")
      (if (setq tmp (getkword (strcat "选择类型[水平线(h)/垂直线(v)] <" ans ">: \n"))) 
        (setq ans tmp)
      )
      (cond 
        ((= ans "v")
         (if ssetVLines 
           (sssetfirst nil ssetVLines)
           (prompt "提示：没有垂直线\n")
         )
        )
        ((= ans "h")
         (if ssetHLines 
           (sssetfirst nil ssetHLines)
           (prompt "提示：没有水平线\n")
         )
        )
      )
    )
  )




  (princ)
)

(defun c:ottoPlotNameUpdate (/ cmd ssetBlock ssetSheetName ssetDebugRect ssUnsettled 
                             plotBlockEnts plotBlockVla minExt maxExt fHeight fWidth 
                             nameVla nameVla nameMinExt nameMaxExt factor titleStr 
                             debugMode debugBlockName
                            ) 
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )
  (vl-load-com)
  (princ "\n")
  (setq cmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (setq debugMode t)
  (if 
    (and debugMode 
         (not (tblsearch "layer" "xline"))
    )
    (command "-layer" "n" "xline" "p" "n" "xline" "d" "辅助线图层，不可打印！" "xline" "c" "41" 
             "xline" ""
    )
  )

  (setq plotBlockEnts (getPlotBlockEnts))
  (if plotBlockEnts 
    (progn 
      (command "undo" "be")
      (command "_.zoom" "_e")
      ; Check current ucs to world if it doesn't
      (command "_.ucs" "_w")
      ; Loop through all blocks
      (foreach plotBlockEnt plotBlockEnts 
        (command "_.draworder" plotBlockEnt "" "_front")
        (setq plotBlockVla (vlax-ename->vla-object plotBlockEnt))
        (setq plotBlockInsertPt (vlax-safearray->list 
                                  (vlax-variant-value 
                                    (vla-get-InsertionPoint plotBlockVla)
                                  )
                                )
        )
        (vla-GetBoundingBox plotBlockVla 'minExt 'maxExt)
        (setq minExt (vlax-safearray->list minExt)
              maxExt (vlax-safearray->list maxExt)
        )

        ; Compose title text position
        (if 
          (> (abs (- (car minExt) (car maxExt))) 
             (abs (- (cadr minExt) (cadr maxExt)))
          )
          (progn 
            (setq fWidth (abs (- (car minExt) (car maxExt))))
            (setq fHeight (abs (- (cadr minExt) (cadr maxExt))))
            (setq nameMinExt (list 
                               (+ (car minExt) 
                                  (* fWidth 
                                     0.692261953
                                  )
                               )
                               (+ (cadr minExt) 
                                  (* fHeight 
                                     0.122554762
                                  )
                               )
                             )
            )
            (setq nameMaxExt (list 
                               (+ (car minExt) 
                                  (* fWidth 
                                     0.793033221
                                  )
                               )
                               (+ (cadr minExt) 
                                  (* fHeight 
                                     0.147065714
                                  )
                               )
                             )
            )
          )
          (progn 
            (setq fWidth (abs (- (cadr minExt) (cadr maxExt))))
            (setq fHeight (abs (- (car minExt) (car maxExt))))
            (setq nameMinExt (list 
                               (+ (car minExt) 
                                  (* fWidth 
                                     0.436294762
                                  )
                               )
                               (+ (cadr minExt) 
                                  (* fHeight 
                                     0.087410774
                                  )
                               )
                             )
            )
            (setq nameMaxExt (list 
                               (+ (car minExt) 
                                  (* fWidth 
                                     0.294131429
                                  )
                               )
                               (+ (cadr minExt) 
                                  (* fHeight 
                                     0.104741751
                                  )
                               )
                             )
            )
          )
        )
        (command "_.zoom" "_w" "_non" nameMinExt "_non" nameMaxExt)
        (redraw)

        (setq ssetSheetName (ssget "_C" 
                                   nameMinExt
                                   nameMaxExt
                                   '((0 . "TEXT,MTEXT,ATTDEF"))
                            )
        )
        (if ssetSheetName 
          (progn 
            (setq nameVla (vlax-ename->vla-object (ssname ssetSheetName 0))) ; Always select the newest object
            (if (vlax-property-available-p nameVla 'TagString) 
              (LM:vl-setattributevalue 
                plotBlockVla
                "图纸名称"
                (setq titleStr (vlax-get-property nameVla 'TagString))
              )
              (LM:vl-setattributevalue 
                plotBlockVla
                "图纸名称"
                (setq titleStr (vlax-get-property nameVla 'TextString))
              )
            )
            (setq factor (LM:getdynpropvalue plotBlockVla "缩放因子"))
            (princ (strcat "Update " titleStr "\n"))
            (if debugMode 
              (progn 
                (if (null ssetDebugRect) 
                  (setq ssetDebugRect (ssadd))
                )
                (mapcar '(lambda (i) (ssadd i ssetDebugRect)) 
                        (debugDrawLine 
                          plotBlockInsertPt
                          nameMinExt
                          nameMaxExt
                          factor
                        )
                )
              )
            )
          )
          (progn 
            (princ 
              (strcat "没有文字对象或者定义属性存在于: (" 
                      (rtos (car nameMinExt))
                      ", "
                      (rtos (cadr nameMinExt))
                      "), ("
                      (rtos (car nameMaxExt))
                      ", "
                      (rtos (cadr nameMaxExt))
                      ")\n"
              )
            )
            (if (null ssUnsettled) 
              (setq ssUnsettled (ssadd))
              (ssadd plotBlockEnt ssUnsettled)
            )
          )
        )
      )

      (if 
        (and debugMode 
             ssetDebugRect
        )
        (progn (setq debugBlockName (rtos (getvar "CDATE") 2 6)) 
               (command "_.-Block" debugBlockName '(0 0) ssetDebugRect "")
               (command "_.-insert" debugBlockName '(0 0) "" "" "")
        )
      )
      (command "_.zoom" "_e")
      (command "undo" "e")
      (setvar 'cmdecho cmd)
      (if ssUnsettled 
        (sssetfirst nil ssUnsettled)
      )
    )

    (alert "\n没有找到欧拓打印图块")
  )

  (princ)
)

(defun c:ottoPlotRatioUpdate (/ cmd ssetKeyword ssetCoincident ssetUnsettled 
                              ssetSettled ssetDebugRect plotBlockEnts targetLines 
                              plotBlockEnts plotBlockVla plotBlockInsert i 
                              distanceThreshold factor plotWidth plotHeight 
                              keywordMinDeltaX keywordMinDeltaY keywordMinExt 
                              keywordMaxExt keywordVla debugMode debugAns 
                              debugBlockName
                             ) 
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )

  (princ "\n")
  (setq cmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (setq debugMode t)
  (setq distanceThreshold 0.05)
  (if 
    (and debugMode 
         (not (tblsearch "layer" "xline"))
    )
    (command "-layer" "n" "xline" "p" "n" "xline" "d" "辅助线图层，不可打印！" "xline" "c" "41" 
             "xline" ""
    )
  )

  (setq plotBlockEnts (getPlotBlockEnts))
  (if plotBlockEnts 
    (progn 
      (command "undo" "be")
      (command "_.zoom" "_e")
      (command "_.ucs" "_w")

      (foreach plotBlockEnt plotBlockEnts 
        (command "_.draworder" plotBlockEnt "" "_front")
        (setq plotBlockVla (vlax-ename->vla-object plotBlockEnt))

        (if 
          (member 
            (vla-get-rotation plotBlockVla)
            '(0 180)
          )
          (progn  ; Landscape
                 (setq targetLines (cadr (getVHLines)))
                 (setq plotWidth 297.0)
                 (setq plotHeight 210.0)
                 (setq keywordMinDeltaX 0.764297999638)
                 (setq keywordMinDeltaY 0.073532976190)
          )
          (progn  ; Portrait
                 (setq targetLines (car (getVHLines)))
                 (setq plotWidth 210.0)
                 (setq plotHeight 297.0)
                 (setq keywordMinDeltaX 0.666650119048)
                 (setq keywordMinDeltaY 0.051993009091)
          )
        )

        (setq foundKeywordChk nil)
        (foreach targetLine targetLines 
          (if (null foundKeywordChk)  ; if keyword is found in previous loop, no need to continue
            (progn 
              (setq factor (/ 
                             (distance (car (cdr targetLine)) 
                                       (cadr (cdr targetLine))
                             )
                             plotWidth
                           )
              )
              (setq plotBlockInsertPt (vlax-safearray->list 
                                        (vlax-variant-value 
                                          (vla-get-InsertionPoint plotBlockVla)
                                        )
                                      )
              )


              (if 
                (or 
                  (member 
                    plotBlockInsertPt
                    (cdr targetLine)
                  )
                  (< 
                    (distance plotBlockInsertPt 
                              (car (cdr targetLine))
                    )
                    distanceThreshold
                  )
                  (< 
                    (distance plotBlockInsertPt 
                              (cadr (cdr targetLine))
                    )
                    distanceThreshold
                  )
                )
                (progn 
                  ; (print
                  ;   (strcat
                  ;     "'"
                  ;     (cdr
                  ;       (assoc 5 (entget (car targetLine)))
                  ;     )
                  ;     "' is a coincident line"
                  ;   )
                  ; )
                  (if debugMode 
                    (progn 
                      (if (null ssetCoincident) 
                        (setq ssetCoincident (ssadd))
                      )
                      (ssadd (car targetLine) ssetCoincident)
                    )
                  )

                  ; Compute "比例" coordinate
                  (setq keywordMinExt (list 
                                        (+ (car plotBlockInsertPt) 
                                           (* 
                                             (* plotWidth 
                                                keywordMinDeltaX
                                             )
                                             factor
                                           )
                                        )
                                        (+ (cadr plotBlockInsertPt) 
                                           (* 
                                             (* plotHeight 
                                                keywordMinDeltaY
                                             )
                                             factor
                                           )
                                        )
                                        0
                                      )
                  )
                  (setq keywordMaxExt (list 
                                        (+ (car keywordMinExt) 
                                           (* 8.2357 
                                              factor
                                           )
                                        )
                                        (+ (cadr keywordMinExt) 
                                           (* 5.147325 
                                              factor
                                           )
                                        )
                                        0
                                      )
                  )
                  (command "_.zoom" "_w" "_non" keywordMinExt "_non" keywordMaxExt)
                  (redraw)
                  (setq ssetKeyword (ssget "_C" 
                                           keywordMinExt
                                           keywordMaxExt
                                           '((0 . "TEXT,MTEXT,ATTDEF"))
                                    )
                  )
                  (if 
                    (and ssetKeyword 
                         (setq keywordVla (vlax-ename->vla-object 
                                            (ssname ssetKeyword 0) ; Always select the newest object
                                          )
                         )
                    )
                    (progn 
                      (if (vlax-property-available-p keywordVla 'TagString) 
                        (if (= (vlax-get-property keywordVla 'TagString) "比例") 
                          (progn 
                            (LM:setdynpropvalue plotBlockVla "缩放因子" factor)
                            (setq foundKeywordChk t)
                            (if debugMode 
                              (progn 
                                (if (null ssetSettled) 
                                  (setq ssetSettled (ssadd))
                                )
                                (ssadd plotBlockEnt ssetSettled)

                                (if (null ssetDebugRect) 
                                  (setq ssetDebugRect (ssadd))
                                )
                                (mapcar '(lambda (i) (ssadd i ssetDebugRect)) 
                                        (debugDrawLine 
                                          plotBlockInsertPt
                                          keywordMinExt
                                          keywordMaxExt
                                          factor
                                        )
                                )
                              )
                            )
                          )
                        )
                        (if (= (vlax-get-property keywordVla 'TextString) "比例") 
                          (progn 
                            (LM:setdynpropvalue plotBlockVla "缩放因子" factor)
                            (setq foundKeywordChk t)
                            (if debugMode 
                              (progn 
                                (if (null ssetSettled) 
                                  (setq ssetSettled (ssadd))
                                )
                                (ssadd plotBlockEnt ssetSettled)

                                (if (null ssetDebugRect) 
                                  (setq ssetDebugRect (ssadd))
                                )
                                (mapcar '(lambda (i) (ssadd i ssetDebugRect)) 
                                        (debugDrawLine 
                                          plotBlockInsertPt
                                          keywordMinExt
                                          keywordMaxExt
                                          factor
                                        )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )

        ; Add current block ent to unsettle sset if keyword hasn't been found
        (if (null foundKeywordChk) 
          (progn 
            (if (null ssetUnsettled) 
              (setq ssetUnsettled (ssadd))
            )
            (ssadd plotBlockEnt ssetUnsettled)
          )
        )
      )

      (if 
        (and debugMode 
             ssetDebugRect
        )
        (progn 
          (setq debugBlockName (rtos (getvar "CDATE") 2 6))
          (command "_.-Block" debugBlockName '(0 0) ssetDebugRect "")
          (command "_.-insert" debugBlockName '(0 0) "" "" "")
        )
      )
      (command "_.zoom" "_e")
      (command "undo" "e")
      (setvar 'cmdecho cmd)
      (if ssetUnsettled 
        (progn 
          (if 
            (eq (length plotBlockEnts) 
                (sslength ssetUnsettled)
            )
            (alert (strcat (rtos (sslength ssetUnsettled) 2 0) "个图框更新比例失败"))
            (alert 
              (strcat (rtos (sslength ssetUnsettled) 2 0) 
                      "个图框更新比例失败\n"
                      (rtos (sslength ssetSettled) 2 0)
                      "个图框更新比例成功"
              )
            )
          )
          (if ssetUnsettled 
            (sssetfirst nil ssetUnsettled)
          )

          (if debugMode 

            (while t 
              (initget "F U C")
              (setq debugAns (getkword "\n高亮选择[更新失败图框(F)/更新成功图框(U)/共点单线(C)] <F>: "))
              (cond 
                ((= debugAns "F")
                 (if ssetUnsettled 
                   (sssetfirst nil ssetUnsettled)
                   (prompt "提示：没有未更新的图框\n")
                 )
                )
                ((= debugAns "U")
                 (if ssetSettled 
                   (sssetfirst nil ssetSettled)
                   (prompt "提示：没有已更新的图框\n")
                 )
                )
                ((= debugAns "C")
                 (if ssetCoincident 
                   (sssetfirst nil ssetCoincident)
                   (prompt "提示：没有共点的单线\n")
                 )
                )
                (t
                 nil
                )
              )
            )
          )
        )

        (alert "全部图框更新比例完毕！")
      )
    )

    (princ)
  )
)
(defun getPlotBlockEnts (/ ssetBlock plotBlockEnts i plotBlockEnt) 
  (setq plotBlockEnts '())

  (if 
    (not 
      (setq ssetBlock (ssget "_I"))
    )
    (setq ssetBlock (vl-catch-all-apply 'ssget (list "_X" '((0 . "INSERT")))))
  )

  (if ssetBlock 
    (progn 
      (setq i -1)
      (repeat (sslength ssetBlock) 
        (setq i (+ 1 i))
        (setq plotBlockEnt (ssname ssetBlock i))
        (if 
          (eq 
            (vla-get-effectivename (vlax-ename->vla-object plotBlockEnt))
            "otto_plot_recognition_frame"
          )
          (setq plotBlockEnts (append plotBlockEnts (list plotBlockEnt)))
        )
      )
    )
  )

  plotBlockEnts
)
(defun getVHLines (/ ssetLines ssetHLines ssetVLines ssetLinesCnt hLines vLines 
                   lineEnt entData hLines vLines entType startPt endPt 
                   promptSelectAllLines
                  ) 
  (setq ssetLines (ssget "_X" '((0 . "LINE"))))
  (setq hLines '()) ; Horizontal line
  (setq vLines '()) ; Vertical line
  (setq ssetHLines (ssadd))
  (setq ssetVLines (ssadd))
  (setq promptSelectAllLines nil)
  (if ssetLines 
    (progn 
      ; Get all vertial lines and horizontal lines
      (setq ssetLinesCnt (sslength ssetLines))
      (repeat ssetLinesCnt 
        (setq lineEnt (ssname ssetLines (setq ssetLinesCnt (1- ssetLinesCnt))))
        (setq entData (entget lineEnt))
        (setq entType (cdr (assoc 0 entData)))
        (setq startPt (cdr (assoc 10 entData)))
        (setq endPt (cdr (assoc 11 entData)))
        (if 
          (or 
            (= (car startPt) (car endPt)) ; Vertical
            (< 
              (abs 
                (- (car startPt) 
                   (car endPt)
                )
              )
              distanceThreshold
            )
          )
          (progn 
            (setq vLines (cons 
                           (list lineEnt startPt endPt)
                           vLines
                         )
            )
            (if (and debugMode promptSelectAllLines) 
              (ssadd lineEnt ssetVLines)
            )
          )
          (if 
            (or 
              (= (cadr startPt) (cadr endPt)) ; Horizontal
              (< 
                (abs 
                  (- (cadr startPt) 
                     (cadr endPt)
                  )
                )
                distanceThreshold
              )
            )
            (progn 
              (setq hLines (cons 
                             (list lineEnt startPt endPt)
                             hLines
                           )
              )
              (if (and debugMode promptSelectAllLines) 
                (ssadd lineEnt ssetHLines)
              )
            )
          )
        )
      )
    )
  )

  (list vLines hLines)
)
(defun debugDrawLine (plotBlcokInsertPt minExt maxExt factor / entNew ssetDebugRect 
                      vlaObj ent1 ent2
                     ) 
  (defun setXlineLayer (/ entNew vlaObj) 
    (setq entNew (entlast))
    (setq vlaObj (vlax-ename->vla-object entNew))
    (if (null factor) 
      (setq factor 2.5)
    )
    (vla-put-ConstantWidth vlaObj factor)
    (vla-put-color vlaObj 256)
    (vlax-put-property vlaObj 'Layer "xline")
    entNew
  )
  (command "_pline" "_non" plotBlcokInsertPt "_non" minExt "")
  (setq ent1 (setXlineLayer))
  (command "_rectang" "_non" minExt "_non" maxExt)
  ; (command "_revcloud" "_s" "_c" "_o" "_l" "")
  (setq ent2 (setXlineLayer))

  (list ent1 ent2)
)
(defun LM:vl-getattributevalue (blk tag) 
  (setq tag (strcase tag))
  (vl-some 
    '(lambda (att) 
       (if (= tag (strcase (vla-get-tagstring att))) (vla-get-textstring att))
     )
    (vlax-invoke blk 'getattributes)
  )
)
(defun LM:vl-setattributevalue (blk tag val) 
  (setq tag (strcase tag))
  (vl-some 
    '(lambda (att) 
       (if (= tag (strcase (vla-get-tagstring att))) 
         (progn (vla-put-textstring att val) val)
       )
     )
    (vlax-invoke blk 'getattributes)
  )
)

;; Set Dynamic Block Property Value  -  Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
  ;; blk - [vla] VLA Dynamic Block Reference object
  ;; prp - [str] Dynamic Block property name (case-insensitive)
  ;; val - [any] New value for property
  ;; Returns: [any] New value if successful, else nil
(defun LM:setdynpropvalue (blk prp val) 
  (setq prp (strcase prp))
  (vl-some 
    '(lambda (x) 
       (if (= prp (strcase (vla-get-propertyname x))) 
         (progn 
           (vla-put-value x 
                          (vlax-make-variant val 
                                             (vlax-variant-type (vla-get-value x))
                          )
           )
           (cond (val) (t))
         )
       )
     )
    (vlax-invoke blk 'getdynamicblockproperties)
  )
)

;; Get Dynamic Block Property Value  -  Lee Mac
;; Returns the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
(defun LM:getdynpropvalue (blk prp) 
  (setq prp (strcase prp))
  (vl-some 
    '(lambda (x) 
       (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value))
     )
    (vlax-invoke blk 'getdynamicblockproperties)
  )
)
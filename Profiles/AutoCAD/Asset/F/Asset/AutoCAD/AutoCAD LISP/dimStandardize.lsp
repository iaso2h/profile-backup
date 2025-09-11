(defun c:dimStandardize (/ doc ans ssFilter dimTextStyleEnt dimStyleTableEnt 
                         dimStyleEntData textHeight sizeFactor ss i ent obj 
                         dimStyleModifiedCount
                        ) 
  (vl-load-com)

  (defun *error* (msg) 
    (if 
      (not 
        (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))
      )
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )

  (initget "All Select")
  (setq ans (getkword "选择模式[全部\(A\)/选择\(S\)]:<选择\(S\)>"))
  (if (= ans "All") 
    (setq ssFilter "_X")
    (setq ssFilter "_:L")
  )
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq dimTextStyleEnt nil)
  (setq dimStyleModifiedCount 0)
  (vlax-for obj (vla-get-textstyles doc) 
    (if 
      (and 
        (null dimTextStyleEnt)
        (= (vla-get-name obj) "斜仿宋")
      )
      (progn 
        (setq dimTextStyleEnt (vlax-vla-object->ename obj))
      )
    )
  )

  (setvar "CMDECHO" 0)
  (command "undo" "be")
  ; Modify dimstyle
  ; NOTE: The dimstyle object doesn't have similar properties as the dimension objects do.
  ; https://help.autodesk.com/view/OARX/2024/ENU/?guid=GUID-3227408C-75A9-434C-BD3F-B189A2BE098A
  ; https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/to-vlax-dump-object-a-dimstyle/td-p/5926058
  ; (vlax-for obj (vla-get-dimstyles doc)
  ;   (if (/= (vla-get-name obj) "Standard")
  ;     (dimObjModify obj dimFontSetupChk)
  ;   )
  ; )

  ; So here I resort to using the entmod command to modify the entity data of dimension styles, but it seems that you can never read the entity data from the "Standard" dimstyle, "Annotative" can be read though. See: https://forums.augi.com/showthread.php?173075-Getting-text-height-from-quot-standard-quot-text-style

  (while 
    (and (= ans "All") 
         (setq dimStyleTableEnt (tblnext "dimstyle" (not dimStyleTableEnt)))
    )
    (setq dimStyleEntData (entget 
                            (tblobjname "dimstyle" 
                                        (cdr (assoc 2 dimStyleTableEnt))
                            )
                          )
    )
    (if 
      (and 
        (/= (cdr (assoc 2 dimStyleEntData)) 
            "Standard"
        )
      )
      (progn 
        ; Always set Fangsong Italic as the text style for all dimension styles
        (if dimTextStyleEnt 
          (setq dimStyleEntData (subst (cons 340 dimTextStyleEnt) 
                                       (assoc 340 dimStyleEntData)
                                       dimStyleEntData
                                )
          )
        )
        ; As for dimension style exported from SolidWorks, text height from dot list 140 will somehow be evaluated to nil. In this case, set it to 2.5 by default.
        (if (not (setq textHeight (cdr (assoc 140 dimStyleEntData)))) 
          (setq textHeight 2.5)
        )

        (setq sizeFactor (/ textHeight 2.5))
        (if sizeAdjustChk 
          (setq val (* val sizeFactor))
        )
        ; DXF Code: https://help.autodesk.com/view/OARX/2024/ENU/?guid=GUID-F2FAD36F-0CE3-4943-9DAD-A9BCD2AE81DA
        ;; Table "Lines"
        ; `DIMDLI`. Baseline Spacing
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 43 3.75 T))

        ; `DIMSD1`, `DIMSD2`. Dimension Line Suppressions
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 281 0 nil))
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 282 0 nil))

        ; `DIMSE1`, `DIMSE2`. Extension Line Suppressions
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 75 0 nil))
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 76 0 nil))

        ; `DIMEXE`.Extend Beyond Dimension Line
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 44 1.25 T))
        ; `DIMEXO`.Offset from Origin
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 42 0.625 T))

        ; No `DIMFXLON` Fix Length Extension Lines Related DXF Code

        ;; Tab "Symbols and Arrows"
        ; `DIMBLK1`, `DIMBLK2`. Arrow Head Block for Dimension Line
        ; TODO: Not Sure Whether This Will Work in Lower AutoCAD Versions or ZWCAD.
        (setq dimStyleEntData (dimStyleMod dimStyleEntData dimStyleEnt 5 "" nil))
        (setq dimStyleEntData (dimStyleMod dimStyleEntData dimStyleEnt 6 "" nil))
        (setq dimStyleEntData (dimStyleMod dimStyleEntData dimStyleEnt 7 "" nil))
        ; Need to Pass in The Arrow Block Name
        ; (setq dimStyleEntData (dimStyleMod dimStyleEntData dimStyleEnt 343 "" nil))
        ; (setq dimStyleEntData (dimStyleMod dimStyleEntData dimStyleEnt 344 "" nil))

        ; `DIMASZ`. Arrowhead Size
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 41 2.5 T))

        ; `DIMCEN`. Center Mark Size
        ; No `DIMFXLON` Related DXF code, But Postive Value in `DIMASZ` Means Center Mark is Visible
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 141 2.5 T))

        ; No Dimension Breaks Related DXF Code
        ; `DIMFXLON`. Arc Length Symbol Location
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 90 1 nil))

        ; No `JOGANG` Jog Angle Related DXF Code
        ; No Jog Height Facotor Related DXF Code

        ;; Tab "Text"
        ; No Text Style Related DXF Code

        ; No `DIMTFILL` Text Fill Related DXF Code
        ; No Draw Frame around Dimension Text

        ; `DIMTAD`. Text Vertical Placement
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 77 1 nil))

        ; `DIMJUST`. Text Horizontal Placement
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 280 0 nil))

        ; ; No `DIMTEXTDIRECTION` Text View Direction Related DXF Code.

        ; `DIMGAP`. Text Offset from Dimension Line
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 147 0.625 T))

        ; ISO Text Alginment Standard
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 74 1 nil))

        ;; Tab "Fit"
        ; `DIMATFIT`. Fit Options
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 289 3 nil))

        ; `DIMTMOVE`. Text Move with Dimension
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 279 0 nil))

        ; `DIMUPT`. Place Text Mannually
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 288 0 nil))

        ; `DIMTOFL`. Draw Line Between Extension Lines(Diameter Dimension)
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 172 1 nil))

        ;; Tab "Primatry Units"
        ; `DIMLUNIT`. Unit Type
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 277 2 nil))

        ; `DIMDEC`. Number of Decimals
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 271 2 nil))

        ; `DIMDSEP`. Decimal Separator
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 278 46 nil))

        ; `DIMRND`. Rounding Value
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 45 0 nil))

        ; `DIMLFAC`. Scale Factor
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 144 1.0 nil))

        ; Suppress Leading Zeros and Trailing Decimals in Primary Unit Value
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 78 9 nil))

        ; `DIMAUNIT`. Unit Type for Angular Units
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 275 0 nil))

        ; `DIMADEC`. Number of Decimals for Angular Units
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 179 2 nil))

        ; Suppress Leading Zeros and Trailing Decimals for Angular Units
        (setq dimStyleEntData (dimStyleMod dimStyleEntData sizeFactor 79 2 nil))


        (entmod dimStyleEntData)
        (setq dimStyleModifiedCount (1+ dimStyleModifiedCount))
      )
    )
  )

  ; Modify drawn dimensions
  ; I don't want to use the "._dimoverride" command to override all dimensions mentioned in this post: https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/how-to-change-textstyles-in-all-dimensions-and-leaders-in-an/m-p/7064499#M120471
  ; Nither do I want to select dimension one by one as it's hefty work as well. What I implement here is adjust all the sizable element in dimentsion based on the text height of each dimension.

  (setq i 0)
  (if (setq ss (ssget ssFilter '((0 . "*DIMENSION")))) 
    (progn 
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))


        (dimObjModify obj dimTextStyleEnt)

        (setq i (1+ i))
        (vla-update obj)
      )
    )
  )

  (command "undo" "e")
  (setvar "CMDECHO" 1)
  (terpri)
  (princ (strcat "已修改 " (itoa dimStyleModifiedCount) " 个标注样式。\n"))
  (if (> i 0) 
    (princ (strcat "已修改 " (itoa i) " 个标注实体。\n"))
    (princ "没有修改可以修改的标注实体。\n")
  )

  (princ)
)

(defun dimStyleMod (entData sizeFactor assocNum val sizeAdjustChk / 
                    textOverrideUnformat
                   ) 
  ; Credit: https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/lisp-code-to-change-the-overall-scale-for-all-dimension-objects/td-p/7968485
  (if sizeAdjustChk 
    (setq val (* val sizeFactor))
  )
  (if (assoc assocNum entData) 
    (setq entData (subst (cons assocNum val) (assoc assocNum entData) entData))
    (setq entData (append entData (list (cons assocNum val))))
  )

  entData
)

(defun dimObjModify (obj dimFontSetupChk / sizeFactor) 
  (setq sizeFactor (/ 
                     (vlax-get-property obj 'TextHeight)
                     2.5
                   )
  )
  ;; Tab "Lines"
  ; TODO: No baseline spacing tweak yet

  ; Dimension Line Suppressions
  (if 
    (and 
      (vlax-property-available-p obj 'DimLine1Suppress)
      (= (vlax-get-property obj 'DimLine1Suppress) :vlax-true)
    )
    (vlax-put-property obj 'DimLine1Suppress :vlax-false)
  )
  (if 
    (and 
      (vlax-property-available-p obj 'DimLine2Suppress)
      (= (vlax-get-property obj 'DimLine2Suppress) :vlax-true)
    )
    (vlax-put-property obj 'DimLine2Suppress :vlax-false)
  )

  ; Dimension Extension Line Suppressions
  (if 
    (and 
      (vlax-property-available-p obj 'ExtLine1Suppress)
      (= (vlax-get-property obj 'ExtLine1Suppress) :vlax-true)
    )
    (vlax-put-property obj 'ExtLine1Suppress :vlax-false)
  )
  (if 
    (and 
      (vlax-property-available-p obj 'ExtLine2Suppress)
      (= (vlax-get-property obj 'ExtLine2Suppress) :vlax-true)
    )
    (vlax-put-property obj 'ExtLine2Suppress :vlax-false)
  )

  ; Extension Beyond Dimension Line
  (if 
    (and 
      (vlax-property-available-p obj 'ExtensionLineExtend)
      (/= sizefactor 1)
    )
    (vlax-put-property obj 'ExtensionLineExtend (* sizeFactor 1.25))
  )

  ; Extension Beyond Dimension Line
  (if 
    (and 
      (vlax-property-available-p obj 'ExtensionLineOffset)
      (/= sizefactor 1)
    )
    (vlax-put-property obj 'ExtensionLineOffset (* sizeFactor 0.625))
  )

  ; Dimension Fixed Length Extension Line Suppressions
  (if 
    (and 
      (vlax-property-available-p obj 'ExtLineFixedLenSuppress)
      (= (vlax-get-property obj 'ExtLineFixedLenSuppress) :vlax-true)
    )
    (vlax-put-property obj 'ExtLineFixedLenSuppress :vlax-false)
  )

  ;; Tab "Symbols and Arrows"
  ; Arrowhead Types
  (if 
    (and 
      (vlax-property-available-p obj 'Arrowhead1Type)
      (/= (vlax-get-property obj 'Arrowhead1Type) 0)
    )
    (vlax-put-property obj 'Arrowhead1Type 0)
  )
  (if 
    (and 
      (vlax-property-available-p obj 'Arrowhead2Type)
      (/= (vlax-get-property obj 'Arrowhead2Type) 0)
    )
    (vlax-put-property obj 'Arrowhead2Type 0)
  )

  ; Arrow Head Size
  (if 
    (and 
      (vlax-property-available-p obj 'ArrowheadSize)
      (/= sizefactor 1)
    )
    (vlax-put-property obj 'ArrowheadSize (* sizeFactor 2.5))
  )

  ; Center Mark Size
  (if 
    (and 
      (vlax-property-available-p obj 'CenterMarkSize)
      (/= sizefactor 1)
    )
    (vlax-put-property obj 'CenterMarkSize (* sizeFactor 2.5))
  )

  ; Jog Angle
  (if 
    (and 
      (vlax-property-available-p obj 'JogAngle)
      (/= (vlax-get-property obj 'JogAngle) 45)
    )
    (vlax-put-property obj 'JogAngle 45)
    nil
  )

  ;; Tab "Text"

  ; Text Style
  (if 
    (and 
      dimFontSetupChk
      ; (vlax-property-available-p obj 'TextStyle)
      (/= (vlax-get-property obj 'TextStyle) "斜仿宋")
    )
    (vlax-put-property obj 'TextStyle "斜仿宋")
  )

  ; Text Fill
  (if (/= (vlax-get-property obj 'TextFill) 0) 
    (vlax-put-property obj 'TextFill 0)
  )

  ; Text Horizontal Alignment
  (if 
    (and 
      (vlax-property-available-p obj 'HorizontalTextPosition)
      (/= (vlax-get-property obj 'HorizontalTextPosition) acHorzCentered)
    )
    (vlax-put-property obj 'HorizontalTextPosition acHorzCentered)
  )

  ; Text Vertical Alignment
  (if 
    (and 
      (vlax-property-available-p obj 'VerticalTextPosition)
      (/= (vlax-get-property obj 'VerticalTextPosition) acAbove)
    )
    (vlax-put-property obj 'VerticalTextPosition acAbove)
  )

  ; Text Dimension Direction
  (if 
    (and 
      *AutoCADLoaded*
      (vlax-property-available-p obj 'DimTxtDirection)
      (= (vlax-get-property obj 'DimTxtDirection) :vlax-true)
    )
    (vlax-put-property obj 'DimTxtDirection :vlax-false)
  )

  ; Text Gap
  (if 
    (and 
      (vlax-property-available-p obj 'TextGap)
      (/= sizefactor 1)
    )
    (vlax-put-property obj 'TextGap (* sizeFactor 0.625))
  )

  ; Text Alignment
  (if 
    (and 
      (vlax-property-available-p obj 'TextInsideAlign)
      (vlax-property-available-p obj 'TextOutsideAlign)
      (or 
        (/= (vlax-get-property obj 'TextInsideAlign) 0)
        (/= (vlax-get-property obj 'TextOutsideAlign) -1)
      )
    )
    (progn 
      (vlax-put-property obj 'TextInsideAlign 0)
      (vlax-put-property obj 'TextOutsideAlign -1)
    )
  )

  ; Remove fontype override in value
  (if 
    (and 
      (setq textOverride (vlax-get-property obj 'TextOverride))
      (wcmatch textOverride "*\*;*")
      (not (wcmatch textOverride "*GDT*"))
      (not (wcmatch textOverride "*gdt*"))
    )
    ; #Check diameter symbol appearence count and skip it
    (progn 
      (if (setq textOverrideUnformat (LM:UnFormat textOverride :vlax-false)) 
        (progn 
          (vlax-put-property 
            obj
            'TextOverride
            textOverrideUnformat
          )
        )
      )
    )
  )

  ;; Tab "Adjustment"
  ; Fit
  (if 
    (and 
      (vlax-property-available-p obj 'Fit)
      (/= (vlax-get-property obj 'Fit) acTextAndArrows)
    )
    (vlax-put-property obj 'Fit acTextAndArrows)
  )

  ; Text Placement
  (if 
    (and 
      (vlax-property-available-p obj 'TextMovement)
      (/= (vlax-get-property obj 'TextMovement) acDimLineWithText)
    )
    (vlax-put-property obj 'TextMovement acDimLineWithText)
  )

  ; Always Draw Dimensiion Line
  (if 
    (and 
      (vlax-property-available-p obj 'ForceLineInside)
      (= (vlax-get-property obj 'ForceLineInside) :vlax-false)
    )
    (vlax-put-property obj 'ForceLineInside T)
  )

  ;; Primary Units
  ; TODO: Specify Units

  ; Decimal Separator
  (if (/= (vlax-get-property obj 'DecimalSeparator) ".") 
    (vlax-put-property obj 'DecimalSeparator ".")
  )

  ; Rounding
  (if (/= (vlax-get-property obj 'RoundDistance) 0) 
    (vlax-put-property obj 'RoundDistance 0)
  )

  ; Always Set Scale Factor to 1
  ; (if 
  ;   (and 
  ;     (vlax-property-available-p obj 'ScaleFactor)
  ;     (/= (vlax-get-property obj 'ScaleFactor) 1)
  ;   )
  ;   (vlax-put-property obj 'ScaleFactor 1)
  ; )

  ; Suppress Leading Zeros
  (if 
    (and 
      (vlax-property-available-p obj 'SuppressLeadingZeros)
      (= (vlax-get-property obj 'SuppressLeadingZeros) :vlax-true)
    )
    (vlax-put-property obj 'SuppressLeadingZeros 0)
  )

  ; Suppress Trailing Zeros
  (if 
    (and 
      (vlax-property-available-p obj 'SuppressTrailingZeros)
      (= (vlax-get-property obj 'SuppressTrailingZeros) :vlax-false)
    )
    (progn 
      (vlax-put-property obj 'SuppressTrailingZeros 1)
    )
  )

  ; Angle Format
  (if 
    (and 
      (vlax-property-available-p obj 'AngleFormat)
      (/= (vlax-get-property obj 'AngleFormat) acDegrees)
    )
    (vlax-put-property obj 'AngleFormat acDegrees)
  )

  ; TODO: No Angle Zero Suppression yet
  (princ)
)
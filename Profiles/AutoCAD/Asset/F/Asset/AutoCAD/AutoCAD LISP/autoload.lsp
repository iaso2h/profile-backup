;; Otto
(autoload "otto" '("otto"))
(autoload "ottoPlot" '("ottoPlotNameUpdate" "ottoPlotRatioUpdate"))

;; File
(autoload "fileOpenContainingFolder" '("fileOpenContainingFolder"))
(autoload "fileExportSelected" '("fileExportSelected"))

;; Debug
(autoload "whatIs" '("whatIs"))
(autoload "whatIsInside" '("whatIsInside"))

;;Utilities
(autoload "getLength" '("getLength" "getLengthAverage"))
(autoload "optimize" '("optimize"))

;; Select
(if (not *tchLoaded*) 
  (autoload "selectSimilar" '("ss"))
)
(autoload "selectDim" '("selectDim" "sed"))
(autoload "selectChain" '("selectChain" "sec"))

;; Alignment & Space
(if (not *tchLoaded*) 
  (progn 
    (autoload "alignCoordinate" '("alignCoordinate"))
    (autoload "space" '("space"))
    (autoload "spaceSpecific" '("spaceSpecific"))
    (autoload "spaceOrigin" '("spaceOrigin"))
  )
)

;; AddSelected Plus
(autoload "addSelectedPlus" '("addSelectedPlus"))

;; Move & Copy
(if (not *tchLoaded*) 
  (progn 
    (autoload "freeMove" '("freeMove"))
    (autoload "freePaste" '("freePaste"))
  )
)

;; Block
(autoload "blockBreakLine" 
          '("blockBreakMove" "blockBreakInplace" "blockBreakInplaceSelection")
)
(autoload "blockAddObj" '("blockAddObj"))
(autoload "blockCreateInplace" 
          '("blockCreateInplace" "blockCreateInplaceByBlock")
)
(autoload "blockDel" '("blockDel"))
(autoload "blockRebase" '("blockRebase"))
(autoload "blockNameRandom" '("blockNameRandom"))
(autoload "blockNestedMove" '("blockNestedMove"))
(autoload "blockNewInstance" '("blockNewInstance"))
(autoload "blockColor" '("blockColor"))


;; Xref
(autoload "xrefLayerMerge" '("xrefLayerMerge"))
(autoload "xrefAddObj" '("xrefAddObj"))


;; PolyLine
(if *autoCADLoaded* 
  (autoload "doubleOffset" '("doubleOffset"))
)
(autoload "plineOffset" '("plineOffset"))
(autoload "plineLengthen" '("plineLengthen"))
(autoload "plineConvert" '("plineConvert" "2`"))
(autoload "plineContinue" '("plineContinue" "`c"))
(autoload "plineSubtract" '("plineSubtract" "`s"))
(autoload "plineUnion" '("plineUnion" "`a"))
(autoload "plineHeal" '("plineHeal" "`h"))
(autoload "plineWidthExpand" '("plineWidthExpand" "plineWidthExpandMultiple" "`ww" "`w"))

;; Font & Text
(autoload "fontGBSwap" '("fontGBSwap"))
(autoload "font2Standard" '("font2Standard" "font2StandardAll"))
(autoload "fontStandardize" '("fontStandardize"))
(autoload "fontSwap" '("fontSwap"))
(autoload "textCopySwap" '("textCopy" "textSwap"))
(autoload "textMerge" '("textMerge"))
(autoload "text2Mtext" '("text2Mtext"))
(autoload "textAlign" '("textAlign"))
(autoload "attr2Text" '("attr2Text" "attr2TextAll"))

;; Dimension
(autoload "dimTangentAdapt" '("dimTangentToggle" "dimTangentAdapt"))
(autoload "dimTangent" '("dimTangent"))
(autoload "dimContinuePlus" '("dimContinuePlus"))
(autoload "dimSpacePlus" '("dimSpacePlus" "dimSpacePlusView"))
(autoload "dimSWFix" '("dimSWFix"))
(autoload "dimSelectOverrided" '("dimSelectOverrided"))
(autoload "dimSelectPrecision" '("dimSelectPrecision"))
(autoload "dimByBlock" '("dimByBlock"))
(autoload "dimRemoveFontOverride" '("dimRemoveFontOverride"))

;; Hatch
(autoload "hatchMerge" '("hatchMerge" "hMerge"))

;; Change Color
(defun c:mapColor () (colorHotkeyBinding))
(defun colorHotkeyBinding () 
  (defun okc (color / ss1) 
    (setq ss1 (ssget))
    (command "._change" ss1 "" "p" "c" color "")
    (princ)
  )
  (setq i 1)
  (while (<= i 255) 
    (eval 
      (read (strcat "(defun c:" (itoa i) (chr 40) (chr 41) "(okc " (itoa i) "))"))
    )
    (setq i (1+ i))
  )
  (princ)
)
(colorHotkeyBinding)

;; Load APLUS
(defun c:APLUS () 
  (vl-load-all "aplus.vlx")
  (colorHotkeyBinding)
  (princ)
)

;; Load YSTool
(defun c:YSTOOL () 
  (arxload "YSTools2021X.arx")
  (princ)
)

;; Batch Plot
(defun c:BPLoad () 
  (vl-load-all "BatchPlot.vlx")
  (princ)
)

(princ)

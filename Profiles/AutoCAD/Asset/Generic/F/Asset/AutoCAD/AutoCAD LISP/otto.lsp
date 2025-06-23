(defun c:otto (/ ans) 
  (princ "\n")
  (initget "S R N")
  (setq ans (getkword "选择功能[规范化SoldiWorks出图(S)/打印图框比例调整(R)/打印图框更新名称(N)]<S>：\n"))
  (cond 
    ((= ans "S")
     (progn 
       (c:xrefLayerMerge)
       (c:dimSWFix)
       (c:fontStandardize)
     )
    )
    ((= ans "R")
     (c:ottoPlotRatioUpdate)
    )
    ((= ans "N")
     (c:ottoPlotNameUpdate)
    )
    (t
     null
    )
  )

  (princ)
)

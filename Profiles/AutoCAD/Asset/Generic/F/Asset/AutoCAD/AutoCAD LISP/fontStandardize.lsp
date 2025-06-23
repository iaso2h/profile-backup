(defun c:fontStandardize (/ *acad* *doc* *n-regen* ss i ent obj dimstyles dimStyles 
                          textStyleNameInDimension textStyleEnt textStyleName count
                         ) 
  (vl-load-com)
  (setvar "cmdecho" 0)
  (setq *acad* (vlax-get-acad-object))
  (setq *doc* (vla-get-ActiveDocument *acad*))
  (vla-StartUndoMark *doc*)
  (setq *n-regen* 1)
  (setq count 0) 

  ;;; Collect all text styles used in dimension styles
  (setq textStyleNameInDimension '())
  (setq dimStyles (tblnext "DIMSTYLE" T)) ; Get first DIMSTYLE entry

  (while dimStyles 
    (setq textStyleEnt (cdr (assoc 340 dimStyles))) ; Text style name (group code 3)
    (setq textStyleName (vla-get-name (vlax-ename->vla-object textStyleEnt)))
    (if (not (member textStyleName textStyleNameInDimension)) 
      (setq textStyleNameInDimension (cons textStyleName textStyleNameInDimension))
    )
    (setq dimStyles (tblnext "DIMSTYLE")) ; Get next entry
  )


  (if (setq ss (ssget "_X" '((0 . "*DIMENSION,MULTILEADER")))) 
    (repeat (setq i (sslength ss)) 
      (setq ent (ssname ss (setq i (1- i))))
      (setq obj (vlax-ename->vla-object ent))
      (if (vlax-property-available-p obj 'textStyle) 
        (setq textStyleName (vla-get-TextStyle obj))
        (setq textStyleName (vla-get-TextStyleName obj)) ; Get text style from dimension/mleader
      )
      (if (not (member textStyleName textStyleNameInDimension)) 
        (setq textStyleNameInDimension (cons textStyleName 
                                             textStyleNameInDimension
                                       )
        )
      )
    )
  )

  (vlax-for x (vla-get-textstyles *doc*) 
    ; Only modify if text style is used in dimension styles
    (if 
      (and 
        (member (vla-get-name x) textStyleNameInDimension)
        (not 
          (vl-string-search "仿宋_gb2312.ttf" (strcase (vla-get-fontfile x) T))
        )
      )
      (progn 
        (vla-put-fontfile x "tssdeng")
        (vla-put-bigfontfile x "tssdchn")
        (vla-put-width x 0.8)
        (vla-put-obliqueangle x (* pi (/ 6.0 180.0)))
        (setq count (1+ count))
      )
      (if 
        (or 
          ; (vl-string-search "仿宋.ttf" (strcase (vla-get-fontfile x) T))
          (vl-string-search "仿宋_gb2312.ttf" (strcase (vla-get-fontfile x) T))
          ; (vl-string-search "simfang.ttf" (strcase (vla-get-fontfile x) T))
        )
        (progn 
          (vla-put-fontfile x "C:\\Windows\\Fonts\\simfang.ttf")
          (setq count (1+ count))
        )
      )
    ) 
  )
  
  (princ (strcat "\nModified " (itoa count) " text styles.\n"))


  (repeat *n-regen* (vla-regen *doc* 0))
  (vla-EndUndoMark *doc*)
  (setvar "cmdecho" 1)
  (princ)
)

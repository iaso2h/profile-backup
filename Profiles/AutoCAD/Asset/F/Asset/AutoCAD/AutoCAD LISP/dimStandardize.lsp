; https://help.autodesk.com/view/OARX/2024/ENU/?guid=GUID-F2FAD36F-0CE3-4943-9DAD-A9BCD2AE81DA
;

(defun c:dimStandardize (/ ss i ent vlaObj dimTextOverride) 
  (vl-load-com)
  (princ "\n")
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (princ (strcat "Error: " msg "\n"))
    )
    (princ)
  )
  (if *searchIncluded* 
    (progn 
      (load "util.lsp")
    )
  )

  (if (setq ss (ssget "_X" '((0 . "*DIMENSION")))) 
    (progn 
      (command "undo" "be")
      (setq i 0)
      (setq cnt 0)
      (while (< i (sslength ss)) 
        (setq ent (ssname ss i))
        (setq vlaObj (vlax-ename->vla-object ent))
        (setq dimTextOverride (vla-get-TextOverride vlaObj))

        ; Remove dim override
        (if (wcmatch dimTextOverride "*\*;*") 
          (vla-put-TextOverride vlaObj (LM:UnFormat dimTextOverride nil))
        )

        ; Text Horizontal Alignment
        (if (/= (vlax-get-property vlaObj 'HorizontalTextPosition) acHorzCentered) 
          (vlax-put-property vlaObj 'HorizontalTextPosition acHorzCentered)
        )
        ; Text Vertical Alignment
        (if (/= (vlax-get-property vlaObj 'VerticalTextPosition) acVertCentered) 
          (vlax-put-property vlaObj 'VerticalTextPosition acVertCentered)
        )

        (if (not (vlax-get-property vlaObj 'SuppressTrailingZeros)) 
          (vlax-put-property vlaObj 'SuppressTrailingZeros T)
        )

        (setq i (1+ i))
      )
      (command "undo" "e")
      (if (> cnt 0) (princ (strcat (rtos cnt 2 0) "\n个标准尺寸的覆盖文字已经被清除\n\n")))
    )
  )

  (princ)
)

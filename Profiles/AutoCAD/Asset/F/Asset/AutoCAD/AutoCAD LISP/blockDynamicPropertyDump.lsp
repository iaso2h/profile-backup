(defun c:blockDynamicPropertyDump (/ ss obj) 
  (vl-load-com)
  (if 
    (not 
      (setq ss (ssget "_I"))
    )
    (vl-catch-all-error-p 
      (setq ss (vl-catch-all-apply 'ssget (list "_:S+." '((0 . "INSERT")))))
    )
  )
  (if 
    (and 
      (setq obj (vlax-ename->vla-object (ssname ss 0)))
      (vlax-property-available-p obj 'IsDynamicBlock)
    )
    (foreach p 
      (vlax-safearray->list 
        (vlax-variant-value (vla-getdynamicblockproperties obj))
      )
      (vlax-dump-object p)
    )
    (prompt "Not a DynaBlock!\n")
  )

  (princ)
)
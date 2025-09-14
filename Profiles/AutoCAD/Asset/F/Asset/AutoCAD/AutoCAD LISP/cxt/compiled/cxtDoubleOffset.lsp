(defun cxtDoubleOffset (ent / *error* _StartUndo _EndUndo DoubleOffset doc 
                                exitflag layer mpoint obj object of point symbol value
                               ) 
  (vl-load-com)
  (defun _StartUndo (doc) (vla-StartUndoMark doc))

  (defun _EndUndo (doc) 
    (if (= 8 (logand 8 (getvar 'UNDOCTL))) (vla-EndUndomark doc))
  )

  (defun DoubleOffset (object offset layer) 
    (mapcar 
      (function 
        (lambda (o) 
          (if 
            (and 
              (not 
                (vl-catch-all-error-p 
                  (setq o (vl-catch-all-apply 
                            (function vlax-invoke)
                            (list object 'Offset o)
                          )
                  )
                )
              )
              layer
            )
            (mapcar 
              (function 
                (lambda (o) 
                  (vla-put-layer o (getvar 'CLAYER))
                )
              )
              o
            )
          )
        )
      )
      (list offset (- offset))
    )
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (_EndUndo doc)
  (_StartUndo doc)

  (DoubleOffset 
    (vlax-ename->vla-object ent)
    (/ *CXTHeatingWireDesignWidth* 2.0)
    T
  )
  (_EndUndo doc)
  (princ)
)   
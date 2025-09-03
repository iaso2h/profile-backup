;;说明:判断两图元是否相交，相交则无任何操作，不相交则判断两图元是否延伸相交，如延伸相交，则延伸entname1至entname2最近点;;返回:相交则提示相交，不相交且延伸相交则返回延伸相交点，否则则提示图元无相交点
; http://bbs.mjtd.com/forum.php?mod=viewthread&tid=178314&highlight=%C5%D0%B6%CF
(defun C:ff (/ bf-list-split data enpt ent1 ent2 extendpt lst ptlst var)  ;BY-忘霄-qq：702099480(2018.10.8)
  (defun BF-list-split (lst x / lst2)  ;BY 落魄山人-qq：403009819
    (foreach n lst 
      (if (and lst2 (/= x (length (car lst2)))) 
        (setq lst2 (cons (append (car lst2) (list n)) (cdr lst2)))
        (setq lst2 (cons (list n) lst2))
      )
    )
    (reverse lst2)
  )
  (setq ent1 (vlax-ename->vla-object (car (entsel "\n选择对象1或延伸对象：")))
        ent2 (vlax-ename->vla-object (car (entsel "\n选择对象2或被延伸对象：")))
  )
  (if 
    (not 
      (> 
        (vlax-safearray-get-u-bound 
          (vlax-variant-value (vla-IntersectWith ent1 ent2 acExtendNone))
          1
        )
        1
      )
    )
    ;判断两图元是否相交
    (if 
      (> 
        (vlax-safearray-get-u-bound 
          (setq var (vlax-variant-value (vla-IntersectWith ent1 ent2 acExtendBoth)))
          1
        )
        1
      ) ;判断两图元是否延伸相交
      (progn 
        (setq lst   (BF-list-split (vlax-safearray->list var) 3)
              enpt  (car lst)
              ptlst (mapcar '(lambda (x) (cdr x)) 
                            (vl-remove-if-not '(lambda (x) (= (car x) 10)) 
                                              (setq data (entget 
                                                           (vlax-vla-object->ename ent1)
                                                         )
                                              )
                            )
                    )
        )
        (if 
          (equal (angle (nth 1 ptlst) (nth 0 ptlst)) 
                 (angle (nth 0 ptlst) enpt)
                 1e-8
          )
          (progn 
            (setq extendpt (cdr (assoc 10 data))
                  lst      (vl-sort lst 
                                    '(lambda (a b) 
                                       (< (distance extendpt a) 
                                          (distance extendpt b)
                                       )
                                     )
                           )
                  enpt     (car lst)
            )
            (entmod (setq data (subst (cons 10 enpt) (assoc 10 data) data)))
          )
          (progn 
            (setq extendpt (cdr (assoc 10 data))
                  lst      (vl-sort lst 
                                    '(lambda (a b) 
                                       (< (distance extendpt a) 
                                          (distance extendpt b)
                                       )
                                     )
                           )
                  enpt     (car lst)
                  data     (reverse data)
                  data     (reverse (subst (cons 10 enpt) (assoc 10 data) data))
            )
            (entmod data)
          )
        )
        (prompt (strcat "\n两图元延伸后最近相交于点" (vl-prin1-to-string enpt) "！"))
      )
      (prompt "\n两图元无相交点！")
    )
    (prompt "\n两图元相交！")
  )
  (prin1)
)
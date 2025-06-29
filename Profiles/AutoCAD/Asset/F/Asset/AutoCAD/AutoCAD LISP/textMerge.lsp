; http://bbs.mjtd.com/forum.php?mod=viewthread&tid=187082&highlight=%BA%CF%B2%A2
(defun c:textMerge (/ sort_text_by_column1 sslst textlst scale)
  ;;按行排列文字,nscale为字高的倍数，设为0.5，即文字竖向间距小于0.5倍字高，则按一行考虑
  (defun sort_text_by_column1
            (sstext  nscale  /       n       rtnlst
             y       rtnlst1 rtnlst2 space1  space2
             aa      bb      cc      dd
            )
    (setq n -1
    rtnlst nil
    )
    (repeat (sslength sstext)
      (setq rtnlst (cons (ssname sstext (setq n (1+ n))) rtnlst))
    )
    (setq rtnlst
     (vl-sort
       rtnlst
       '(lambda (a b)
    (setq a  (vlax-ename->vla-object a)
          b  (vlax-ename->vla-object b)
    )
    (vla-GetBoundingBox a 'aa 'bb)
    (vla-GetBoundingBox b 'cc 'dd)
    (if
      (< (abs (- (vlax-safearray-get-element aa 1)
           (vlax-safearray-get-element cc 1)
        )
         )
         (abs
           (* nscale
        (- (vlax-safearray-get-element bb 1)
           (vlax-safearray-get-element aa 1)
        )
           )
         )
      )
       (< (vlax-safearray-get-element aa 0)
          (vlax-safearray-get-element cc 0)
       )
       (> (vlax-safearray-get-element aa 1)
          (vlax-safearray-get-element cc 1)
       )
    )
        )
     )
    )
    (setq y (cadr (zgx-get-dxf 10 (car rtnlst) 1)))
    (setq rtnlst1 nil
    rtnlst2 nil
    )
    (mapcar
      '(lambda (x)
   (vla-GetBoundingBox (vlax-ename->vla-object x) 'aa 'bb)
   (if
     (< (abs (- (cadr (zgx-get-dxf 10 x 1)) y))
        (* nscale
     (abs (- (vlax-safearray-get-element bb 1)
       (vlax-safearray-get-element aa 1)
          )
     )
        )
     )
      (progn
        (setq rtnlst1 (append rtnlst1 (list x)))
      )
      (progn
        (setq rtnlst2 (append rtnlst2 (list rtnlst1)))
        (setq y (cadr (zgx-get-dxf 10 x 1)))
        (setq rtnlst1 nil
        rtnlst1 (append rtnlst1 (list x))
        )
      )
   )
       )
      rtnlst
    )
    (setq rtnlst2 (append rtnlst2 (list rtnlst1)))
  )
  ;;----------------------------------------------
  (defun zgx-chg-dxf (en code newdata / endata)
    (setq endata (entget en))
    (if  (assoc code endata)
      (setq
  endata (subst (cons code newdata) (assoc code endata) endata)
      )
      (setq
  endata (append endata (list (cons code newdata)))
      )
    )
    (entmod endata)
  )
  (defun zgx-get-dxf (code entname kk)
    (if  (= kk 2)
      (assoc code (entget entname))
      (cdr (assoc code (entget entname)))
    )
  )
  ;;----------------------------------------------
  (prompt "\n选择需要合并的文字[更改间距系数]:")
  (setq sslst (ssget '((0 . "text,swr_text"))))

  (while (not sslst)
    (setq scale (getreal "\n输入间距系数[默认0.5]:"))
    (if  (not scale)
      (setq scale 0.5)
    )
    (prompt "\n选择需要合并的文字[更改间距系数]:")
    (setq sslst (ssget '((0 . "text,swr_text,tch_text"))))
  )

  (if (not scale)
    (setq scale 0.5)
  )
  (setq  sslst  (sort_text_by_column1 sslst scale)
  textlst  (mapcar  '(lambda (c)
         (apply 'strcat c)
       )
      (mapcar  '(lambda (x)
           (mapcar '(lambda (a)
                (zgx-get-dxf 1 a 1)
              )
             x
           )
         )
        sslst
      )
    )
  )
  (vla-startundomark
    (vla-get-ActiveDocument (vlax-get-acad-object))
  )
  ;;改变每行第一个文字值
  (mapcar '(lambda (a b)
       (zgx-chg-dxf (car a) 1 b)
     )
    sslst
    textlst
  )
  (setq  sslst (apply 'append
         (mapcar 'cdr
           sslst
         )
        )
  )
  (foreach n sslst
    (entdel n)
  )
  (vla-endundomark
    (vla-get-ActiveDocument (vlax-get-acad-object))
  )
  (princ "\n文字合并结束!")
  (princ)
)
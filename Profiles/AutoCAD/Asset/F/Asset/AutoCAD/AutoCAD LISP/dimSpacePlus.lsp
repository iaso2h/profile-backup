;; credit: http://bbs.mjtd.com/forum.php?mod=viewthread&tid=181931&highlight=%BC%E4%BE%E0d
;;; 　　　      《标注整理》v4.1
;;; ===================================================
;;; 功能：水平标注和垂直标注整理成等距离格式
;;; 使用：1单视图整理：命令bzzl，把一个视图连同标注选全，
;;;       2单方向整理：命令zl，选择同一个方向的标注整理，根据字体高度的自动调整间距
;;; 作者：langjs    qq:59509100     日期:2011年11月30日
;;; ===================================================
;;;
;;; 单方向标注整理主程序
(defun c:dimSpacePlus (/ bili ent hlst i lst name p0 p10 p10x p10y p13 p13x p13y p14 p14x p14y pan pany shezi ss uu vv zhigao)
  (setvar "CMDECHO" 0)
  (command ".UNDO" "BE")
  (vl-load-com)
  (setq bili (getvar "DIMSCALE"))
  (setq ss (ssget '((0 . "DIMENSION"))))
  (setq ss (ssgengxin ss))
  (setq zhigao (biaozzg ss))
  (setq pany (* zhigao bili 3))
  (setq shezi "R")
  (while (/= (type shezi) 'list)
    (initget "S ")
    (princ (strcat "\n指定尺寸偏移起点，或<不改变>：[默认尺寸间距<" (rtos pany 2 1) ">重新设置(S)]"))
    (setq shezi (getpoint ""))
    (if (= shezi "S")
      (setq pany (getreal (strcat "\n设置尺寸间距:<" (rtos pany 2 1) ">")))
      (setq p0 shezi)
    )
  )
  (if (/= p0 nil)
    (fenxiangxianbiaozhu ss p0 pany)
  )
  (setq p00 (getpoint "\n指定引出线位置，或<不改变>："))
  (if p00
    (repeat (setq i (sslength ss))
      (setq ent (ssname ss (setq i (1- i))))
      (setq ent (entget ent))
      (jisuanshuju01 ent p00)
      (gengxinchichunjiexian01 ent np13 np14)
    )
    (princ)
  )
  (command ".UNDO" "E")
  (princ)
)
;;; 单视图标注整理主程序
(defun c:c:dimSpacePlusView (/ bili end end_data ent i maxp maxx maxx0 maxy maxy0 minp minx minx0 miny miny0 name p10 p10x p10y p13 p13x p13y p14
                 p14x p14y pan pany pmax pmin ss ss00 ss01 ss02 ss03 ss04 zhigao
              )
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (command ".UNDO" "BE")
  (setq bili (getvar "DIMSCALE"))
  (setq ss00 (ssget '((0 . "DIMENSION,LINE,LWPOLYLINE,INSERT"))))
  (setq zhigao (biaozzg ss00))
  (setq pany (* zhigao bili 1.6))      ; 此处设置“默认尺寸间距”为字高的1.6倍
  (setq ss (ssadd)
        ss01 (ssadd)
        ss02 (ssadd)
        ss03 (ssadd)
        ss04 (ssadd)
  )
  (repeat (setq i (sslength ss00))
    (setq name (ssname ss00 (setq i (1- i))))
    (if (= (cdr (assoc 0 (entget name))) "DIMENSION")
      (progn
        (setq ent (entget name))
        (setq p10 (cdr (assoc 10 ent))
              p13 (cdr (assoc 13 ent))
              p14 (cdr (assoc 14 ent))
              p10x (car p10)
              p10y (cadr p10)
              p13x (car p13)
              p13y (cadr p13)
              p14x (car p14)
              p14y (cadr p14)
        )
        (if (> (fix (+ 0.5 p10y)) (fix (+ 0.5 p14y)))
          (setq ss01 (ssadd name ss01))
        )
        (if (> (fix (+ 0.5 p10x)) (fix (+ 0.5 p14x)))
          (setq ss02 (ssadd name ss02))
        )
        (if (< (fix (+ 0.5 p10y)) (fix (+ 0.5 p14y)))
          (setq ss03 (ssadd name ss03))
        )
        (if (< (fix (+ 0.5 p10x)) (fix (+ 0.5 p14x)))
          (setq ss04 (ssadd name ss04))
        )
      )
      (setq ss (ssadd name ss))
    )
  )
  (setq ss (lguolv ss))
  (if (>= (sslength ss) 1)
    (progn
      (setq minx0 1e6
            miny0 1e6
            maxx0 -1e6
            maxy0 -1e6
      )
      (repeat (setq i (sslength ss))
        (setq end (ssname ss (setq i (1- i))))
        (setq end_data (entget end))
        (vla-getboundingbox (vlax-ename->vla-object end) 'minp 'maxp)
        (setq minp (vlax-safearray->list minp)
              maxp (vlax-safearray->list maxp)
              minx (car minp)
              maxx (car maxp)
              miny (cadr minp)
              maxy (cadr maxp)
        )
        (if (> minx0 minx)
          (setq minx0 minx)
        )
        (if (> miny0 miny)
          (setq miny0 miny)
        )
        (if (< maxx0 maxx)
          (setq maxx0 maxx)
        )
        (if (< maxy0 maxy)
          (setq maxy0 maxy)
        )
      )
      (setq pmin (list minx0 miny0)
            pmax (list maxx0 maxy0)
      )
      (fenxiangxianbiaozhu ss01 pmax pany)
      (fenxiangxianbiaozhu ss02 pmax pany)
      (fenxiangxianbiaozhu ss03 pmin pany)
      (fenxiangxianbiaozhu ss04 pmin pany)
    )
  )
  (command ".UNDO" "E")
  (princ)
)
;;; 分方向标注子函数
(defun fenxiangxianbiaozhu (ss p0 pany / ent hlst i lst name p10 p10x p10y p13 p13x p13y p14 p14x p14y uu vv)
  (setq lst '()
        hlst '()
  )
  (repeat (setq i (sslength ss))
    (setq name (ssname ss (setq i (1- i))))
    (setq ent (entget name))
    (setq p10 (cdr (assoc 10 ent))
          p13 (cdr (assoc 13 ent))
          p14 (cdr (assoc 14 ent))
          p10x (car p10)
          p10y (cadr p10)
          p13x (car p13)
          p13y (cadr p13)
          p14x (car p14)
          p14y (cadr p14)
    )
    (cond
      ((= (fix (+ 0.5 p10x)) (fix (+ 0.5 p14x)))
        (if (< p13x p14x)
          (setq lst (cons (list name p13x p14x) lst))
          (setq lst (cons (list name p14x p13x) lst))
        )
      )
      ((= (fix (+ 0.5 p10y)) (fix (+ 0.5 p14y)))
        (if (< p13y p14y)
          (setq hlst (cons (list name p13y p14y) hlst))
          (setq hlst (cons (list name p14y p13y) hlst))
        )
      )
      (t
        (princ)
      )
    )
  )
  (setq uu 0
        vv 1
  )
  (biaozhu lst p0 uu vv pany)               ; 处理水平标注
  (setq uu 1
        vv 0
  )
  (biaozhu hlst p0 uu vv pany)               ; 处理垂直标注
  (princ)
)
;;; 计算坐标点，尺寸更新到合适位置子函数
(defun biaozhu (lst p0 uu vv pany / bili chansu dim1 ent fuh fuh1 i lst_p1314x lst_p1314y lst02 lst04 n name p0x p0y p10 p10x p10y
                    p11 p11x p11y p13 p13x p13y p14 p14x p14y pl pmax pmin
               )
  (setq bili (getvar "DIMSCALE")
        pl (getvar "DIMEXE")
        n 1
  )
  (while (> (length lst) 0)               ; 如果标注还有标注列表着循环
    (setq p0x (car p0)
          p0y (cadr p0)
          lst02 (lstbak lst)               ; 将列表备份一个
          lst04 '()
          lst_p1314x '()
          lst_p1314y '()
    )                                       ; 对列表的标注循环
    (repeat (setq i (length lst))
      (setq dim1 (nth (setq i (1- i))
                      lst
                 )
      )
      (setq name (car dim1)
            pmin (cadr dim1)
            pmax (caddr dim1)
      )
      (setq chansu (baohan dim1 lst))  ; 判断这个元素是否包含其它尺寸如无则更新。
      (if (= chansu "F")
        (progn
          (setq ent (entget name))
          (setq p10 (cdr (assoc 10 ent))
                p11 (cdr (assoc 11 ent))
                p13 (cdr (assoc 13 ent))
                p14 (cdr (assoc 14 ent))
                p10x (car p10)
                p10y (cadr p10)
                p11x (car p11)
                p11y (cadr p11)
                p13x (car p13)
                p13y (cadr p13)
                p14x (car p14)
                p14y (cadr p14)
          )
          (if (> p10y p13y)
            (setq fuh 1)
            (setq fuh -1)
          )
          (if (> p10x p13x)
            (setq fuh1 1)
            (setq fuh1 -1)
          )
          (setq p10 (list (+ (* vv p10x) (* uu p0x) (* uu fuh1 n pany)) (+ (* uu p10y) (* vv p0y) (* vv fuh n pany))))
          (setq p11 (list (+ (* vv p11x) (* uu p0x) (* uu fuh1 n pany)) (+ (* uu p11y) (* vv p0y) (* vv fuh n pany))))
          (setq lst02 (vl-remove dim1 lst02))
          (setq ent (subst
                      (cons 10 p10)
                      (assoc 10 ent)
                      ent
                    )
          )
          (setq ent (subst
                      (cons 11 p11)
                      (assoc 11 ent)
                      ent
                    )
          )
          (entmod ent)
        )
      )
    )
    (setq n (1+ n))
    (setq lst lst02)
  )
  (princ)
)
;;; 判断某个尺寸范围内是否有其它尺寸子函数
(defun baohan (dim1 lst / chansu dim2 e1 e2 i jili jili01 lst03 name name01 pmax pmax01 pmin pmin01)
  (setq name (car dim1)
        pmin (cadr dim1)
        pmax (caddr dim1)
        jili (sswr (- pmax pmin) 1)
        chansu "F"
        lst03 '()
  )
  (repeat (setq i (length lst))
    (setq name01 (car (nth (setq i (1- i))
                           lst
                      )
                 )
          pmin01 (cadr (nth i lst))
          pmax01 (caddr (nth i lst))
          jili01 (- pmax01 pmin01)
          dim2 (nth i lst)
    )
    (if (or
          (and
            (<= (sswr pmin 1) (sswr pmin01 1))
            (< (sswr pmax01 1) (sswr pmax 1))
          )
          (and
            (< (sswr pmin 1) (sswr pmin01 1))
            (<= (sswr pmax01 1) (sswr pmax 1))
          )
        )
      (setq chansu "T")
    )
    (if (or
          (and
            (< (sswr pmin 1) (sswr pmin01 1))
            (< (sswr pmax 1) (sswr pmax01 1))
            (< (sswr pmin01 1) (sswr pmax 1))
          )
          (and
            (< (sswr pmin01 1) (sswr pmin 1))
            (< (sswr pmax01 1) (sswr pmax 1))
            (< (sswr pmin 1) (sswr pmax01 1))
          )
        )
      (setq lst03 (cons (list name01 (sswr jili01 1)) lst03))
    )
  )
  (setq lst03 (vl-sort lst03 (function (lambda (e1 e2)
                                         (< (cadr e1) (cadr e2))
                                       )
                             )
              )
  )
  (if (>= (length lst03) 1)
    (progn
      (if (> jili (cadr (car lst03)))
        (setq chansu "T")
      )
      (repeat (setq i (length lst03))
        (if (= jili (cadr (nth (setq i (1- i))
                               lst03
                          )
                    )
            )
          (setq lst04 (cons (nth i lst03) lst04))
        )
      )
    )
  )
  (if (member (list name jili) lst04)
    (setq chansu "T")
  )
  (princ "\n程序正在计算，请稍后......")
  chansu
)
;;; 将误选的横纵标注（少数量）从选择集中删除子函数
(defun ssgengxin (ss / ent i name p10 p10x p10y p14 p14x p14y ss1 ss2)
  (setq ss1 (ssadd)
        ss2 (ssadd)
  )
  (repeat (setq i (sslength ss))
    (setq name (ssname ss (setq i (1- i))))
    (setq ent (entget name))
    (setq p10 (cdr (assoc 10 ent))
          p14 (cdr (assoc 14 ent))
          p10x (car p10)
          p10y (cadr p10)
          p14x (car p14)
          p14y (cadr p14)
    )
    (cond
      ((= (fix (+ 0.5 p10x)) (fix (+ 0.5 p14x)))
        (setq ss1 (ssadd name ss1))
      )
      ((= (fix (+ 0.5 p10y)) (fix (+ 0.5 p14y)))
        (setq ss2 (ssadd name ss2))
      )
      (t
        (princ)
      )
    )
  )
  (if (>= (sslength ss1) (sslength ss2))
    (setq ss ss1)
    (setq ss ss2)
  )
  ss
)
;;; 四舍五入函数，ent:实数，n：小数点保留位数
(defun sswr (ent n / fh)
  (if (>= ent 0.0)
    (setq fh +)
    (setq fh -)
  )
  (setq ent (/ (atof (itoa (fix (fh (* ent (expt 10 n)) 0.5)))) (expt 10 n)))
  ent
)
;;; 生成一个备份的列表
(defun lstbak (lst / i lst02)
  (setq lst02 '())
  (repeat (setq i (length lst))
    (setq lst02 (cons (nth (setq i (1- i))
                           lst
                      ) lst02
                )
    )
  )
  lst02
)
(defun lguolv (ss / ent ent1 i ssguol) ; 下面程序设置过滤中心线虚线条件
  (setq ssguol '("ACAD_ISO03W100" "ACAD_ISO02W100"
         "DASHED" "DASHED2"
         "DASHEDX2" "HIDDEN"
         "HIDDEN2" "HIDDENX2"
         "ACAD_ISO04W100" "ACAD_ISO08W100"
         "CENTER" "CENTER2"
         "CENTERX2" "DASHDOT"
         "DASHDOT2" "DASHDOTX2"
        )
  )                                       ; 下面程序将虚线中心线图层加入虚线过滤条件
  (setq ssguol (append
                 ssguol
                 (guolv-01 "ACAD_ISO03W100")
                 (guolv-01 "ACAD_ISO02W100")
                 (guolv-01 "DASHED")
                 (guolv-01 "DASHED2")
                 (guolv-01 "DASHEDX2")
                 (guolv-01 "HIDDEN")
                 (guolv-01 "HIDDEN2")
                 (guolv-01 "HIDDENX2")
                 (guolv-01 "ACAD_ISO04W100")
                 (guolv-01 "ACAD_ISO08W100")
                 (guolv-01 "CENTER")
                 (guolv-01 "CENTER2")
                 (guolv-01 "CENTERX2")
                 (guolv-01 "DASHDOT")
                 (guolv-01 "DASHDOT2")
                 (guolv-01 "DASHDOTX2")
               )
  )                                       ; 下面程序将选择集中随层的过滤掉
  (repeat (setq i (sslength ss))
    (setq ent (ssname ss (setq i (1- i))))
    (setq ent1 (entget ent))
    (if (and
          (member (cdr (assoc 8 ent1)) ssguol)
          (/= (cdr (assoc 0 ent1)) "INSERT")
          (= (assoc 6 ent1) nil)
        )
      (setq ss (ssdel ent ss))
    )
  )                                       ; 下面程序将选择集中其他层的过滤掉
  (repeat (setq i (sslength ss))
    (setq ent (ssname ss (setq i (1- i))))
    (setq ent1 (entget ent))
    (if (member (cdr (assoc 6 ent1)) ssguol)
      (setq ss (ssdel ent ss))
    )
  )
  ss
)
(defun guolv-01 (xianxing / layers)
  (setq layers '())
  (setq layers (get_layer_linetype xianxing)) ; 获取包含指定线型的图层
  layers
)
(defun get_layer_linetype (linetype / ly_info ly_infos tmplist)        ; 提取包含指定线型的图层
  (setq ly_infos (get_layer))
  (foreach ly_info ly_infos
    (if (= linetype (substr (cdr (assoc 6 ly_info)) 1 (strlen linetype)))
      (setq tmplist (append
                      tmplist
                      (list (cdr (assoc 2 ly_info)))
                    )
      )
    )
  )
  tmplist
)
(defun get_layer (/ layer_info layers) ; 返回当前图纸中图层信息
  (setq layer_info (tblnext "LAYER" t))
  (while (/= layer_info nil)
    (setq layers (append
                   layers
                   (list layer_info)
                 )
    )
    (setq layer_info (tblnext "LAYER"))
  )
  layers
)
;;; 计算坐标点子程序
(defun jisuanshuju01 (ent p00 / p00x p00y p0x p0y p10 p10x p10y p11 p11x p11y p13 p13x p13y p14 p14x p14y)
  (setq p00x (car p00)
        p00y (cadr p00)
  )                                       ; 取得标注各关键坐标点值
  (setq p10 (cdr (assoc 10 ent))
        p14 (cdr (assoc 14 ent))
        p11 (cdr (assoc 11 ent))
        p13 (cdr (assoc 13 ent))
        p10x (car p10)
        p10y (cadr p10)
        p14x (car p14)
        p14y (cadr p14)
        p11x (car p11)
        p11y (cadr p11)
        p13x (car p13)
        p13y (cadr p13)
  )                                       ; 判断横、纵坐标并计算对齐后的关键标注坐标点值
  (cond
    ((= (fix (+ 0.5 p10x)) (fix (+ 0.5 p14x)))
      (setq np13 (list p13x p00y 0.0)
            np14 (list p14x p00y 0.0)
      )
    )
    ((= (fix (+ 0.5 p10y)) (fix (+ 0.5 p14y)))
      (setq np13 (list p00x p13y 0.0)
            np14 (list p00x p14y 0.0)
      )
    )
    (t
      (exit)
    )
  )
  (princ)
)
(defun gengxinchichunjiexian01 (ent np13 np14) ; 对齐引出线子程序
  (setq ent (subst
              (cons 13 np13)
              (assoc 13 ent)
              ent
            )
  )
  (setq ent (subst
              (cons 14 np14)
              (assoc 14 ent)
              ent
            )
  )
  (entmod ent)
  (princ)
)
(defun biaozzg (ss / bl dim i lst name wzgd wzh)
  (setq lst '())
  (repeat (setq i (sslength ss))
    (setq name (ssname ss (setq i (1- i))))
    (if (= (cdr (assoc 0 (entget name))) "DIMENSION")
      (progn
        (setq dim (vlax-ename->vla-object name))
        (setq wzgd (vla-get-textheight dim)) ; 得到标注样式的文字高度
        (setq bl (vla-get-scalefactor dim)) ; 得到标注的调整比例
        (setq wzh (* wzgd bl))               ; 得到真正的文字高度
        (setq lst (cons wzh lst))
      )
    )
  )
  (setq lst (vl-sort lst '>))
  (car lst)
)

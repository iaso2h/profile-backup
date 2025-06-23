;; Credit: http://bbs.mjtd.com/forum.php?mod=viewthread&tid=187150&highlight=%B6%D4%C6%EB
;---图元根据高度或者宽度进行从大到小排列
(defun c:spaceSpecific(/ en ename enamep1 enbox enlst i jj lst n p p1 p9 ss style style2 tuyuan x y)
	(vl-load-com)
	(setq ss(ssget))(if(null ss)(quit))
	(setq p(car(zf-get-ssbox ss)));---用作排列的左下角（起点）
	(vl-cmdf "undo" "be")
  (setvar "cmdecho" 0)
	(setq jj(getdist "设置排列图元之间的间距：<500>"))(or jj(setq jj 500))
  (setq lst nil)
  (repeat(setq n(sslength ss))
    (setq en(ssname ss(setq n(1- n))))
    (setq enbox(zf-get-enbox en))
    (setq p1(car enbox))
    (setq p9(cadr enbox))
		(setq x(-(car p9)(car p1)));---获取单个图元的x长度（宽度）
    (setq y(-(cadr p9)(cadr p1)));---获取单个图元的y长度（高度）
    (setq tuyuan(cdr(assoc -1(entget en))));---获取图元名
    (setq lst(cons(list x y p1 p9 tuyuan)lst));---宽度X、高度Y、左下角坐标、右上角坐标、图元名五者构成的表
  )
	(initget "X Y")
	(setq style(getkword "图元按照什么规则排列？[宽度(X)/高度(Y)]<Y>"))(or style(setq style "Y"))
	(initget "X Y")
	(setq style2(getkword "图元按照什么方向排列？[X轴向(X)/Y轴向(Y)]<X>"))(or style2(setq style2 "X"))
	(if(= style "Y")
		(progn
			(setq lst(vl-sort lst(function(lambda(e1 e2)(<(cadr e1)(cadr e2))))));---按照图元高度从小到大排序表
			(if(= style2 "X")
				(progn
					(repeat(setq i(length lst))  
						(setq enlst(nth(setq i(1- i))lst));---从后往前开始返回图元名称
						(setq ename(last enlst));---获取第五项图元名
						(setq enamep1(caddr enlst));---获取第三项左下角坐标
						(setq x(car enlst));---获取第一项X宽度
						(vl-cmdf "move" ename "" "non" enamep1 "non" p)
						(setq p(list(+ jj x(car p))(cadr p)))
					)
				)
				(progn
					(repeat(setq i(length lst))  
						(setq enlst(nth(setq i(1- i))lst));---从后往前开始返回图元名称
						(setq ename(last enlst));---获取第五项图元名
						(setq enamep1(caddr enlst));---获取第三项左下角坐标
						(setq y(cadr enlst));---获取第二项y宽度
						(vl-cmdf "move" ename "" "non" enamep1 "non" p)
						(setq p(list(car p)(+ jj y(cadr p))))
					)
				)
			)
		)
		(progn
			(setq lst(vl-sort lst(function(lambda(e1 e2)(<(car e1)(car e2))))));---按照图元宽度从小到大排序表
			(if(= style2 "X")
				(progn
					(repeat(setq i(length lst))  
						(setq enlst(nth(setq i(1- i))lst));---从后往前开始返回图元名称
						(setq ename(last enlst));---获取第五项图元名
						(setq enamep1(caddr enlst));---获取第三项左下角坐标
						(setq x(car enlst));---获取第一项X宽度
						(vl-cmdf "move" ename "" "non" enamep1 "non" p)
						(setq p(list(+ jj x(car p))(cadr p)))
					)
				)
				(progn
					(repeat(setq i(length lst))  
						(setq enlst(nth(setq i(1- i))lst));---从后往前开始返回图元名称
						(setq ename(last enlst));---获取第五项图元名
						(setq enamep1(caddr enlst));---获取第三项左下角坐标
						(setq y(cadr enlst));---获取第二项y宽度
						(vl-cmdf "move" ename "" "non" enamep1 "non" p)
						(setq p(list(car p)(+ jj y(cadr p))))
					)
				)
			)
		)	
	)
  (vl-cmdf "undo" "e")
  (setvar "cmdecho" 1)
  (princ)
)
;---获取选择集包围盒
(defun zf-get-ssbox(ss / en maxx maxy minx miny n ssbox)
	(setq minx 1e38 miny 1e38 maxx -1e38 maxy -1e38)
	(repeat(setq n(sslength ss))
		(setq en(ssname ss(setq n(1- n))))
		(vla-getboundingbox(vlax-ename->vla-object en)'minp 'maxp)
		(setq minx(min minx(car(vlax-safearray->list minp))))
		(setq miny(min miny(cadr(vlax-safearray->list minp))))
		(setq maxx(max maxx(car(vlax-safearray->list maxp))))
		(setq maxy(max maxy(cadr(vlax-safearray->list maxp))))
	)
	(setq ssbox(list(list minx miny)(list maxx maxy)(list(*(+ minx maxx)0.5)(*(+ miny maxy)0.5))))
)
;---获取单个图元包围盒
(defun zf-get-enbox(en / center enbox maxp minp)
	(vla-getboundingbox(vlax-ename->vla-object en)'minp 'maxp)
	(setq minp(vlax-safearray->list minp))
	(setq maxp(vlax-safearray->list maxp))
	(setq center(mapcar '(lambda(x1 x2)(*(+ x1 x2)0.5))minp maxp))
	(setq enbox(list minp maxp center))
)
;; Credit: http://bbs.mjtd.com/forum.php?mod=viewthread&tid=187150&highlight=%B6%D4%C6%EB
;---ͼԪ���ݸ߶Ȼ��߿�Ƚ��дӴ�С����
(defun c:spaceSpecific(/ en ename enamep1 enbox enlst i jj lst n p p1 p9 ss style style2 tuyuan x y)
	(vl-load-com)
	(setq ss(ssget))(if(null ss)(quit))
	(setq p(car(zf-get-ssbox ss)));---�������е����½ǣ���㣩
	(vl-cmdf "undo" "be")
  (setvar "cmdecho" 0)
	(setq jj(getdist "��������ͼԪ֮��ļ�ࣺ<500>"))(or jj(setq jj 500))
  (setq lst nil)
  (repeat(setq n(sslength ss))
    (setq en(ssname ss(setq n(1- n))))
    (setq enbox(zf-get-enbox en))
    (setq p1(car enbox))
    (setq p9(cadr enbox))
		(setq x(-(car p9)(car p1)));---��ȡ����ͼԪ��x���ȣ���ȣ�
    (setq y(-(cadr p9)(cadr p1)));---��ȡ����ͼԪ��y���ȣ��߶ȣ�
    (setq tuyuan(cdr(assoc -1(entget en))));---��ȡͼԪ��
    (setq lst(cons(list x y p1 p9 tuyuan)lst));---���X���߶�Y�����½����ꡢ���Ͻ����ꡢͼԪ�����߹��ɵı�
  )
	(initget "X Y")
	(setq style(getkword "ͼԪ����ʲô�������У�[���(X)/�߶�(Y)]<Y>"))(or style(setq style "Y"))
	(initget "X Y")
	(setq style2(getkword "ͼԪ����ʲô�������У�[X����(X)/Y����(Y)]<X>"))(or style2(setq style2 "X"))
	(if(= style "Y")
		(progn
			(setq lst(vl-sort lst(function(lambda(e1 e2)(<(cadr e1)(cadr e2))))));---����ͼԪ�߶ȴ�С���������
			(if(= style2 "X")
				(progn
					(repeat(setq i(length lst))  
						(setq enlst(nth(setq i(1- i))lst));---�Ӻ���ǰ��ʼ����ͼԪ����
						(setq ename(last enlst));---��ȡ������ͼԪ��
						(setq enamep1(caddr enlst));---��ȡ���������½�����
						(setq x(car enlst));---��ȡ��һ��X���
						(vl-cmdf "move" ename "" "non" enamep1 "non" p)
						(setq p(list(+ jj x(car p))(cadr p)))
					)
				)
				(progn
					(repeat(setq i(length lst))  
						(setq enlst(nth(setq i(1- i))lst));---�Ӻ���ǰ��ʼ����ͼԪ����
						(setq ename(last enlst));---��ȡ������ͼԪ��
						(setq enamep1(caddr enlst));---��ȡ���������½�����
						(setq y(cadr enlst));---��ȡ�ڶ���y���
						(vl-cmdf "move" ename "" "non" enamep1 "non" p)
						(setq p(list(car p)(+ jj y(cadr p))))
					)
				)
			)
		)
		(progn
			(setq lst(vl-sort lst(function(lambda(e1 e2)(<(car e1)(car e2))))));---����ͼԪ��ȴ�С���������
			(if(= style2 "X")
				(progn
					(repeat(setq i(length lst))  
						(setq enlst(nth(setq i(1- i))lst));---�Ӻ���ǰ��ʼ����ͼԪ����
						(setq ename(last enlst));---��ȡ������ͼԪ��
						(setq enamep1(caddr enlst));---��ȡ���������½�����
						(setq x(car enlst));---��ȡ��һ��X���
						(vl-cmdf "move" ename "" "non" enamep1 "non" p)
						(setq p(list(+ jj x(car p))(cadr p)))
					)
				)
				(progn
					(repeat(setq i(length lst))  
						(setq enlst(nth(setq i(1- i))lst));---�Ӻ���ǰ��ʼ����ͼԪ����
						(setq ename(last enlst));---��ȡ������ͼԪ��
						(setq enamep1(caddr enlst));---��ȡ���������½�����
						(setq y(cadr enlst));---��ȡ�ڶ���y���
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
;---��ȡѡ�񼯰�Χ��
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
;---��ȡ����ͼԪ��Χ��
(defun zf-get-enbox(en / center enbox maxp minp)
	(vla-getboundingbox(vlax-ename->vla-object en)'minp 'maxp)
	(setq minp(vlax-safearray->list minp))
	(setq maxp(vlax-safearray->list maxp))
	(setq center(mapcar '(lambda(x1 x2)(*(+ x1 x2)0.5))minp maxp))
	(setq enbox(list minp maxp center))
)
;; Credit: http://bbs.mjtd.com/forum.php?mod=viewthread&tid=173294&highlight=%D6%F0%B5%E3
(vl-load-com)
(defun c:dimContinuePlus (/		  ENTX_DIM_1   ENTX_DIM_NEW P_1
	     SSX_1	  SS_1	       ENTSEL_1	    STR_OBJECTNAME
	    )
  ;;选择对齐/旋转标注
  ; https://www.cadtutor.net/forum/topic/72449-use-currently-selected-command-into-lisp-routine/
  (if (setq sset (ssget "_I"))
    (setq entsel_1 (ssname sset 0))
    (setq entsel_1 (car (entsel "选择一个标注，若无则切换至多选")))
  )
  (if (= entsel_1 nil)
    ;;选择对象为空，则框选
    (progn
      (princ "\n未找到标注，切换至多选模式")
      (setq ss_1 (ssget	'((-4 . "<OR")

			  (-4 . "<AND")
			  (0 . "Dimension")
			  (100 . "AcDbRotatedDimension")
			  (-4 . "AND>")

			  (-4 . "<AND")
			  (0 . "Dimension")
			  (100 . "AcDbAlignedDimension")
			  (-4 . "AND>")

			  (-4 . "OR>")
			 )
		 )
      )
    )
    ;;选择对象不为空，则构造选择集
    (progn
      ;;检测是否为标注
      (setq str_objectname
	     (vla-get-objectname
	       (vlax-ename->vla-object entsel_1)
	     )
      )
      (if (or (= (strcase str_objectname)
		 (strcase "AcDbAlignedDimension")
	      )
	      (= (strcase str_objectname)
		 (strcase "AcDbRotatedDimension")
	      )
	  )
	(princ)
	(progn
	  (princ "\n这不是标注!")
	  (quit)
	)
      )
      (setq ss_1 (ssadd))
      (setq ss_1 (ssadd entsel_1 ss_1))
    )
  )

  ;;选择集转vla-object表
  (setq ssx_1 (BR:SSGET_TO_VLIST ss_1))

  (while (setq p_1 (getpoint "\n指定一个点"))
    (setq p_1 (trans p_1 1 0))
    ;;判断点在列表的哪个标注的范围内
    (setq entx_dim_1 (br:PointProjectNearest_Dimension ssx_1 p_1))
    ;;标注断开,返回新标注
    (setq entx_dim_new
	   (br:AlignedRotatedDimension_CopyBreak entx_dim_1 p_1)
    )
    ;;构造新表
    (setq ssx_1 (cons entx_dim_new ssx_1))
    (command "dimtedit" "_L" "C")
  )
  
  (princ)
)

;;ssx_1 提供对齐标注或旋转标注的vla-object列表
;;p_1 参考点
;;返回参考点投影后对应的标注vla-object
(defun br:PointProjectNearest_Dimension	(ssx_1	       p_1
					 /	       D_DIFF
					 D_DIST_13_14_PRO
					 D_DIST_PRO_13 D_DIST_PRO_14_PRO
					 D_ROTATION    ENTGET_1
					 ENT_1	       LI_DIM_1
					 P_13	       P_14
					 P_14_PRO      P_PRO
					 P_ROT	       SS_1
					 STR_OBJECTNAME
					)
  (setq li_dim_1 (list))
  (foreach eachx_1 ssx_1
    (setq ent_1 (vlax-vla-object->ename eachx_1))
    (setq entget_1 (entget ent_1))
    ;;标注点1
    (setq p_13 (cdr (assoc 13 entget_1)))
    ;;标注点2
    (setq p_14 (cdr (assoc 14 entget_1)))
    ;;取得对象名
    (setq str_objectname (vla-get-objectname eachx_1))
    ;;标注角度
    (if	(= (strcase str_objectname)
	   (strcase "AcDbAlignedDimension")
	)
      (setq d_rotation (angle p_13 p_14))
      (setq d_rotation (cdr (assoc 50 entget_1)))
    )
    ;;标注旋转角度线
    (setq p_rot (polar p_13 d_rotation 1000.0))
    ;;投影点
    (setq p_pro (lm:projectpointtoline p_1 p_13 p_rot))
    (setq p_14_pro (lm:projectpointtoline p_14 p_13 p_rot))
    ;;投影点距离
    (setq d_dist_13_14_pro (distance p_13 p_14_pro))
    (setq d_dist_pro_13 (distance p_pro p_13))
    (setq d_dist_pro_14_pro (distance p_pro p_14_pro))
    ;;差距，用于判断点在哪个标注范围
    ;;数据范围零到负无穷
    (setq d_diff (- d_dist_13_14_pro d_dist_pro_13 d_dist_pro_14_pro))
    ;;组合vla-object和差距表
    (setq li_dim_1 (cons (list eachx_1 d_diff) li_dim_1))
  )
  ;;表排序，差距从小到大
  (setq	li_dim_1
	 (vl-sort li_dim_1
		  (function (lambda (e1 e2) (> (cadr e1) (cadr e2))))
	 )
  )
  ;;输出标注
  (car (car li_dim_1))
)


;;entx_1 - 标注vla-object
;;p_1 - 按提供的点分成两个标注
;;d_diff - 正值小数(如0.0000001)，最小点距离，判断点重合
;;重合返回nil
;;不重合返回新复制的标注
(defun br:AlignedRotatedDimension_CopyBreak (entx_1	p_1
					     /		D_ROTATION
					     ENTGET_1	ENTGET_2
					     ENTX_2	ENT_1
					     ENT_2	li_3points
					     P_2	P_13
					     P_14	P_14_PRO
					     P_PRO	P_ROT
					     STR_OBJECTNAME
					    )
  (setq ent_1 (vlax-vla-object->ename entx_1))
  (setq entget_1 (entget ent_1))
  (setq entx_2 (vla-copy entx_1))
  (setq ent_2 (vlax-vla-object->ename entx_2))
  (setq entget_2 (entget ent_2))
  ;;标注点1
  (setq p_13 (cdr (assoc 13 entget_1)))
  ;;标注点2
  (setq p_14 (cdr (assoc 14 entget_1)))
  ;;取得对象名
  (setq str_objectname (vla-get-objectname entx_1))
  ;;标注角度
  (if (= (strcase str_objectname)
	 (strcase "AcDbAlignedDimension")
      )
    (setq d_rotation (angle p_13 p_14))
    (setq d_rotation (cdr (assoc 50 entget_1)))
  )
  ;;标注旋转角度线
  (setq p_rot (polar p_13 d_rotation 1000.0))
  ;;投影点
  (setq p_pro (lm:projectpointtoline p_1 p_13 p_rot))
  (setq p_14_pro (lm:projectpointtoline p_14 p_13 p_rot))
  ;;点位若点在标注外调整
  (setq li_3points (list p_pro p_13 p_14_pro))
  (if (< (abs (sin d_rotation)) 0.707)
    ;;按X轴排序
    (setq li_3points
	   (vl-sort li_3points
		    (function (lambda (e1 e2) (> (car e1) (car e2))))
	   )
    )
    ;;按Y轴排序
    (setq li_3points
	   (vl-sort li_3points
		    (function (lambda (e1 e2) (> (cadr e1) (cadr e2))))
	   )
    )
  )
  (setq p_13 (car li_3points))
  (setq p_pro (cadr li_3points))
  (setq p_14 (caddr li_3points))
  ;;标注更新
  (setq entget_1 (br:assoc_update entget_1 13 p_13))
  (setq entget_1 (br:assoc_update entget_1 14 p_pro))
  (setq entget_2 (br:assoc_update entget_2 13 p_pro))
  (setq entget_2 (br:assoc_update entget_2 14 p_14))
  (entmod entget_1)
  (entmod entget_2)
  ;;示意线
  (grdraw (trans p_pro 0 1) (trans p_13 0 1) 1)
  (grdraw (trans p_pro 0 1) (trans p_14 0 1) 2)

  entx_2
)


;;####通用函数####

;;将ssget中的对象转成vla-object输出list
(defun br:ssget_to_vlist (t_ss_1 / t_1 t_list_1 t_en_1)
  (setq t_1 0)
  (setq t_list_1 (list))
  (repeat (sslength t_ss_1)
    (setq t_en_1 (vlax-ename->vla-object (ssname t_ss_1 t_1)))
    (setq t_list_1 (cons t_en_1 t_list_1))
    (setq t_1 (1+ t_1))
  )
  (reverse t_list_1)
)

;; project point onto line  -  lee mac
;; projects pt onto the line defined by p1,p2
(defun lm:projectpointtoline (pt p1 p2 / nm)
  (setq	nm (mapcar '- p2 p1)
	p1 (trans p1 0 nm)
	pt (trans pt 0 nm)
  )
  (trans (list (car p1) (cadr p1) (caddr pt)) nm 0)
)

;;提供表和assoc值，更新表
(defun br:assoc_update
       (p_list_name p_before_dot p_new_data / NEW_LIST OLD_LIST)
  (setq old_list (assoc p_before_dot p_list_name))
  (setq new_list (cons p_before_dot p_new_data))
  (setq p_list_name (subst new_list old_list p_list_name))
)
(princ)
;;;==================={ 自由移动V1.0 BY wowan1314 }================================;;;
;;;功能：实现MOVE的过程中镜像、旋转、放大、缩小、对齐、改基点、改转角、记忆移动。  ;;;
;;;                特别鸣谢: G版 不死猫 xshrimp                                    ;;;
;;;特别说明：左键点取位置,右键退出,F3开关捕捉,F8开关正交,距离可直接输入无需按键    ;;;
;;;          量取Z的意思是：复制距离可由屏幕两点来确认，方便后面空格来默认距离     ;;;
;;;================================================================================;;;
(DEFUN C:freeMove (/	    SS	   PT	  SIZE	 OLDOS	BB     PT1    NEARPT
	     G2	    H	   D	  LST	 PTX	PTY    PTT1   PTT2
	     PTT3   PTT4   AERROR Aerror_end	olderr PT0    SS0
	     APT1   JULI   zhuyi1 zhuyi2 F3	F8     pt0x   pt0y
	     STARTPT WS ASC REAL 
	    )
  (defun Aerror	(x)
    (Aerror_end)
    (AND oldos (COMMAND "ERASE" SS ""))
  )
  (defun Aerror_end ()
    (setq *error* olderr)
    (if	oldos
      (setvar "osmode" oldos)
    )
    (if	oldCM
      (setvar "cmdecho" oldCM)
    )
    (command "_.undo" "e")
    (REDRAW)
    (prinC)
  )
  (IF (SETQ SS0 (SSGET ":L"))
    (SETQ PT0 (getpoint "\n选择复制基点:"))
  )
  (IF (AND SS0 PT0)
    (PROGN
      (setq olderr  *error*
	    *error* Aerror
      )
      (setq oldos (getvar "osmode")
	    oldCM (getvar "cmdecho")
      )
      (setq F8 (getvar "ORTHOMODE")
	    F3 T ws  (vlax-Create-Object "WScript.Shell")
      )
      (setvar "osmode" 0)
      (setvar "cmdecho" 0)
      (setvar "nomutt" 0)
      (setq zhuyi1 "\n点取位置或\n[转90度(A)/左右翻(D)/上下翻(S)/对齐(F)/改转角(R)/改基点(T)/大1倍(+)/小一倍(-)/量取(Z)/默认<"
	    zhuyi2 "mm>(空格)]<退出>"
      )
      (command "_.undo" "be")
      (IF *JULI1*
	(setq juli *JULI1*)
	(setq juli 100)
      )
      (WHILE pt0
	(if pt1
	  (setq	pt   pt1
		pt0  pt1
		SS   SS0
		PT0X (mapcar '+ pt0 '(1 0 0))
		PT0y (mapcar '+ pt0 '(0 1 0))
	  )
	  (setq	pt   pt0
		SS   SS0
		PT0X (mapcar '+ pt0 '(1 0 0))
		PT0y (mapcar '+ pt0 '(0 1 0))
	  )
	)
	(IF PT1 (command "_.copy" SS "" "0,0" "@"))
	(PRINC (strcat zhuyi1 (rtos juli 2) zhuyi2))
	(while PT
	  (setq	BB	(grread T 5 1)
		STARTPT	(CADR BB)
	  )
	  (cond
	    ((= (car BB) 5)
	     (SETQ PT1 STARTPT)
	     (redRaw)
	     (setq size (* (getvar "viewsize") 2))

	     (if (AND F3
		      (gxl-Sel-ReDrawSel SS 2)
		      (setq
			nearpt (osnap PT1 "_ENDP,_MID,_INT,NEA")
		      )
		 )			; 取得的捕捉点,端点，中点，交点,最近点.
	       (PROGN
		 (setq g2 nearpt)
		 (setq h   (/ (getvar "viewsize")
			      (cadr (getvar "screensize"))
			   )
		       d   (getvar "pickbox")
		       lst (list (* d h) (* (- d 0.5) h) (* (+ d 0.5) h))
		       ptx (car g2)
		       pty (cadr g2)
		 )
		 (foreach x lst
		   (setq ptt1 (list (- ptx x) (- pty x))
			 ptt2 (list (+ ptx x) (- pty x))
			 ptt3 (list (+ ptx x) (+ pty x))
			 ptt4 (list (- ptx x) (+ pty x))
		   )
		   (grvecs
		     (list 2 ptt1 ptt2 ptt2 ptt3 ptt3 ptt4 ptt4 ptt1)
		   )
		 )
		 (setq pt1 g2)
		 (gxl-Sel-ReDrawSel SS 1)
	       )
	     )
	     (IF (AND (= G2 NIL) (= F8 1))
	       (PROGN
		 (setq PT1 STARTPT)
		 (IF
		   (OR (< (* pi 0.25) (ANGLE PT1 PT0) (* pi 0.75))
		       (< (* pi 1.25) (ANGLE PT1 PT0) (* pi 1.75))
		   )
		    (SETQ PT1
			   (inters pt1
				   (polar pt1
					  (+ (angle pt0 pt0Y) (* pi 0.5))
					  1.0
				   )
				   pt0
				   pt0Y
				   nil
			   )
		    )
		    (SETQ PT1
			   (inters pt1
				   (polar pt1
					  (+ (angle pt0 pt0X) (* pi 0.5))
					  1.0
				   )
				   pt0
				   pt0X
				   nil
			   )
		    )
		 )
	       )
	     )
	     (GRVECS
	       (LIST 1314
		     PT0
		     PT1
		     1314
		     STARTPT
		     (mapcar '+ (LIST size 0 0) STARTPT)
		     1314
		     STARTPT
		     (mapcar '- STARTPT (LIST size 0 0))
		     1314
		     STARTPT
		     (mapcar '- STARTPT (LIST 0 size 0))
		     1314
		     STARTPT
		     (mapcar '+ (LIST 0 size 0) STARTPT)
	       )
	     )
	     (COMMAND "MOVE" SS "" PT PT1)
	     (SETQ PT PT1
		   G2 NIL
	     )
	    )
	    ((= (car BB) 3) (SETQ PT NIL))
	    ((member (car BB) '(11 25)) (SETQ pt0 NIL) (EXIT))
	    ((member BB '((2 97) (2 65)))
	     (COMMAND "ROTATE" SS "" PT1 90)
	    )
	    ((member BB '((2 82) (2 114)))
	     (redraw)
	     (COMMAND "ROTATE" SS "" PT1 pause)
	    )
	    ((member BB '((2 70) (2 102)))
	     (REDRAW)
	     ;;对齐
	     (setvar "osmode" oldos)
	     (initget 1)
	     (IF (SETQ APT1 (getpoint "\n选择第一个源点:"))
	       (PROGN
		 (COMMAND "align" SS "" APT1)
		 (princ "\n选择第一个目标点:")
		 (COMMAND pause)
		 (princ "\n选择第二个源点:")
		 (COMMAND pause)
		 (princ "\n选择第二个目标点:")
		 (COMMAND pause)
		 (COMMAND "" "N")
		 (REDRAW)
		 (setvar "osmode" 0)
	       )
	       (PRINC (strcat zhuyi1 (rtos juli 2) zhuyi2))
	     )
	    )
	    ((member BB '((2 115) (2 83)))
	     (COMMAND "mirror" SS "" PT1 (mapcar '- pt1 '(1 0)) "Y")
	    )
	    ((member BB '((2 32)))
	     (IF JULI
	       (progn (setq pt1 (polar PT0 (ANGLE PT0 PT1) JULI))
		      (COMMAND "MOVE" SS "" PT PT1)
		      (SETQ PT nil)
	       )
	     )
	    )
	    ((member BB
		     '((2 46)
		       (2 49)
		       (2 48)
		       (2 50)
		       (2 51)
		       (2 52)
		       (2 53)
		       (2 54)
		       (2 55)
		       (2 56)
		       (2 57)
		      )
	     )
	     (redraw)
	     (setq 
		   ASC (CADR BB)
	     )
	     (setq real
		    (getreal
		      (car
			(list ""
			      (vlax-invoke-method ws 'sendkeys (chr asc))
			)
		      )
		    )
	     )
	     (setq pt1 (polar PT0 (ANGLE PT0 PT1) real))
	     (COMMAND "MOVE" SS "" pt PT1)
	     (SETQ *JULI1* real
		   JULI	*JULI1*
		   PT NIL
	     )
	    )
	    ((member BB '((2 90) (2 122)))
	     (redraw)
	     (setq
	       juli (getdist
		      (strcat "\n输入复制距离<" (rtos juli 2) ">:")
		    )
	     )
	     (setq pt1 (polar PT0 (ANGLE PT0 PT1) JULI))
	     (COMMAND "MOVE" SS "" PT PT1)
	     (SETQ PT nil
		   *JULI1* JULI
	     )
	    )
	    ((member BB '((2 100) (2 68)))
	     (COMMAND "mirror" SS "" PT1 (mapcar '- pt1 '(0 1)) "Y")
	    )
	    ((member BB '((2 43) (2 61)))
	     (COMMAND "scale" SS "" PT1 "2")
	    )
	    ((member BB '((2 116) (2 84)))
	     (setvar "osmode" oldos)
	     (redRaw)
	     (IF (setq pt (getpoint "\n请选择新基点:"))
	       (PRINC (strcat zhuyi1 (rtos juli 2) zhuyi2))
	       (exit)
	     )
	     (setvar "osmode" 0)
	    )
	    ((equal BB '(2 45))
	     (COMMAND "scale" SS "" PT1 "0.5")
	    )
	    ((equal BB '(2 6))
	     (if (= f3 NIL)
	       (progn (setq f3 T) (prompt "\n<对象捕捉 开>"))
	       (progn (setq f3 NIL) (prompt "\n<对象捕捉 关>"))
	     )
	     (redraw)
	    )
	    ((equal BB '(2 15))
	     (if (= f8 0)
	       (progn (setq f8 1) (prompt "\n<正交 开>"))
	       (progn (setq f8 0) (prompt "\n<正交 关>"))
	     )
	     (setvar "orthomode" f8)
	     (redraw)
	    )
	  )
	)
	;;END 内WHILE

      )
      ;;END 外WHILE
    )					;END progn
  )
  ;;end if
  (Aerror_end)
)

;;;==================================================================
;;gxl-Sel-ReDrawSel 重画选择集中的对象，Sel 为选择集或图元名 mode 为方式码
;;;重画选择集中的对象，mode 为方式码，
;;;方式码 1 在屏幕重画该选择集对象
;;;方式码 2 隐藏该选择集对象
;;;方式码 3 “醒目显示”该选择集对象
;;;方式码 4 取消“醒目显示”该选择集对象--------BY G版
;;;==================================================================
(defun gxl-Sel-ReDrawSel (Sel mode / m n)
  (if sel
    (progn
      (cond ((= 'pickset (type Sel))
	     (setq m (sslength Sel)
		   n 0
	     )
	     (repeat m
	       (redraw (ssname Sel n) mode)
	       (setq n (1+ n))
	     )
	    )
	    ((= 'ename (type Sel))
	     (redraw Sel mode)
	    )
      )
    )
  )
  T
)

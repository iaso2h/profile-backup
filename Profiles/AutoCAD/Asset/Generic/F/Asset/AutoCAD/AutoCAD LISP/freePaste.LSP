;;;==================={ 自由复制V1.8 BY wowan1314 }================================;;; 
;;; 功能：实现复制的过程中镜像、旋转、放大、缩小、对齐、改基点、改转角、记忆复制。         
;;; 特别鸣谢: G版 不死猫 xshrimp                                                     
;;; 特别说明：左键点取位置,右键退出,F3开关捕捉,F8开关正交,距离可直接输入无需按键          
;;; 量取Z的意思是：复制距离可由屏幕两点来确认，方便后面空格来默认距离                     
;;;===============================================================================;;; 
;;;缺点：1、对于圆心的捕捉毫无办法。2、对极轴的支持。3、所有command都改为VLA函数。				 
;;;===============================================================================;;;
(defun c:freePaste (/ pt1 pt0 pt oldcm f8 f3 ws aerror aerror_end ss0 ss draftobj 
                  str_osmode lst_osmode autosnapmarkersize autosnapmarkercolor zhuyi1 
                  zhuyi2 juli pt0x pt0y bb startpt size drag1 ghostpt distperpixel 
                  bold apt1 olderr x0 y0 x1 y1 z1 asc real
                 ) 
  (defun aerror (x) 
    (aerror_end)
    (and oldcm (command "ERASE" ss ""))
  )
  (defun aerror_end () 
    (setq *error* olderr)
    (if oldcm 
      (setvar "cmdecho" oldcm)
    )
    (command "_.undo" "e")
    (redraw)
    (princ)
  )
  (if (setq ss0 (ssget ":L")) 
    (setq pt0 (nth 1 (grread 5)))
  )
  (if (and ss0 pt0) 
    (progn 
      (setq olderr  *error*
            *error* aerror
      )
      (setq oldcm (getvar "cmdecho"))
      (setq f8 (getvar "ORTHOMODE")
            f3 (getvar "osmode")
            ws (vlax-create-object "WScript.Shell")
      )
      (setq draftobj (vla-get-drafting 
                       (vla-get-preferences (vlax-get-acad-object))
                     )
      )
      (setq str_osmode (get_osmode))
      (setq lst_osmode (gxl-strparse str_osmode ","))
      (setq autosnapmarkersize (vla-get-autosnapmarkersize draftobj))
      (setq autosnapmarkercolor (vla-get-autosnapmarkercolor draftobj))
      (setvar "cmdecho" 0)
      (setvar "nomutt" 0)
      (setq zhuyi1 "\n点取位置或\n[转90度(A)/左右翻(D)/上下翻(S)/对齐(F)/改转角(R)/改基点(T)/大1倍(+)/小一倍(-)/量取(Z)/默认<"
            zhuyi2 "mm>(空格)]<退出>"
      )
      (command "_.undo" "be")
      (if *juli* 
        (setq juli *juli*)
        (setq juli 100)
      )
      (if pt1 
        (setq pt  pt1
              pt0 pt1
              ss  ss0
        )
        (setq pt pt0
              ss ss0
        )
      )
      (command "_.copy" ss "" "non" "0,0" "non" "@")
      (princ (strcat zhuyi1 (rtos juli 2) zhuyi2))
      (while pt 
        (setq bb      (grread t 5 1)
              startpt (cadr bb)
        )
        (cond 
          ((= (car bb) 5)
           (setq pt1 startpt)
           (setvar 'lastpoint pt0)
           (redraw)
           (setq size (* (getvar "viewsize") 2))
           (setq drag1 (cadr bb))
           (if 
             (and (< 0 f3 16384) 
                  (gxl-sel-redrawsel ss 2)
                  (setq ghostpt (osnap drag1 str_osmode))
             )
             (progn 
               (setq pt1 ghostpt)
               (setq distperpixel (/ 
                                    (getvar "VIEWSIZE")
                                    (cadr (getvar "SCREENSIZE"))
                                  )
               )
               (setq bold (mapcar 
                            '*
                            (list distperpixel distperpixel distperpixel)
                            (list (+ autosnapmarkersize 0.5) 
                                  autosnapmarkersize
                                  (- autosnapmarkersize 0.5)
                            )
                          )
               )
               (foreach item bold 
                 (drawvecs 
                   ghostpt
                   (getgrvecs ghostpt drag1 lst_osmode)
                   item
                   autosnapmarkercolor
                 )
               )
               (gxl-sel-redrawsel ss 1)
             )
             (if (= f8 0) 
               (setq pt1 startpt)
               (progn 
                 (setq x0 (car pt0)
                       y0 (cadr pt0)
                       x1 (car pt1)
                       y1 (cadr pt1)
                       z1 (caddr pt1)
                 )
                 (if (> (abs (- x0 x1)) (abs (- y0 y1))) 
                   (setq pt1 (list x1 y0 z1))
                   (setq pt1 (list x0 y1 z1))
                 )
               )
             )
           )
           (grvecs 
             (list 1314 
                   pt0
                   pt1
                   1314
                   startpt
                   (mapcar '+ (list size 0 0) startpt)
                   1314
                   startpt
                   (mapcar '- startpt (list size 0 0))
                   1314
                   startpt
                   (mapcar '- startpt (list 0 size 0))
                   1314
                   startpt
                   (mapcar '+ (list 0 size 0) startpt)
             )
           )
           (command "MOVE" ss "" "non" pt "non" pt1)
           (setq pt      pt1
                 ghostpt nil
           )
          )
          ((= (car bb) 3) (setq pt nil))
          ((member (car bb) '(11 25)) (setq pt0 nil) (exit))
          ((member bb '((2 97) (2 65)))
           (command "ROTATE" ss "" "non" pt1 90)
          )
          ((member bb '((2 82) (2 114)))
           (redraw)
           (command "ROTATE" ss "" "non" pt1 pause)
          )
          ((member bb '((2 70) (2 102)))
           (redraw)
           (initget 1)
           (if (setq apt1 (getpoint "\n选择第一个源点:")) 
             (progn 
               (command "align" ss "" "non" apt1)
               (princ "\n选择第一个目标点:")
               (command pause)
               (princ "\n选择第二个源点:")
               (command pause)
               (princ "\n选择第二个目标点:")
               (command pause)
               (command "" "N")
               (redraw)
             )
             (princ (strcat zhuyi1 (rtos juli 2) zhuyi2))
           )
          )
          ((member bb '((2 115) (2 83)))
           (command "mirror" 
                    ss
                    ""
                    "non"
                    pt1
                    "non"
                    (mapcar '- pt1 '(1 0))
                    "Y"
           )
          )
          ((member bb '((2 32)))
           (if juli 
             (progn (setq pt1 (polar pt0 (angle pt0 pt1) juli)) 
                    (command "MOVE" ss "" "non" pt "non" pt1)
                    (setq pt nil)
             )
           )
          )
          ((member bb 
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
           (setq asc (cadr bb))
           (setq real (getreal 
                        (car 
                          (list "" 
                                (vlax-invoke-method ws 'sendkeys (chr asc))
                          )
                        )
                      )
           )
           (setq pt1 (polar pt0 (angle pt0 pt1) real))
           (command "MOVE" ss "" "non" pt "non" pt1)
           (setq *juli* real
                 juli   *juli*
                 pt     nil
           )
          )
          ((member bb '((2 90) (2 122)))
           (redraw)
           (setq juli (getdist 
                        (strcat "\n输入复制距离<" (rtos juli 2) ">:")
                      )
           )
           (setq pt1 (polar pt0 (angle pt0 pt1) juli))
           (command "MOVE" ss "" "non" pt "non" pt1)
           (setq pt     nil
                 *juli* juli
           )
          )
          ((member bb '((2 100) (2 68)))
           (command "mirror" 
                    ss
                    ""
                    "non"
                    pt1
                    "non"
                    (mapcar '- pt1 '(0 1))
                    "Y"
           )
          )
          ((member bb '((2 43) (2 61)))
           (command "scale" ss "" "non" pt1 "2")
          )
          ((member bb '((2 116) (2 84)))
           (redraw)
           (if (setq pt (getpoint "\n请选择新基点:")) 
             (princ (strcat zhuyi1 (rtos juli 2) zhuyi2))
             (exit)
           )
          )
          ((equal bb '(2 45))
           (command "scale" ss "" "non" pt1 "0.5")
          )
          ((equal bb '(2 6))
           (if (< f3 16384) 
             (progn (setq f3 (+ f3 16384)) (prompt "\n<对象捕捉 开>"))
             (progn (setq f3 (- f3 16384)) (prompt "\n<对象捕捉 关>"))
           )
           (setvar "OSMODE" f3)
           (redraw)
          )
          ((equal bb '(2 15))
           (if (= f8 0) 
             (progn (setq f8 1) (prompt "\n<正交 开>"))
             (progn (setq f8 0) (prompt "\n<正交 关>"))
           )
           (setvar "orthomode" f8)
           (redraw)
          )
        )
      )
    )
  )
  (aerror_end)
)
;;;================================================================== 
;;gxl-Sel-ReDrawSel 重画选择集中的对象，Sel 为选择集或图元名 mode 为方式码 
;;;重画选择集中的对象，mode 为方式码， 
;;;方式码 1 在屏幕重画该选择集对象 
;;;方式码 2 隐藏该选择集对象 
;;;方式码 3 “醒目显示”该选择集对象 
;;;方式码 4 取消“醒目显示”该选择集对象--------BY G版 
;;;================================================================== 
(defun gxl-sel-redrawsel (sel mode / m n) 
  (if sel 
    (progn 
      (cond 
        ((= 'pickset (type sel))
         (setq m (sslength sel)
               n 0
         )
         (repeat m 
           (redraw (ssname sel n) mode)
           (setq n (1+ n))
         )
        )
        ((= 'ename (type sel))
         (redraw sel mode)
        )
      )
    )
  )
  t
)
;;;分列字串 
(defun gxl-strparse (str del / pos lst) 
  (while (setq pos (vl-string-search del str)) 
    (setq lst (cons (substr str 1 pos) lst)
          str (substr str (+ pos 1 (strlen del)))
    )
  )
  (if (= " " del) 
    (vl-remove "" (reverse (cons str lst)))
    (reverse (cons str lst))
  )
)
;;;返回捕捉模式字串 
(defun get_osmode (/ cur_mode mode$) 
  (setq mode$ "")
  (if (< 0 (setq cur_mode (getvar "osmode")) 16383) 
    (mapcar 
      (function 
        (lambda (x) 
          (if (not (zerop (logand cur_mode (car x)))) 
            (if (zerop (strlen mode$)) 
              (setq mode$ (cadr x))
              (setq mode$ (strcat mode$ "," (cadr x)))
            )
          )
        )
      )
      '((1 "_end")
        (2 "_mid")
        (4 "_cen")
        (8 "_nod")
        (16 "_qua")
        (32 "_int")
        (64 "_ins")
        (128 "_per")
        (256 "_tan")
        (512 "_nea")
        (1024 "_qui")
        (2048 "_app")
        (4096 "_ext")
        (8192 "_par")
       )
    )
  )
  mode$
)
;;;返回捕捉标记Vecs 
(defun getgrvecs (pt dragpt lst / key) 
  (setq key t)
  (while (and key lst) 
    (if (equal (osnap dragpt (car lst)) pt 1e-6) 
      (setq key nil)
      (setq lst (cdr lst))
    )
  )
  (cdr 
    (assoc (car lst) 
           '(("_end" 
               ((-1 1) (-1 -1))
               ((-1 -1) (1 -1))
               ((1 -1) (1 1))
               ((1 1) (-1 1))
             )
             ;正方形
             ("_mid" 
               ((0 1.414) (-1.225 -0.707))
               ((-1.225 -0.707) (1.225 -0.707))
               ((1.225 -0.707) (0 1.414))
             )
             ;三角形
             ("_cen" 
               ((0 1) (-0.707 0.707))
               ((-0.707 0.707) (-1 0))
               ((-1 0) (-0.707 -0.707))
               ((-0.707 -0.707) (0 -1))
               ((0 -1) (0.707 -0.707))
               ((0.707 -0.707) (1 0))
               ((1 0) (0.707 0.707))
               ((0.707 0.707) (0 1))
             )
             ;圆
             ("_nod" 
               ((0 1) (-0.707 0.707))
               ((-0.707 0.707) (-1 0))
               ((-1 0) (-0.707 -0.707))
               ((-0.707 -0.707) (0 -1))
               ((0 -1) (0.707 -0.707))
               ((0.707 -0.707) (1 0))
               ((1 0) (0.707 0.707))
               ((0.707 0.707) (0 1))
               ((-1 1) (1 -1))
               ((-1 -1) (1 1))
             )
             ;圆+十字交叉
             ("_qua" 
               ((0 1.414) (-1.414 0))
               ((-1.414 0) (0 -1.414))
               ((0 -1.414) (1.414 0))
               ((1.414 0) (0 1.414))
             )
             ;旋转45°的正方形
             ("_int" 
               ((-1 1) (1 -1))
               ((-1 -1) (1 1))
               ((1 0.859) (-0.859 -1))
               ((-1 0.859) (0.859 -1))
               ((0.859 1) (-1 -0.859))
               ((-0.859 1) (1 -0.859))
             )
             ;十字交叉
             ("_ins" 
               ((-1 1) (-1 -0.1))
               ((-1 -0.1) (0 -0.1))
               ((0 -0.1) (0 -1.0))
               ((0 -1.0) (1 -1))
               ((1 -1) (1 0.1))
               ((1 0.1) (0 0.1))
               ((0 0.1) (0 1.0))
               ((0 1.0) (-1 1))
             )
             ;两个正方形
             ("_per" 
               ((-1 1) (-1 -1))
               ((-1 -1) (1 -1))
               ((0 -1) (0 0))
               ((0 0) (-1 0))
             )
             ;半个正方形
             ("_tan" 
               ((0 1) (-0.707 0.707))
               ((-0.707 0.707) (-1 0))
               ((-1 0) (-0.707 -0.707))
               ((-0.707 -0.707) (0 -1))
               ((0 -1) (0.707 -0.707))
               ((0.707 -0.707) (1 0))
               ((1 0) (0.707 0.707))
               ((0.707 0.707) (0 1))
               ((1 1) (-1 1))
             )
             ;园+线
             ("_nea" 
               ((-1 1) (1 -1))
               ((1 -1) (-1 -1))
               ((-1 -1) (1 1))
               ((1 1) (-1 1))
             )
             ;两个三角形
             ("_qui") ; ???
             ("_app" 
               ((-1 1) (-1 -1))
               ((-1 -1) (1 -1))
               ((1 -1) (1 1))
               ((1 1) (-1 1))
               ((-1 1) (1 -1))
               ((-1 -1) (1 1))
             )
             ;正方形+十字交叉
             ("_ext" 
               ((0.1 0) (0.13 0))
               ((0.2 0) (0.23 0))
               ((0.3 0) (0.33 0))
             )
             ;三个点
             ("_par" ((0 1) (-1 -1)) ((1 1) (0 -1))) ;两条线
            )
    )
  )
)
;;绘制捕捉标记 
(defun drawvecs (pt vecs size color / lst xdir) 
  (setq xdir (trans (getvar 'ucsxdir) 0 2))
  (setq vecs (mapcar 
               '(lambda (x) 
                  (mapcar 
                    '(lambda (a) 
                       (setq a (trans a 0 xdir))
                       (setq a (list (caddr a) (car a)))
                       (list (+ (car pt) (* size (car a))) 
                             (+ (cadr pt) (* size (cadr a)))
                       )
                     )
                    x
                  )
                )
               vecs
             )
  )
  (setq lst (mapcar 'cons 
                    (mapcar (function (lambda (x) color)) vecs)
                    vecs
            )
  )
  (grvecs (apply 'append lst))
) 
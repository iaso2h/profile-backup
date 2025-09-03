;; http://bbs.mjtd.com/forum.php?mod=viewthread&tid=180209&highlight=%D7%E9%C2%EB%B2%E9%BF%B4%C6%F7
;;修订记录
;;;;;英文版bug修正 by edata 2019年9月19日01:11:52
;;;;;2019年9月19日增加重选按钮，优化部分代码 by 半途中
;;;;;2019年9月19日增加清空剪切板避免干扰剪切板
(defun Str->list (lstr DelimLst)
  (setq lstr (list lstr))
  (foreach del DelimLst
    (setq lstr (apply 'append
		      (mapcar '(lambda (x) (str->lst x del)) lstr)
	       )
    )
  )
  (if (member " " DelimLst)
    (vl-remove "" lstr)
    lstr
  )
)
(defun str->lst (str del / pos)
  (if (setq pos (vl-string-search del str))
    (cons (substr str 1 pos)
	  (str->lst (substr str (+ pos 1 (strlen del))) del)
    )
    (list str)
  )
)
;复制剪切板
(defun get-clip_bord ( / clip_bord get)
  (or *htm
      (setq *htm (vlax-create-object "htmlfile"))
  )
  (setq	clip_bord (vlax-get-property
		    (vlax-get *htm 'parentwindow)
		    'clipboarddata
		  )
  )
  (setq get (vlax-invoke clip_bord 'getdata "text"))
)
(defun adlst (key lst)
;;;仅对popup_list或list_box有效
  (start_list key)
;;;处理列表开始
  (mapcar 'add_list lst)
  (end_list)
;;;添加列表结束
)
;设置剪切板
(defun set-clip-string (str /  result)
  (vl-load-com)
  (or *htm
      (setq *htm (vlax-create-object "htmlfile"))
  )
  (and (= (type str) 'str)

       (setq result (vlax-invoke
		      (vlax-get	(vlax-get *htm 'parentwindow)
				'clipboarddata
		      )
		      'setdata
		      "text"
		      str
		    )
       )
  )
)
;;列表1单击
(defun List1fun	(key value reason listdata / selnth num text str)
  (setq selnth (read (get_tile key)))
  (if selnth
    (progn
      (setq text (nth selnth Listdata))
      (setq text (str->lst text " "))
      (setq num (substr (car text) 2 5))
      (setq str (strcat "(Cdr(Assoc " num " (Entget (Car(Entsel)))))"))
      (set_tile	"textbox1"
		(strcat "(Cdr(Assoc " num " (Entget (Car(Entsel)))))")
      )
    )
  )
)					;列表2单击
(defun List2fun	(key value reason Listdata / tex selnth method str  canshu)
  (if (= reason 4)			;如果是双击
    (setq method "vlax-Put"  canshu " 参数)" )
    (setq method "Vlax-Get"  canshu ")")
  )
  (setq selnth (read (get_tile key)))
  (if selnth
    (progn
      (setq tex (nth selnth Listdata))
      (setq tex (str->lst tex " "))
      (setq name (car tex))
      (setq str	(strcat
		  "("
		  method
		  " (Vlax-Ename->Vla-Object (Car(Entsel))) '"
		  name
		  canshu
		  
		 )
      )
      (set_tile "textbox1" str)
    )
  )
)
(defun Strpad(str num)
	(setq strNew "")
	(repeat num
		(setq strNew (strcat str strNew))
	)
	strNew
)
;列表3单击
(defun List3fun	(key value reason Listdata / tex selnth method str cs)
  (setq selnth (read (get_tile key)))
  (if selnth
    (progn
      (setq tex (nth selnth Listdata))
      (setq tex (Str->lst tex " "))
      (setq cs (str->list (cadr tex) (list "(" ")")))
      (if (> (atoi(cadr cs)) 0)
				(setq str (Strpad " 参数 " (atoi(cadr cs))))
				(setq str "")
      )
      (setq str	(strcat	"(Vlax-Invoke-Method (Vlax-Ename->Vla-Object (Car(Entsel))) '"	(car tex)  str	")"))
      (set_tile "textbox1" str)
    )
  )
)
;生成dcl
(defun Zhumamake-dcl  (/ lst_str str file f)
		(setq lst_str '(
"shux:dialog{"
"label=\"组码查看器\";"
":row {"
":row {"
"label = \"组码\";"
":list_box{label=\"\";key=\"List1\";height=25;width=35;multiple_select=true;}"
"}"
":row{"
"label = \"属性\";"
":list_box{label=\"\";key=\"List2\";height=25;width=50;multiple_select=true;}"
"}"
":row{"
"label = \"方法\";"
":list_box{label=\"\";key=\"List3\";height=25;width=30;multiple_select=true;}"
"}"
"}"
":row{"
":edit_box{label=\"代码\";key=\"textbox1\";height=1;width=90;multiple_select=true;}"
":button{label=\"加载\";key=\"loadtocad\"; }"
":button{label=\"复制\";key=\"copy\"; }"
":button{label=\"重选\";key=\"repick\"; }"				
"cancel_button;"
" }"
"fixed_width=true;"
"}"
""
""
		    )
    )
(setq file (vl-filename-mktemp "DclTemp.dcl"))
    (setq f (open file "w"))
    (foreach str lst_str
	(princ "\n" f)
	(princ str f)
    )
    (close f)
    ;;返回
    file
)
;命令dxf
(defun c:dxf (/ maxlen idcl_id do_what lstshow ff ffn newtxt x en
	      dclfile display enget enget-3 enstr get list1 list2 list3 DisplayOld
	      methods object objstrlist preferences property recdclpt str lanid)
  (vl-load-com)
  (setq en (car (entsel "\n选择图元: ")))
  (setq dclfile (Zhumamake-dcl))
  (if dclfile
    (progn
      (setq idcl_id (load_dialog dclfile))
      (if (findfile dclfile)
	(vl-file-delete dclfile)
      )
    )
    (progn (alert "无法加载对话框") (exit))
  )
  (setq do_what 3)
  (while (member do_what '(3) )
   (if en
     (progn
       (princ "\n1")
       (or *acad
	   (setq *acad (vlax-get-acad-object))
       )
       (setq Preferences (vla-get-Preferences *acad))
       (setq Display (vla-get-Display Preferences))
       (setq DisplayOld (vla-get-HistoryLines DISPLAY))
       (vla-put-HistoryLines DISPLAY 25)
       
       (princ "\n2")
       (vla-put-HistoryLines DISPLAY 25)
       (REPEAT 25 (PRINC "\n") (PRINC))
       (vla-put-HistoryLines DISPLAY 500)
       (if en
	 (progn
	   (setq OBJECT (vlax-ename->vla-object en))
	   (vlax-dump-Object OBJECT T)
	 )
       )
       (command "_.COPYHIST")
       (vla-put-HistoryLines DISPLAY 25)
       (PRINT ".")
       (REPEAT 50 (PRINC "\n") (PRINC))
       (setq get (get-clip_bord))	;获取剪切板
       (set-clip-string "")             ;清空剪切板避免干扰剪切板
       ;dxf组码处理
       (setq enget (entget en))
       (setq enStr nil)
       (foreach	dxf enget
	 (setq enStr (cons (vl-princ-to-string dxf) enStr))
       )
       (setq enget-3 (assoc -3 (entget en (list "*"))))
       (if enget-3
	 (progn
	   (foreach dxf	enget-3
	     (setq enStr (cons (vl-princ-to-string dxf) enStr))
	   )
	 )
       )
       (princ "\n3")
       
       (if recdclpt
	 (new_dialog "shux" idcl_id "" recdclpt)
	 (new_dialog "shux" idcl_id)
       );建立窗体
;群码表
       (setq List1 (reverse enStr))
       (adlst "List1" List1)
;;英文版bug修正 by edata 2019年9月19日01:11:52
;;添加对英文版的支持
  (setq lanid(vl-registry-read (strcat "HKEY_LOCAL_MACHINE\\" (vlax-product-key)) "LocaleID"))
  (if (and lanid (= lanid "804"))
    (setq objStrList (Str->list GET '(";特性值:\r\n" ";支持的方法:\r\n")))
    (setq objStrList (Str->list GET '("; Property values:\r\n" "; Methods supported:\r\n")))
    )
;object属性
;;增加判断是否有返回值
(if objStrList
	 (progn
	   (setq property (str->lst (cadr objStrList) ";   "))
	   (setq list2 (cdr property))
	   (adlst "List2" List2)
;object方法
	   (if (caddr objStrList)
	     (progn
	       (setq methods (str->lst (caddr objStrList) ";   "))
	       (setq list3 (cdr methods))
	       (adlst "List3" List3)
	     )
	   )
	 )
       );end if
     )
   )
  (action_tile
    "List1"
    "(List1fun  \"List1\"  $value $reason  List1)"
  )
  (action_tile
    "List2"
    "(List2fun  \"List2\"  $value $reason  List2)"
  )
  (action_tile
    "List3"
    "(List3fun   \"List3\" $value $reason  List3)"
  )
  (action_tile "copy" "(setq str (get_tile \"textbox1\") )(SET-CLIP-STRING  str)")
  (action_tile "loadtocad" "(setq str (get_tile \"textbox1\") )(done_dialog 2)")
  (action_tile "repick" "(setq recdclpt (done_dialog 3))")
  (action_tile "cancel" "(setq recdclpt (done_dialog 0))")
  (setq do_what (start_dialog))
  (if (= do_what 3 )
    (PROGN
      (setq en  (car (entsel "\n选择图元: ")))
      )
    )
   )
  (unload_dialog idcl_id)
  (cond	((= do_what 2)
	 (and str
	      (print str)
	      (print (eval (read str)))
	 )
	)
   )
  ;恢复变量可以自己加错误处理函数
  (vla-put-HistoryLines DISPLAY DisplayOld)
  ;(set-clip-string "")      ;清空剪切板
  (princ)
)
(princ "\n命令dxf")
(princ)
;;;=================================================================*





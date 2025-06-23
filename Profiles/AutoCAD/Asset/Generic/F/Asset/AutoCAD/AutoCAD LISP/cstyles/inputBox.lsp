;;;标准输入对话框
;;;调用形式 ( InputBox  显示的对话框名称   表( ( "editbox显示的说明" "editbox显示值"  "editbox显示宽度")  ...)   )
;;;注意，调用参数均为字符串形式
;;;返回值为与输入表长度相等的字符串列表
(defun InputBox (strDialogName EditBoxDefList
                 / ResultList fStream dclname tempFileName fileN fileStream templist i
                 dclid)
;;;-------------------------------------------------                     
 (defun GetInput (len)
  (setq i 1
        ResultList nil
  )

  (repeat len
   (setq ResultList
         (append ResultList (list (get_tile (itoa i))))
   )

   (setq i (1+ i))
  )
 )
;;;-------------------------------------------------                      
 (setq tempFileName (vl-filename-mktemp "dcltmp.dcl"))
 (setq fileN (open tempFileName "w"))

 (setq fileStream (list
                   "InputBox:dialog {\n"
                   (strcat "label =\"" strDialogName "\";\n")
                  )
 )
 (setq i 0)
 (repeat (length EditBoxDefList)
  (setq templist (nth i EditBoxDefList))
  (setq fileStream (append fileStream
                           (list
                            ":edit_box{\n"
                            "allow_accept = true ;\n"
                            (strcat "edit_width =" (caddr templist) ";\n")
                            "fixed_width = true ;\n"
                            (strcat "key =\"" (itoa (1+ i)) "\";\n")
                            (strcat "label=\"" (car templist) "\";\n")
                            (strcat "value=\"" (cadr templist) "\";\n}\n")
                           )
                   )
  )

  (setq i (1+ i))
 )

 (setq fileStream (append fileStream
                          (list   "ok_cancel;\n}\n"  )
                  )
 )
 (foreach fStream
                  fileStream
  (princ fStream fileN)
 )
 (close fileN)

 (setq dclname tempFileName)


 (setq dclid (load_dialog dclname))
 (if (not (new_dialog "InputBox" dclid ""))
  (progn (alert "对话框加载失败!") (exit))
 )

 (action_tile "accept"   "(GetInput (length EditBoxDefList) ) (done_dialog 1)" )

 (start_dialog)

 (unload_dialog dclid)
 (vl-file-delete dclname)

 ResultList
)
;;;-------------------------------------------------
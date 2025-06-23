;;;��׼����Ի���
;;;������ʽ ( InputBox  ��ʾ�ĶԻ�������   ��( ( "editbox��ʾ��˵��" "editbox��ʾֵ"  "editbox��ʾ���")  ...)   )
;;;ע�⣬���ò�����Ϊ�ַ�����ʽ
;;;����ֵΪ�����������ȵ��ַ����б�
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
  (progn (alert "�Ի������ʧ��!") (exit))
 )

 (action_tile "accept"   "(GetInput (length EditBoxDefList) ) (done_dialog 1)" )

 (start_dialog)

 (unload_dialog dclid)
 (vl-file-delete dclname)

 ResultList
)
;;;-------------------------------------------------
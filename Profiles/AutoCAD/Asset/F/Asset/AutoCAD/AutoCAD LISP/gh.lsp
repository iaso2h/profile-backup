;;程序功能:自动添加菜单项 By Gu_xl 明经通道
;;菜单加载测试测试
(defun c:tt (/ items items1) 
  ;;'((标签 命令 帮助字串 次级菜单项)) 表为菜单项，注意命令后要有一个空格
  (setq items1 (list '("逆时针调整宗地界址点顺序" "\003\003CS:TZJZD " "逆时针调整宗地界址点顺序") 
                     '("--" nil nil) ;_ "--" 表示插入分隔符
                     '("曲线换向" "\003\003LWPOLYLINE_REVERSE " "曲线换向")
               )
  ) ;_ 下一级子菜单列表
  (setq items (list '("初始化CASS环境" "\003\003cs:InitCASS " "创建图层、加载字体、线型...") 
                    '("--" nil nil) ;_ "--" 表示插入分隔符
                    '("展野外测点" "\003\003cs:zdh " " 文件格式:点名,编码,Y（东）,X（北）,高程")
                    '("展高程点" "\003\003CS:ZGCD " " 文件格式:点名,编码,Y（东）,X（北）,高程")
                    '("小数点替代高程点" "\003\003CS:Gcd2Dot " " 小数点替代高程点")
                    '("稀释CASS填充符号" "\003\003cs:fhxs " " 稀释CASS填充符号")
                    '("--" nil nil) ;_ "--" 表示插入分隔符
                    (list "宗地<a href=" http://bbs.mjtd.com/forum-6-1.html " target=" 
                          _blank " class=" relatedlink ">工具</a>" nil nil items1
                    ) ;_ 含下一级子菜单，命令若为nil，表示还有下级子菜单
                    '("--" nil nil) ;_ "--" 表示插入分隔符
                    '("定位并绘制拆迁成果" "\003\003dwcqsj " " 定位并绘制拆迁成果")
                    '("绘制拆迁成果" "\003\003hzcqsj " " 绘制拆迁成果")
                    '("--" nil nil) ;_ "--" 表示插入分隔符
                    '("激活第一个配置" "\003\003ACTP0 " "激活第一个配置文件")
                    '("屏幕菜单开关" "\003\003Screenmmenu " "屏幕菜单开关")
                    ; '("访问Gu_xl网盘" "\003\003_Browser <a href=" http://guxl.ys168.com/ 
                    ;   " target=" _blank ">http://guxl.ys168.com/</a> " 
                    ;   "http://guxl.ys168.com/"
                    ;  )
              )
  )
  (Gxl-AddCassMenu "ACAD" ;_ 已有菜单组名称
                   "CASS工具" ;_ 显示的Pop菜单项名称
                   items ;_ Pop菜单项列表
                   "工具箱" ;_ 在菜单项"工具箱"之前插入，若为 nil,则插在最后
  )
  (princ)
)




;;(gxl-RemoveMenuItem POPName) 移除下拉菜单，成功返回T
;;; 例如: (gxl-RemoveMenuItem "CASS工具") 移除 “CASS工具” 菜单
(defun gxl-RemoveMenuItem (POPName / MenuBar n i MenuItem Menu tag) 
  (setq MenuBar (vla-get-menubar (vlax-get-acad-object)))
  ;; 找菜单 Item
  (setq menuitem (GXL-CATCHAPPLY 'vla-item (list MenuBar POPName)))
  (if menuitem (GXL-CATCHAPPLY 'vla-RemoveFromMenuBar (list menuitem)))
)
;;函数 Gxl-AddCassMenu 添加CASS菜单
;;;语法: (Gxl-AddCassMenu MenuGroupName POPName PopItems InsertBeforeItem) 
;;MenuGroupName 要插入的菜单组名称
;;POPName 下拉菜单名称
;;PopItems 下拉菜单列表
;;   如 '((标签 命令 帮助字串 次级子项)...) 表为下拉菜单列表，注意命令后要有一个空格
;;InsertBeforeItem 在该菜单条名称之前插入，例如 "工具箱"，若为 nil,则插在最后
(defun Gxl-AddCassMenu (MenuGroupName POPName PopItems InsertBeforeItem / MENUBAR N I 
                        MENUITEM POPUPMENU K TMP SUBPOPUPMENU
                       ) 
  ;;卸载原有菜单
  (gxl-RemoveMenuItem POPName)

  (setq MenuBar (vla-get-menubar (vlax-get-acad-object)))
  (if InsertBeforeItem 
    (progn 
      ;; 查找菜单“工具箱”
      (setq n (vla-get-count MenuBar))
      (setq i (1- n))
      (while 
        (and (>= i 0)  ; 没有超过上限
             (/= InsertBeforeItem 
                 (vla-get-name (setq menuitem (vla-item MenuBar i)))
             ) ; 找到"工具箱"菜单条
        )
        (setq i (1- i))
      )
      (if (< i 0)  ; 如果没有文件菜单, 取最后一条菜单菜单
        (setq i (vla-get-count MenuBar))
      )
    )
    (setq i (vla-get-count MenuBar)) ;_  取最后一条菜单菜单
  )
  ;;创建"CASS工具"菜单条
  (if 
    (not 
      (setq popupmenu (GXL-CATCHAPPLY 
                        'vla-Item
                        (list 
                          (vla-get-menus 
                            (vla-item 
                              (vla-get-MenuGroups (vlax-get-acad-object))
                              MenuGroupName ;_ "测量工具集" 菜单组名称
                            )
                          )
                          POPName ;_ "CASS工具" 下拉菜单名称
                        )
                      )
      )
    )
    (setq popupmenu (vla-add 
                      (vla-get-menus 
                        (vla-item (vla-get-MenuGroups (vlax-get-acad-object)) 
                                  MenuGroupName ;_ "测量工具集" 菜单组名称
                        )
                      )
                      POPName ;_ "CASS工具" 下拉菜单名称
                    )
    )
  )
  ;;清除Menu子项
  (vlax-for popupmenuitem popupmenu 
    (vla-delete popupmenuitem)
  )
  ;;插入"CASS工具"菜单条
  (vla-InsertInMenuBar popupmenu i)
  (gxl-insertPopMenuItems popupmenu PopItems)
  (princ)
)

;;函数 gxl-insertPopMenuItems 逐项插入菜单条
;;语法: (gxl-insertPopMenuItems popupmenu PopItems)
;;popupmenu 菜单条vla对象
;;PopItems 下拉菜单列表
;;   如 '((标签 命令 帮助字串 次级子项)...) 表为下拉菜单列表，注意命令后要有一个空格
(defun gxl-insertPopMenuItems (popupmenu PopItems / K TMP) 
  (setq k 0)
  ;;插入"CASS工具"菜单子项目
  (mapcar 
    (function 
      (lambda (x / Label cmdstr hlpstr subItems tmp) 
        (setq Label    (car x)
              cmdstr   (cadr x)
              hlpstr   (caddr x)
              subItems (cadddr x)
        )
        (if (= label "--") 
          ;; 插入分隔符
          (vla-AddSeparator 
            popupmenu
            (setq k (1+ k))
          )
          (if (and Label cmdstr) 
            ;; 插入菜单条
            (progn 
              (setq tmp (vla-addmenuitem 
                          popupmenu
                          (setq k (1+ k))
                          Label
                          cmdstr
                        )
              )
              (vla-put-helpstring tmp hlpstr)
            )
            ;; 插入下一级子菜单
            (progn 
              (setq tmp (vla-addsubmenu 
                          popupmenu
                          (setq k (1+ k))
                          Label
                        )
              )
              (if subItems  ;_ 添加子级菜单
                (gxl-insertPopMenuItems tmp subItems)
              )
            )
          )
        )
      )
    )
    ;;'((标签 命令 帮助字串 次级菜单项)) 表为菜单项，注意命令后要有一个空格
    PopItems
  )
)
;;函数 gxl-CatchApply 重定义 VL-CATCH-ALL-APPLY 
;;语法: (gxl-CatchApply fun args)
;;参数 fun 函数 如 distance or 'distance
;;     args 函数的参数表
;;返回值: 如函数运行错误返回nil,否则返回函数的返回值
(defun gxl-CatchApply (fun args / result) 
  (if 
    (not 
      (vl-catch-all-error-p 
        (setq result (vl-catch-all-apply 
                       (if (= 'SYM (type fun)) 
                         fun
                         (function fun)
                       )
                       args
                     )
        )
      )
    )
    result
  )
)
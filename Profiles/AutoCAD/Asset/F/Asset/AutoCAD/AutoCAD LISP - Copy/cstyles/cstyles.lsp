;;;------------------------------------------------------------------------------------------------
(defun c:cstyles (/		      dcl_id
		  FontStyleDXF	      FontStyleName
		  FontFile	      BigFontFile
		  diandao	      fanxiang
		  ChuiZhi	      Height
		  Width		      ObliqueAngle
		  kw			;记录控制关闭对话框的组件号
		  ExistFontStylesNameList ;已存在的文字样式名列表
		  ToBeChangedFontStylesNameList ;需修改的文字样式名列表
		  StylesExistIndex	;记录已存在文字样式列表中被选择的行号
		  StylesToBeChangedIndex;记录需修改文字样式列表中被选择的行号
		 )
;;;------------------------------------------------------------------------------------------------
;;;搜索已有文字样式并加入到DCL中相应的List
  (defun GetExistFontStyleNames	(/)
    (setq FontStyleDXF		  (tblnext "STYLE" T)
	  ExistFontStylesNameList nil
    )
    (while FontStyleDXF
      (setq FontStyleName (cdr (assoc 2 FontStyleDXF)))
      ;;往表中加入已有文字样式名列表
      (if (/= FontStyleName "")
	(setq ExistFontStylesNameList
	       (append ExistFontStylesNameList
		       (list FontStyleName)
	       )
	)
      )
      (setq FontStyleDXF (tblnext "STYLE"))
    )

    (NameListToListBox ExistFontStylesNameList "StylesExist")

    (setq FontStyleName nil)
  )
;;;------------------------------------------------------------------------------------------------
;;;查询当前文字样式的资料，并反应到与DCL相关联的变量中
  (defun GetFontStyleData (/ tmp)
    (if	FontStyleName
      (progn
	(setq FontStyleDXF (tblsearch "STYLE" FontStyleName))
	(setq FontFile (cdr (assoc 3 FontStyleDXF))) ;主要字体文件名
	(setq BigFontFile (cdr (assoc 4 FontStyleDXF))) ;大字体文件名

	(setq Height (cdr (assoc 40 FontStyleDXF))) ;固定的文字高度
	(setq Width (cdr (assoc 41 FontStyleDXF))) ;宽度因子
	(setq ObliqueAngle (cdr (assoc 50 FontStyleDXF))) ;文字倾斜角度

	(setq tmp (cdr (assoc 70 FontStyleDXF)))
	(if (= tmp 4)
	  (setq ChuiZhi "Y")
	  (setq ChuiZhi "N")
	)				;垂直文字

	(setq tmp (cdr (assoc 71 FontStyleDXF)))
	(cond
	  ((= tmp 2) (setq fanxiang "Y") (setq diandao "N"))
	  ((= tmp 4) (setq fanxiang "N") (setq diandao "Y"))
	  ((= tmp 6) (setq fanxiang "Y") (setq diandao "Y"))
	  (T (setq fanxiang "N") (setq diandao "N"))
	)
      )					;2 = 反向文字( 镜像 X)      4 = 倒置文字( 镜像 Y)

      (progn
	(setq FontFile "")		;主要字体文件名
	(setq BigFontFile "")		;大字体文件名
	(setq Height 0)			;固定的文字高度
	(setq Width 0.8)		;宽度因子
	(setq ObliqueAngle 0)		;文字倾斜角度

	(setq ChuiZhi "N")		;4 = 垂直文字
	(setq fanxiang "N")		;2 = 反向文字( 镜像 X)
	(setq diandao "N")		; 4 = 倒置文字( 镜像 Y)
      )
    )
    (setDCLTileFontStyleSet)
  )
;;;------------------------------------------------------------------------------------------------
;;;根据指定文字样式的信息设置DCL上相关组件值
  (defun setDCLTileFontStyleSet	()
    (if	FontFile
      (set_tile "DCLFontFile" FontFile)
      (set_tile "DCLFontFile" "")
    )
    (if	BigFontFile
      (set_tile "DCLBigFontFile" BigFontFile)
      (set_tile "DCLBigFontFile" "")
    )
    (if	Height
      (set_tile "DCLHeight" (rtos Height 2 3))
      (set_tile "DCLHeight" "0")
    )
    (if	Width
      (set_tile "DCLWidth" (rtos Width 2 3))
      (set_tile "DCLWidth" "1")
    )
    (if	ObliqueAngle
      (set_tile "DCLObliqueAngle" (rtos ObliqueAngle 2 3))
      (set_tile "DCLObliqueAngle" "0")
    )

    (if	(= ChuiZhi "N")
      (set_tile "DCLChuiZhi" "0")
      (set_tile "DCLChuiZhi" "1")
    )
    (if	(= fanxiang "N")
      (set_tile "DCLfanxiang" "0")
      (set_tile "DCLfanxiang" "1")
    )
    (if	(= diandao "N")
      (set_tile "DCLdiandao" "0")
      (set_tile "DCLdiandao" "1")
    )
  )
;;;------------------------------------------------------------------------------------------------
;;;获取DCL上文字样式相关组件的值
  (defun getDCLTileFontStyleSet	(/ tmp)
    (setq FontFile (get_tile "DCLFontFile"))
    (setq BigFontFile (get_tile "DCLBigFontFile"))
    (setq Height (atof (get_tile "DCLHeight")))
    (setq Width (atof (get_tile "DCLWidth")))
    (setq ObliqueAngle (atof (get_tile "DCLObliqueAngle")))

    (setq tmp (atoi (get_tile "DCLChuiZhi")))
    (if	(= tmp 0)
      (setq ChuiZhi "N")
      (setq ChuiZhi "Y")
    )
    (setq tmp (atoi (get_tile "DCLfanxiang")))
    (if	(= tmp 0)
      (setq fanxiang "N")
      (setq fanxiang "Y")
    )

    (setq tmp (atoi (get_tile "DCLdiandao")))
    (if	(= tmp 0)
      (setq diandao "N")
      (setq diandao "Y")
    )
  )
;;;------------------------------------------------------------------------------------------------
;;;设置默认的文字样式值
  (defun setDefaultFontStyleData ()
    (setq FontFile "hztxt_e.shx")	;主要字体文件名
    (setq BigFontFile "hztxt.shx")	;大字体文件名
    (setq Height 0)			;固定的文字高度
    (setq Width 0.8)			;宽度因子
    (setq ObliqueAngle 0)		;文字倾斜角度

    (setq ChuiZhi "N")			;4 = 垂直文字
    (setq fanxiang "N")			;2 = 反向文字( 镜像 X)
    (setq diandao "N")			; 4 = 倒置文字( 镜像 Y)

    (setDCLTileFontStyleSet)
  )
;;;------------------------------------------------------------------------------------------------
;;;设置默认的文字样式值
  (defun setSongTiFontStyleData	()
    (setq FontFile "SimSun.ttf")	;主要字体文件名
    (setq BigFontFile "")		;大字体文件名
    (setq Height 0)			;固定的文字高度
    (setq Width 0.75)			;宽度因子
    (setq ObliqueAngle 0)		;文字倾斜角度

    (setq ChuiZhi "N")			;4 = 垂直文字
    (setq fanxiang "N")			;2 = 反向文字( 镜像 X)
    (setq diandao "N")			; 4 = 倒置文字( 镜像 Y)

    (setDCLTileFontStyleSet)
  )
;;;------------------------------------------------------------------------------------------------
;;;根据设置，修改当前文字样式的设置
  (defun ChangeFontStyleSet (fsName / tmp fstName strFontfiles)
    (if	fsName
      (if (or (= (strcase (substr FontFile (- (strlen FontFile) 3) 4))
		 ".TTF"
	      )
	      (= BigFontFile "")
	  )
;;;    (progn ;对于标准字体，因未找到修改dxf组码的有效方式，暂采用command的方式修改，速度比较慢
	(progn
	  (cond
	    ((= (strcase FontFile) "SIMSUN.TTF")
	     (setq strFontfiles "宋体")
	    )
	    ((= (strcase FontFile) "NSIMSUN.TTF")
	     (setq strFontfiles "新宋体")
	    )
	    (T (setq strFontfiles FontFile))
	  )
	  (command "-style"	fsName	     strFontfiles Height
		   Width	ObliqueAngle fanxiang	  diandao
		  )
	)
	(progn
	  (setq strFontfiles (strcat FontFile "," BigFontFile))
	  (command "-style"	fsName	     strFontfiles Height
		   Width	ObliqueAngle fanxiang	  diandao
		   ""
		  )
	)
      )

;;;     (progn  ;对于非标准字体，采用修改dxf组码的方式修改，速度要快一些
;;;        (setq fstName (tblobjname "STYLE" fsName) )
;;;    (setq FontStyleDXF (entget fstName ))
;;;
;;;    (entmod (setq FontStyleDXF (subst (cons 3 FontFile) (assoc 3 FontStyleDXF) FontStyleDXF) ))     ;主要字体文件名
;;;    (entmod (setq FontStyleDXF (subst (cons 4 BigFontFile) (assoc 4 FontStyleDXF) FontStyleDXF ) ) )    ;大字体文件名
;;;    (entmod (setq FontStyleDXF (subst (cons 40 Height) (assoc 40 FontStyleDXF) FontStyleDXF ) ) )    ;固定的文字高度
;;;    (entmod (setq FontStyleDXF (subst (cons 41 Width) (assoc 41 FontStyleDXF) FontStyleDXF) ))     ;宽度因子
;;;    (entmod (setq FontStyleDXF (subst (cons 50 ObliqueAngle) (assoc 50 FontStyleDXF) FontStyleDXF ) ))     ;文字倾斜角度
;;;
;;;    (if (= ChuiZhi "Y")
;;;     (entmod (setq FontStyleDXF (subst (cons 70 4) (assoc 70 FontStyleDXF) FontStyleDXF) ))
;;;     (entmod (setq FontStyleDXF (subst (cons 70 0) (assoc 70 FontStyleDXF) FontStyleDXF) ))
;;;    )     ;垂直文字
;;;
;;;    (if (= fanxiang "Y")
;;;     (setq tmp 2)
;;;     (setq tmp 0)
;;;    )     ;2 = 反向文字( 镜像 X)
;;;
;;;    (if (= diandao "Y") (setq tmp (+ tmp 4)) )     ;4 = 倒置文字( 镜像 Y)
;;;    (entmod (setq FontStyleDXF (subst (cons 71 tmp) (assoc 71 FontStyleDXF) FontStyleDXF)))
;;;    (entupd fstName )
;;;      )
;;;   )
    )
  )
;;;------------------------------------------------------------------------------------------------
;;;根据设置，修改所有指定的文字样式
  (defun StylesToBeChangedChangeAll ()
    (mapcar 'ChangeFontStyleSet ToBeChangedFontStylesNameList)
  )
;;;------------------------------------------------------------------------------------------------
;;;新建文字样式
  (defun NewFontStyle (/ fsName fsDXF tmp)
    (setq fsName
	   (car	(InputBox "新建文字样式"
			  (list (list "文字样式的名称:" "新样式" "12"))
		)
	   )
    )

    (if	fsName
      (progn
	(setq fsDXF (tblsearch "STYLE" fsName))
	(if fsDXF
	  (alert "已经存在同名的文字样式!")

	  (progn
	    (getDCLTileFontStyleSet)
	    (setq fsDXF	(list '(0 . "STYLE")
			      '(100 . "AcDbSymbolTableRecord")
			      '(100 . "AcDbTextStyleTableRecord")
			      (cons 2 fsName)
			      (cons 3 FontFile)
			      (cons 4 BigFontFile)
			      (cons 40 Height)
			      (cons 41 Width)
			      (cons 50 ObliqueAngle)
			)
	    )

	    (if	(= ChuiZhi "Y")
	      (setq fsDXF (append fsDXF '((70 . 4))))
	      (setq fsDXF (append fsDXF '((70 . 0))))
	    )				;垂直文字

	    (if	(= fanxiang "Y")
	      (setq tmp 2)
	      (setq tmp 0)
	    )				;2 = 反向文字( 镜像 X)
	    (if	(= diandao "Y")
	      (setq tmp (+ tmp 4))
	    )				;4 = 倒置文字( 镜像 Y)
	    (setq fsDXF (append fsDXF (list (cons 71 tmp))))

	    (entmake fsDXF)
	    　
	    (setq ExistFontStylesNameList
		   (append ExistFontStylesNameList
			   (list fsName)
		   )
	    )
	    　
	    (NameListToListBox ExistFontStylesNameList "StylesExist")
	    (setq FontStyleName		 nil
		  StylesExistIndex	 -1
		  StylesToBeChangedIndex -1
	    )
	  )
	)
      )
    )
  )
;;;------------------------------------------------------------------------------------------------
;;;重命名文字样式
  (defun RenameFontStyle (/ fsOldName fsNewName fsDXF fstName)
    (if	(>= StylesExistIndex 0)
      (setq fsOldName (nth StylesExistIndex ExistFontStylesNameList))
    )

    (setq
      fsNewName	(car
		  (InputBox
		    "重命名文字样式"
		    (list (list "文字样式的新名称:" fsOldName "12"))
		  )
		)
    )

    (setq fstName (tblobjname "STYLE" fsOldName))
    (setq fsDXF (entget fstName))
    (entmod
      (setq fsDXF (subst (cons 2 fsNewName) (assoc 2 fsDXF) fsDXF))
    )
    (entupd fstName)

    (setq ExistFontStylesNameList
	   (subst fsNewName
		  fsOldName
		  ExistFontStylesNameList
	   )
    )

    (NameListToListBox ExistFontStylesNameList "StylesExist")
    (setq FontStyleName fsNewName)

    (if	(member fsOldName ToBeChangedFontStylesNameList)
      (progn
	(setq ToBeChangedFontStylesNameList
	       (subst
		 fsNewName
		 fsOldName
		 ToBeChangedFontStylesNameList
	       )
	)

	(NameListToListBox
	  ToBeChangedFontStylesNameList
	  "StylesToBeChanged"
	)
	(setq StylesToBeChangedIndex -1)
      )
    )					;如果重命名的文字样式存在于需要修改的样式列表中，则更新修改列表
  )
;;;------------------------------------------------------------------------------------------------
;;;把已有样式中选定的样式名添加到修改样式列表
  (defun StylesToBeChangedAddSelected (/ fsNameK)
    (if	(>= StylesExistIndex 0)
      (progn
	(setq fsNameK (nth StylesExistIndex ExistFontStylesNameList))
	(if (member fsNameK ToBeChangedFontStylesNameList)
	  (alert "所选择样式已在修改列表中!")

	  (progn
	    (setq ToBeChangedFontStylesNameList
		   (append
		     ToBeChangedFontStylesNameList
		     (list fsNameK)
		   )
	    )

	    (NameListToListBox
	      ToBeChangedFontStylesNameList
	      "StylesToBeChanged"
	    )
	    (setq StylesToBeChangedIndex -1)
	  )
	)
      )
    )
  )
;;;------------------------------------------------------------------------------------------------
;;;把已有样式中所有的样式名添加到修改样式列表
  (defun StylesToBeChangedAddAll ()
    (setq ToBeChangedFontStylesNameList ExistFontStylesNameList)
    　
    (NameListToListBox
      ToBeChangedFontStylesNameList
      "StylesToBeChanged"
    )
    (setq StylesToBeChangedIndex -1)
  )
;;;------------------------------------------------------------------------------------------------
;;;根据相应列表值，重新设置对话框中listbox显示值
;;;调用形式 (  listbox名称  对应列表 )
  (defun NameListToListBox (listName strListBoxName)
    (start_list strListBoxName 3 0)
    (mapcar 'add_list listName)
    (end_list)
  )
;;;------------------------------------------------------------------------------------------------
;;;从修改样式列表中删除选定顶
  (defun StylesToBeChangedRemoveOne (/ tempList i)
    (setq i 0
	  tempList nil
    )
    (repeat (length ToBeChangedFontStylesNameList)
      (if (/= i StylesToBeChangedIndex)
	(setq tempList
	       (append tempList
		       (list (nth i ToBeChangedFontStylesNameList))
	       )
	)
      )

      (setq i (1+ i))
    )

    (setq ToBeChangedFontStylesNameList tempList)

    (NameListToListBox
      ToBeChangedFontStylesNameList
      "StylesToBeChanged"
    )
    (setq FontStyleName		 nil
	  StylesToBeChangedIndex -1
    )
  )
;;;------------------------------------------------------------------------------------------------
;;;清空修改样式列表
  (defun StylesToBeChangedClear	()
    (setq ToBeChangedFontStylesNameList nil)
    (NameListToListBox
      ToBeChangedFontStylesNameList
      "StylesToBeChanged"
    )
    (setq FontStyleName		 nil
	  StylesToBeChangedIndex -1
    )
  )
;;;------------------------------------------------------------------------------------------------
;;;点选已存在样式列表时
  (defun StylesExistOnClick (keyValue)
    (setq StylesExistIndex (atoi keyValue))
    (setq FontStyleName (nth StylesExistIndex ExistFontStylesNameList))
    (GetFontStyleData)
  )
;;;------------------------------------------------------------------------------------------------
;;;点选需修改样式列表时
  (defun StylesToBeChangedOnClick (keyValue)
    (setq StylesToBeChangedIndex (atoi keyValue))
    (setq FontStyleName
	   (nth	StylesToBeChangedIndex
		ToBeChangedFontStylesNameList
	   )
    )
    (GetFontStyleData)
  )
;;;------------------------------------------------------------------------------------------------
;;;主函数内容，设置、调用DCL
  (if (setq dcl_id (load_dialog "cstyles.dcl"))
    (if	(new_dialog "cstyles" dcl_id)
      (progn
	(GetExistFontStyleNames)

	(setq FontStyleName	     nil
	      StylesExistIndex	     -1
	      StylesToBeChangedIndex -1
	)

	(action_tile "StylesExist" "(StylesExistOnClick $value)")
	(action_tile
	  "StylesToBeChanged"
	  "(StylesToBeChangedOnClick $value)"
	)
	(action_tile
	  "btAddSelected"
	  "(StylesToBeChangedAddSelected)"
	)
	(action_tile "btAddAll" "(StylesToBeChangedAddAll)")
	(action_tile "btStyleNew" "(NewFontStyle)")
	(action_tile "btStyleRename" "(RenameFontStyle)")
	(action_tile "btRemoveOne" "(StylesToBeChangedRemoveOne)")
	(action_tile "btRemoveAll" "(StylesToBeChangedClear)")
	(action_tile "btSetDefault" "(setDefaultFontStyleData)")
	(action_tile "btSetSongTi" "(setSongTiFontStyleData)")
	(action_tile
	  "accept"
	  "(getDCLTileFontStyleSet) (done_dialog 1)"
	)

	(setq kw (start_dialog))
	(unload_dialog dcl_id)

	(if (= kw 1)
	  (StylesToBeChangedChangeAll)
;;;      (if (or (/= (strcase (substr FontFile (- (strlen FontFile) 3) 4))
;;;               ".TTF"  )
;;;	      (/= BigFontFile "") )
;;;          (command "regen")
;;;	)
	)
      )
    )
  )
  (princ)
)
;;;------------------------------------------------------------------------------------------------
(defun c:cs()
 (c:cstyles)
)
(princ "文字样式管理器,启动命令\"cs\"")

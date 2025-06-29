;;;------------------------------------------------------------------------------------------------
(defun c:cstyles (/		      dcl_id
		  FontStyleDXF	      FontStyleName
		  FontFile	      BigFontFile
		  diandao	      fanxiang
		  ChuiZhi	      Height
		  Width		      ObliqueAngle
		  kw			;��¼���ƹرնԻ���������
		  ExistFontStylesNameList ;�Ѵ��ڵ�������ʽ���б�
		  ToBeChangedFontStylesNameList ;���޸ĵ�������ʽ���б�
		  StylesExistIndex	;��¼�Ѵ���������ʽ�б��б�ѡ����к�
		  StylesToBeChangedIndex;��¼���޸�������ʽ�б��б�ѡ����к�
		 )
;;;------------------------------------------------------------------------------------------------
;;;��������������ʽ�����뵽DCL����Ӧ��List
  (defun GetExistFontStyleNames	(/)
    (setq FontStyleDXF		  (tblnext "STYLE" T)
	  ExistFontStylesNameList nil
    )
    (while FontStyleDXF
      (setq FontStyleName (cdr (assoc 2 FontStyleDXF)))
      ;;�����м�������������ʽ���б�
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
;;;��ѯ��ǰ������ʽ�����ϣ�����Ӧ����DCL������ı�����
  (defun GetFontStyleData (/ tmp)
    (if	FontStyleName
      (progn
	(setq FontStyleDXF (tblsearch "STYLE" FontStyleName))
	(setq FontFile (cdr (assoc 3 FontStyleDXF))) ;��Ҫ�����ļ���
	(setq BigFontFile (cdr (assoc 4 FontStyleDXF))) ;�������ļ���

	(setq Height (cdr (assoc 40 FontStyleDXF))) ;�̶������ָ߶�
	(setq Width (cdr (assoc 41 FontStyleDXF))) ;�������
	(setq ObliqueAngle (cdr (assoc 50 FontStyleDXF))) ;������б�Ƕ�

	(setq tmp (cdr (assoc 70 FontStyleDXF)))
	(if (= tmp 4)
	  (setq ChuiZhi "Y")
	  (setq ChuiZhi "N")
	)				;��ֱ����

	(setq tmp (cdr (assoc 71 FontStyleDXF)))
	(cond
	  ((= tmp 2) (setq fanxiang "Y") (setq diandao "N"))
	  ((= tmp 4) (setq fanxiang "N") (setq diandao "Y"))
	  ((= tmp 6) (setq fanxiang "Y") (setq diandao "Y"))
	  (T (setq fanxiang "N") (setq diandao "N"))
	)
      )					;2 = ��������( ���� X)      4 = ��������( ���� Y)

      (progn
	(setq FontFile "")		;��Ҫ�����ļ���
	(setq BigFontFile "")		;�������ļ���
	(setq Height 0)			;�̶������ָ߶�
	(setq Width 0.8)		;�������
	(setq ObliqueAngle 0)		;������б�Ƕ�

	(setq ChuiZhi "N")		;4 = ��ֱ����
	(setq fanxiang "N")		;2 = ��������( ���� X)
	(setq diandao "N")		; 4 = ��������( ���� Y)
      )
    )
    (setDCLTileFontStyleSet)
  )
;;;------------------------------------------------------------------------------------------------
;;;����ָ��������ʽ����Ϣ����DCL��������ֵ
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
;;;��ȡDCL��������ʽ��������ֵ
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
;;;����Ĭ�ϵ�������ʽֵ
  (defun setDefaultFontStyleData ()
    (setq FontFile "hztxt_e.shx")	;��Ҫ�����ļ���
    (setq BigFontFile "hztxt.shx")	;�������ļ���
    (setq Height 0)			;�̶������ָ߶�
    (setq Width 0.8)			;�������
    (setq ObliqueAngle 0)		;������б�Ƕ�

    (setq ChuiZhi "N")			;4 = ��ֱ����
    (setq fanxiang "N")			;2 = ��������( ���� X)
    (setq diandao "N")			; 4 = ��������( ���� Y)

    (setDCLTileFontStyleSet)
  )
;;;------------------------------------------------------------------------------------------------
;;;����Ĭ�ϵ�������ʽֵ
  (defun setSongTiFontStyleData	()
    (setq FontFile "SimSun.ttf")	;��Ҫ�����ļ���
    (setq BigFontFile "")		;�������ļ���
    (setq Height 0)			;�̶������ָ߶�
    (setq Width 0.75)			;�������
    (setq ObliqueAngle 0)		;������б�Ƕ�

    (setq ChuiZhi "N")			;4 = ��ֱ����
    (setq fanxiang "N")			;2 = ��������( ���� X)
    (setq diandao "N")			; 4 = ��������( ���� Y)

    (setDCLTileFontStyleSet)
  )
;;;------------------------------------------------------------------------------------------------
;;;�������ã��޸ĵ�ǰ������ʽ������
  (defun ChangeFontStyleSet (fsName / tmp fstName strFontfiles)
    (if	fsName
      (if (or (= (strcase (substr FontFile (- (strlen FontFile) 3) 4))
		 ".TTF"
	      )
	      (= BigFontFile "")
	  )
;;;    (progn ;���ڱ�׼���壬��δ�ҵ��޸�dxf�������Ч��ʽ���ݲ���command�ķ�ʽ�޸ģ��ٶȱȽ���
	(progn
	  (cond
	    ((= (strcase FontFile) "SIMSUN.TTF")
	     (setq strFontfiles "����")
	    )
	    ((= (strcase FontFile) "NSIMSUN.TTF")
	     (setq strFontfiles "������")
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

;;;     (progn  ;���ڷǱ�׼���壬�����޸�dxf����ķ�ʽ�޸ģ��ٶ�Ҫ��һЩ
;;;        (setq fstName (tblobjname "STYLE" fsName) )
;;;    (setq FontStyleDXF (entget fstName ))
;;;
;;;    (entmod (setq FontStyleDXF (subst (cons 3 FontFile) (assoc 3 FontStyleDXF) FontStyleDXF) ))     ;��Ҫ�����ļ���
;;;    (entmod (setq FontStyleDXF (subst (cons 4 BigFontFile) (assoc 4 FontStyleDXF) FontStyleDXF ) ) )    ;�������ļ���
;;;    (entmod (setq FontStyleDXF (subst (cons 40 Height) (assoc 40 FontStyleDXF) FontStyleDXF ) ) )    ;�̶������ָ߶�
;;;    (entmod (setq FontStyleDXF (subst (cons 41 Width) (assoc 41 FontStyleDXF) FontStyleDXF) ))     ;�������
;;;    (entmod (setq FontStyleDXF (subst (cons 50 ObliqueAngle) (assoc 50 FontStyleDXF) FontStyleDXF ) ))     ;������б�Ƕ�
;;;
;;;    (if (= ChuiZhi "Y")
;;;     (entmod (setq FontStyleDXF (subst (cons 70 4) (assoc 70 FontStyleDXF) FontStyleDXF) ))
;;;     (entmod (setq FontStyleDXF (subst (cons 70 0) (assoc 70 FontStyleDXF) FontStyleDXF) ))
;;;    )     ;��ֱ����
;;;
;;;    (if (= fanxiang "Y")
;;;     (setq tmp 2)
;;;     (setq tmp 0)
;;;    )     ;2 = ��������( ���� X)
;;;
;;;    (if (= diandao "Y") (setq tmp (+ tmp 4)) )     ;4 = ��������( ���� Y)
;;;    (entmod (setq FontStyleDXF (subst (cons 71 tmp) (assoc 71 FontStyleDXF) FontStyleDXF)))
;;;    (entupd fstName )
;;;      )
;;;   )
    )
  )
;;;------------------------------------------------------------------------------------------------
;;;�������ã��޸�����ָ����������ʽ
  (defun StylesToBeChangedChangeAll ()
    (mapcar 'ChangeFontStyleSet ToBeChangedFontStylesNameList)
  )
;;;------------------------------------------------------------------------------------------------
;;;�½�������ʽ
  (defun NewFontStyle (/ fsName fsDXF tmp)
    (setq fsName
	   (car	(InputBox "�½�������ʽ"
			  (list (list "������ʽ������:" "����ʽ" "12"))
		)
	   )
    )

    (if	fsName
      (progn
	(setq fsDXF (tblsearch "STYLE" fsName))
	(if fsDXF
	  (alert "�Ѿ�����ͬ����������ʽ!")

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
	    )				;��ֱ����

	    (if	(= fanxiang "Y")
	      (setq tmp 2)
	      (setq tmp 0)
	    )				;2 = ��������( ���� X)
	    (if	(= diandao "Y")
	      (setq tmp (+ tmp 4))
	    )				;4 = ��������( ���� Y)
	    (setq fsDXF (append fsDXF (list (cons 71 tmp))))

	    (entmake fsDXF)
	    ��
	    (setq ExistFontStylesNameList
		   (append ExistFontStylesNameList
			   (list fsName)
		   )
	    )
	    ��
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
;;;������������ʽ
  (defun RenameFontStyle (/ fsOldName fsNewName fsDXF fstName)
    (if	(>= StylesExistIndex 0)
      (setq fsOldName (nth StylesExistIndex ExistFontStylesNameList))
    )

    (setq
      fsNewName	(car
		  (InputBox
		    "������������ʽ"
		    (list (list "������ʽ��������:" fsOldName "12"))
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
    )					;�����������������ʽ��������Ҫ�޸ĵ���ʽ�б��У�������޸��б�
  )
;;;------------------------------------------------------------------------------------------------
;;;��������ʽ��ѡ������ʽ����ӵ��޸���ʽ�б�
  (defun StylesToBeChangedAddSelected (/ fsNameK)
    (if	(>= StylesExistIndex 0)
      (progn
	(setq fsNameK (nth StylesExistIndex ExistFontStylesNameList))
	(if (member fsNameK ToBeChangedFontStylesNameList)
	  (alert "��ѡ����ʽ�����޸��б���!")

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
;;;��������ʽ�����е���ʽ����ӵ��޸���ʽ�б�
  (defun StylesToBeChangedAddAll ()
    (setq ToBeChangedFontStylesNameList ExistFontStylesNameList)
    ��
    (NameListToListBox
      ToBeChangedFontStylesNameList
      "StylesToBeChanged"
    )
    (setq StylesToBeChangedIndex -1)
  )
;;;------------------------------------------------------------------------------------------------
;;;������Ӧ�б�ֵ���������öԻ�����listbox��ʾֵ
;;;������ʽ (  listbox����  ��Ӧ�б� )
  (defun NameListToListBox (listName strListBoxName)
    (start_list strListBoxName 3 0)
    (mapcar 'add_list listName)
    (end_list)
  )
;;;------------------------------------------------------------------------------------------------
;;;���޸���ʽ�б���ɾ��ѡ����
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
;;;����޸���ʽ�б�
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
;;;��ѡ�Ѵ�����ʽ�б�ʱ
  (defun StylesExistOnClick (keyValue)
    (setq StylesExistIndex (atoi keyValue))
    (setq FontStyleName (nth StylesExistIndex ExistFontStylesNameList))
    (GetFontStyleData)
  )
;;;------------------------------------------------------------------------------------------------
;;;��ѡ���޸���ʽ�б�ʱ
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
;;;���������ݣ����á�����DCL
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
(princ "������ʽ������,��������\"cs\"")

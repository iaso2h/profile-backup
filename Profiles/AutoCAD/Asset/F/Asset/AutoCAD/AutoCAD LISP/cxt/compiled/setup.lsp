(defun c:setup () 
  (c:setupSysVar)
  (c:setupFont)
  (c:setupLayer)

  (princ)
)

(defun c:setupSysVar () 
  (setvar "CMDECHO" 1)
  (setvar "CMDDIA" 1)
  (setvar "FILEDIA" 1)
  (setvar "HIGHLIGHT" 1)
  ; (setvar "MIRRTEXT" 0)
  (setvar "PICKADD" 2)
  (setvar "PICKAUTO" 7)
  (setvar "PICKFIRST" 1)
  (setvar "SELECTIONPREVIEW" 3)
  (setvar "SELECTSIMILARMODE" 192)
  (setvar "UCSFOLLOW" 0)
  (princ)
)

(defun c:setupFont (/ doc textStyles standardStyle) 
  (vl-load-com)

  (if (not (tblsearch "STYLE" "斜仿宋")) 
    (entmake 
      (list '(0 . "STYLE") 
            '(100 . "AcDbSymbolTableRecord")
            '(100 . "AcDbTextStyleTableRecord")
            '(2 . "斜仿宋")
            '(70 . 0)
            '(40 . 0) ; Fixed text height; 0 if not fixed
            '(41 . 0.8) ; Width factor
            (cons 50 (* pi (/ 8.0 180))) ; Optional, Oblique angle in radians
            '(3 . "tssdeng.shx") ; Primary font file name
            '(4 . "tssdchn.shx") ; Bigfont file name; blank if none
      )
    )
  )
  (if (not (tblsearch "STYLE" "黑体")) 
    (entmake 
      (list '(0 . "STYLE") 
            '(100 . "AcDbSymbolTableRecord")
            '(100 . "AcDbTextStyleTableRecord")
            '(2 . "黑体")
            '(70 . 0)
            '(40 . 0) ; Fixed text height; 0 if not fixed
            '(3 . "simhei.ttf") ; Primary font file name
      )
    )
  )


  ;; Fix standard font oblique angle and set it to 0. Eventually, I implement it in the ActiveX way.

  ; ERROR: Cannot retrie dot list for DXF group code 50
  ; Reset standrad italic
  ; (setq standardFontEntData (tblsearch "STYLE" "Standard"))
  ; (if
  ;   (and
  ;     (cadr (assoc 50 standardFontEntData))
  ;     (/= (cadr (assoc 50 standardFontEntData)) 0)
  ;     (or (/= (strcase (cadr (assoc 3 standardFontEntData)) T) "tssdeng.shx")
  ;         (/= (strcase (cadr (assoc 4 standardFontEntData)) T) "tssdchn.shx")
  ;     )
  ;   )
  ;   (subst '(50 . 0) (assoc 50 standardFontEntData) standardFontEntData)
  ;   (entmod standardFontEntData)
  ; )

  ; ActiveX way
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq textStyles (vla-get-textstyles doc))
  (setq standardStyle (vl-catch-all-apply 'vla-item (list textStyles "Standard")))
  (if 
    (and 
      (/= vla-get-obliqueangle 0)
      (or (/= (strcase (vla-get-fontfile standardStyle) T) "tssdeng.shx") 
          (/= (strcase (vla-get-bigfontfile standardStyle) T) "tssdchn.shx")
      )
    )
    (vla-put-obliqueangle standardStyle 0)
  )
  (princ)
)


(defun c:setupLayer (/ layerInfo name lineweight printable color description) 
  (defun *error* (msg) 
    (if (not (member msg '("Function cancelled" "quit / exit abort" "函数已取消"))) 
      (progn 
        (princ (strcat "Error: " msg "\n"))
        (princ)
      )
    )
  )
  (princ "\n")
  (vl-load-com)

  (setvar "CMDECHO" 0)
  (setvar "NOMUTT" 1)
  (command "undo" "be")

  (setq layerInfo '(("xline" "default" "n" "41" "辅助线图层，不可打印！")
                    ("中心线" "0.09" "p" "1" "")
                    ("标注" "0.13" "p" "84" "")
                    ("dim" "0.13" "p" "84" "")
                    ("符号" "0.13" "p" "3" "")
                    ("几何体" "0.18" "p" "7" "")

                    ("菲林" "default" "p" "21" "")
                    ("发热丝" "default" "p" "3" "")
                    ("参照" "0.09" "p" "6" "")
                    ("填充" "0.09" "p" "251" "")
                    ("割孔" "default" "p" "172" "")
                    ("发热分区" "default" "p" "20" "")
                   )
  )
  (foreach layerList layerInfo 
    (setq name (nth 0 layerList))
    (setq lineweight (nth 1 layerList))
    (setq printable (nth 2 layerList))
    (setq color (nth 3 layerList))
    (setq description (nth 4 layerList))

    (if (tblsearch "LAYER" name) 
      (command ".-layer" "lw" lineweight name "p" printable name "c" color name "")
      (command ".-layer" "n" name "lw" lineweight name "p" printable name "c" color 
               name "d" description name ""
      )
    )
  )
  (command "undo" "e")
  (setvar "CMDECHO" 1)
  (setvar "NOMUTT" 0)
  (princ)
)

(setq *IsLoadedSetup* T)
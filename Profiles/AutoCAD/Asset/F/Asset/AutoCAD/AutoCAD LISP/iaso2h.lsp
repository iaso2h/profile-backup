; The current workspace folder must add to the suport file search path
; PERFORMANCE: https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/check-add-support-file-search-path-via-lisp/td-p/1452765
; TODO: https://www.cnblogs.com/Higurashi-kagome/p/15366580.html
; TODO: http://bbs.mjtd.com/thread-178359-3-1.html
; utf-8,ucs-bom,gb18030,gbk,gb2312,cp936
; https://utf8.supfree.net
(princ "\n")
(setq *searchIncluded* T)

(if (eq (substr (getvar "cprofile") 1 7) "TArch20") 
  (progn 
    (if (eq (load "aliasTangent.lsp" nil) nil) 
      (progn 
        (princ "iaso2h: 无法找到天正T20缩写命令文件\n")
        (princ "iaso2h: 自定义.lsp文件不在搜索路径上\n")
        (setq *searchIncluded* nil)
      )
    )
    (setq *tchLoaded* t)
  )
  (progn 

    (setq *tchLoaded* nil)
  )
)
; 
(if (wcmatch (getvar "PRODUCT") "AutoCAD*") 
  (progn 
    (if (eq (load "layerDirector.lsp" nil) nil) 
      (progn 
        (princ "iaso2h: 无法找到图层定向文件\n")
        (princ "iaso2h: 自定义.lsp文件不在搜索路径上\n")
        (setq *searchIncluded* nil)
      )
    )

    (setq *autoCADLoaded* T)
  )
  (progn 
    ; (setvar "FONTALT" "C:\\Program Files\\ZWSOFT\\ZWCAD 2024\\fonts\\HZTXT.SHX")
    (setq *autoCADLoaded* nil)
  )
)

;; General Alias
(defun c:` () (command "._pline") (princ))
(defun c:a () (command "._matchprop") (princ))
(defun c:aa () (command "._arc" "c") (princ))
(defun c:a3 () (command "._arc" pause "e" pause "d") (princ))
(defun c:c2 () (command "._circle" "2p") (princ))
(defun c:c2t () (command "._circle" "2p" "tan" pause "tan") (princ))
(defun c:c3 () (command "._circle" "3p") (princ))
(defun c:c3t () (command "._circle" "3p" "tan" pause "tan" pause "tan") (princ))
(defun c:q () (command "._layoff") (princ))
(defun c:qa () (command "._layon") (princ))
(defun c:fr () (command "._layfrz" "c") (princ))
(defun c:ta () (command "._laythw" "c") (princ))
(defun c:fx () (command "._laylck" "c") (princ))
(defun c:df () (command "._layulk" "c") (princ))
(defun c:bl () (command "._setbylayer") (princ))
(defun c:ch () (command "._chamfer") (princ))
(defun c:dwg () (command "._dwg-purge") (princ))
(defun c:loo () (command "._layerp") (princ))
(defun c:lm () (command "._laymch") (princ))
(defun c:cc () (command "._laymcur") (princ))
(defun c:ca () (command "._copym") (princ))
(defun c:r () (command "._rotate") (princ))
(defun c:re () (command "._rectang") (princ))
(defun c:j () (command "._join") (princ))
(defun c:jl () (command "._joinl") (princ))
(defun c:mc () (command "._polygon" "4" pause "c") (princ))
(defun c:reg () (command "._regenall") (princ))
(defun c:rg () (command "._regen") (princ))
(if (not *autoCADLoaded*)
  (defun c:tre () (command "._trim" "_o" "_ex" pause) (princ))
)
(defun c:w () (command "._move") (princ))
(defun c:wt () (command "._syswindows" "V") (princ))
(defun c:wtv () (command "._syswindows" "V") (princ))
(defun c:wth () (command "._syswindows" "H") (princ))
(defun c:f () (command "._fillet" "u") (princ))
(defun c:ff () (command "._fillet" "R" "0") (command "._fillet" "u") (princ))

(defun c:t () (command "._syswindows" "H") (princ))

; Viewport
(if *autoCADLoaded* 
  (progn 
    (defun c:wv () (ai_tiledvp 2 "_V") (princ))
    (defun c:wvv () (ai_tiledvp 2 "_V") (princ))
    (defun c:wvh () (ai_tiledvp 2 "_H") (princ))
  )
  (progn 
    (defun c:wv () (command "_-VPORTS" "_2" "_V") (princ))
    (defun c:wvv () (command "_-VPORTS" "_2" "_V") (princ))
    (defun c:wvh () (command "_-VPORTS" "_2" "_H") (princ))
  )
)

(defun c:sv () (command "_-VPORTS" "SI") (princ))
(defun c:xx () (vl-cmdf "._burst") (princ))
(princ "iaso2h: 通用命令缩写加载完毕.\n")

; ----------------------------------------------

(if *searchIncluded* 
  (defun c:xl (/ savedEntLast) 
    (setq savedEntLast (entlast))
    (command "._xline")
    (while (= 1 (getvar "cmdactive")) 
      (command pause)
    )

    (load "util.lsp")
    (iaso2h:layerSetXline savedEntLast)
    (princ)
  )
)

(if *searchIncluded* 
  (progn 
    (load "autoload.lsp")
    (load "util.lsp")
    (load "layerCloseSelected.lsp")
    (load "layerCloseOthers.lsp")
    (load "layerFreezeSelected.lsp")
    (load "layerFreezeOthers.lsp")
    (defun c:q () (c:layerCloseSelected))
    (defun c:qe () (c:layerCloseOthers))
    (defun c:fr () (c:layerFreezeSelected))
    (defun c:fe () (c:layerFreezeOthers))
    (princ "iaso2h: 插件命令加载完毕.\n")
  )
  (princ "iaso2h: 搜索路径没有设置正确.\n")
)
(princ)

  ;; vim:set fileenconding=utf-8

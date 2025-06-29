; The current workspace folder must add to the suport file search path
; PERFORMANCE: https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/check-add-support-file-search-path-via-lisp/td-p/1452765
; TODO: https://www.cnblogs.com/Higurashi-kagome/p/15366580.html
; TODO: http://bbs.mjtd.com/thread-178359-3-1.html
; utf-8,ucs-bom,gb18030,gbk,gb2312,cp936
; https://utf8.supfree.net
(princ "\n")
(setq cur_dir (getvar "dwgprefix"))
(princ (strcat "CWD: " cur_dir "\n"))
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
    (if (eq (load "layerDirector.lsp" nil) nil) 
      (progn 
        (princ "iaso2h: 无法找到图层定向文件\n")
        (princ "iaso2h: 自定义.lsp文件不在搜索路径上\n") 
        (setq *searchIncluded* nil)
      )
    )
    (setq *tchLoaded* nil)
  )
)


;; General Alias
(defun c:a () (command "._matchprop") (princ))
(defun c:ae () (command "._arc" "c") (princ))
(defun c:bl () (command "._setbylayer") (princ))
(defun c:ch () (command "._chamfer") (princ))
(defun c:dwg () (command "._dwg-purge") (princ))
(defun c:loo () (command "._layerp") (princ))
(defun c:lm () (command "._laymch") (princ))
(defun c:r () (command "._rotate") (princ))
(defun c:re () (command "._rectang") (princ))
(defun c:rg () (command "._regen") (princ))
(defun c:set () (command "._fastsel") (princ))
(defun c:w () (command "._move") (princ))
(defun c:wt () (command "._syswindows" "V") (princ))
(defun c:wtv () (command "._syswindows" "V") (princ))
(defun c:wth () (command "._syswindows" "H") (princ))
(defun c:f () (command "._fillet" "u") (princ))
(defun c:ff () (command "._fillet" "R" "0") (command "._fillet" "u") (princ))
(defun c:wv () (ai_tiledvp 2 "_V") (princ))
(defun c:wvv () (ai_tiledvp 2 "_V") (princ))
(defun c:wvh () (ai_tiledvp 2 "_H") (princ))
(defun c:sv () (ai_tiledvp 1 nil) (princ))
(defun c:xx () (command "._burst") (princ))
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

(princ "iaso2h: 通用命令缩写加载完毕\n")


(autoload "dimTangentAdapt" '("dimTangentToggle" "dimTangentAdapt"))

(if *searchIncluded* 
  (load "autoload.lsp")
  (load "util.lsp")
)

(princ)

;; vim:set fileenconding=utf-8
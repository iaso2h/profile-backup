;; Change Color
(defun pp (obj) (princ (vl-prin1-to-string obj)) (terpri))

(defun c:mapColor () (colorAliasSetup))
(defun colorAliasSetup (/ i) 
  (defun colorAliasHelper (color / ss savedEcho) 
    (if (= color 0) (setq color "BYLAYER"))
    (if (= color "00") (setq color "BYBLOCK"))

    (if (setq ss (ssget "I")) 
      (progn 
        (setq savedEcho (getvar "CMDECHO"))
        (setvar "CMDECHO" 0)
        (if *AutoCADLoaded* 
          (command "._change" "_P" "p" "c" color "")
          (command "._change" ss "" "p" "c" color "")
        )
        (setvar "CMDECHO" savedEcho)
      )
      (progn 
        (if (/= (type color) 'STR) 
          (setq color (itoa color))
        )

        (setvar "CECOLOR" color)
      )
    )

    (princ)
  )
  (setq i 0)
  (while (<= i 255) 
    (eval 
      (read 
        (strcat "(defun c:" 
                (itoa i)
                (chr 40)
                (chr 41)
                "(colorAliasHelper "
                (itoa i)
                "))"
        )
      )
    )
    (setq i (1+ i))
  )
  (princ)
)
(colorAliasSetup)
(defun c:00 () (colorAliasHelper "00") (princ))


(defun iaso2h:layerSetXline (savedEntLast / tmp vlaObj) 
  (vl-load-com)
  (setq cmd (getvar 'cmdecho))

  (if (not (tblsearch "layer" "xline")) 
    (command "-layer" "n" "xline" "p" "n" "xline" "d" "¸¨ÖúÍ¼²ã£¬²»¿É´òÓ¡!" "xline" 
             "c" "41" "xline" ""
    )
  )
  ;;   (if
  ;;     (and (null savedEntLast)
  ;;          (setq savedEntLast (entlast))
  ;;     )
  ;;     (progn
  ;;       (setq vlaObj (vlax-ename->vla-object savedEntLast))
  ;;       (vla-put-color vlaObj 256)
  ;;       (vlax-put-property vlaObj 'Layer "xline")
  ;;     )
  ;;   )
  (if savedEntLast 
    (progn 
      (while (setq tmp (entnext savedEntLast)) 
        (setq savedEntLast tmp)
        (setq vlaObj (vlax-ename->vla-object savedEntLast))
        (vla-put-color vlaObj 256)
        (vlax-put-property vlaObj 'Layer "xline")
      )
    )
  )


  (princ)
)
(defun iaso2h:entlastTillNow (ent / ss) 
  ; Return all entities after ent
  (if (not ent) 
    (setq ent (entlast))
  )
  (setq ss (ssadd))
  (while (setq ent (entnext ent)) (ssadd ent ss))
  (if (zerop (sslength ss)) (setq ss nil))

  ss
)

(defun iaso2h:decimalTruncate (num decimalPlaces / multiplier) 
  (setq multiplier (expt 10.0 decimalPlaces))
  (/ (float (fix (* num multiplier))) multiplier)
)

(defun iaso2h:d2r (dregrees) 
  (* degrees (/ pi 180.0))
)

(defun iaso2h:r2d (radians) 
  (* radians (/ 180.0 pi))
)

(defun iaso2h:biggerEven (value / intPart) 
  "Returns the next even number that is bigger than the given value"
  (setq intPart (fix value))
  (cond 
    ((and (= (rem intPart 2) 0) (= intPart value))
     ;; Even integer exact match
     intPart
    )
    ((= (rem intPart 2) 0)
     ;; Even integer but not exact match
     (+ intPart 2)
    )
    (T
     ;; Odd integer, next even
     (+ intPart 1)
    )
  )
)

(defun iaso2h:biggerOdd (value / intPart) 
  "Returns the next odd number that is bigger than the given value"
  (setq intPart (fix value))
  (setq intPart (fix value))
  (cond 
    ((and (= (rem intPart 2) 0) (= intPart value))
     ;; Even integer exact match
     intPart
    )
    ((= (rem intPart 2) 0)
     ;; Even integer, next even
     (+ intPart 1)
    )
    (T
     ;; Odd integer, but not exact match
     (+ intPart 2)
    )
  )
)

  ;;-------------------=={ UnFormat String }==------------------;;
  ;;                                                            ;;
  ;;  Returns a string with all MText formatting codes removed. ;;
  ;;------------------------------------------------------------;;
  ;;  Author: Lee Mac, Copyright ?0?8 2011 - www.lee-mac.com       ;;
  ;;------------------------------------------------------------;;
  ;;  Arguments:                                                ;;
  ;;  str - String to Process                                   ;;
  ;;  mtx - MText Flag (T if string is for use in MText)        ;;
  ;;------------------------------------------------------------;;
  ;;  Returns:  String with formatting codes removed or nil     ;;
  ;;------------------------------------------------------------;;
(defun LM:UnFormat (str mtx / strSaved _replace rx) 
  (defun _replace (new old str) 
    (vlax-put-property rx 'pattern old)
    (vlax-invoke rx 'replace str new)
  )
  (if (setq rx (vlax-get-or-create-object "VBScript.RegExp")) 
    (progn 
      (setq str (vl-catch-all-apply 
                  (function 
                    (lambda () 
                      (vlax-put-property rx 'global actrue)
                      (vlax-put-property rx 'multiline actrue)
                      (vlax-put-property rx 'ignorecase acfalse)
                      (foreach pair 
                        '(("\032" . "\\\\\\\\")
                          (" " . "\\\\P|\\n|\\t")
                          ("$1" . "\\\\(\\\\[ACcFfHLlOopQTW])|\\\\[ACcFfHLlOopQTW][^\\\\;]*;|\\\\[ACcFfHLlOopQTW]")
                          ("$1$2/$3" . "([^\\\\])\\\\S([^;]*)[/#\\^]([^;]*);")
                          ("$1$2" . "\\\\(\\\\S)|[\\\\](})|}")
                          ("$1" . "[\\\\]({)|{")
                         )
                        (setq str (_replace (car pair) (cdr pair) str))
                      )
                      (if mtx 
                        (_replace 
                          "\\\\"
                          "\032"
                          (_replace 
                            "\\$1$2$3"
                            "(\\\\[ACcFfHLlOoPpQSTW])|({)|(})"
                            str
                          )
                        )
                        (_replace "\\" "\032" str)
                      )
                    )
                  )
                )
      )
      (vlax-release-object rx)
      (if (null (vl-catch-all-error-p str)) 
        str
      )
    )
  )
)

;; Unique  -  Lee Mac
;; Returns a list with duplicate elements removed.

(defun LM:Unique (l / x r) 
  (while l 
    (setq x (car l)
          l (vl-remove x (cdr l))
          r (cons x r)
    )
  )
  (reverse r)
)


(setq *IsLoadedUtil* T)
(princ)

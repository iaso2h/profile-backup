
; Credit: https://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/dynamically-lengthen-polyline/td-p/11713003
(defun C:plineLengthen (/ sel ent edata etype pt gw ss)  ; = Lengthen DYnamic
  (vl-load-com)
  (setq sel   (entsel "\nSelect Line/Arc/Polyline to lengthen: ")
        ent   (car sel)
        edata (entget ent)
        etype (cdr (assoc 0 edata))
        pt    (vlax-curve-getClosestPointTo ent (cadr sel)) ; for wide Polyline
  ) ; setq
  (if 
    (and 
      (wcmatch etype "LINE,ARC,ELLIPSE,*POLYLINE")
      (not (member '(100 . "AcDb3dPolyline") edata)) ; not 3D
      (not (vlax-curve-isClosed ent)) ; for Polyline/Ellipse [Line/Arc always]
    ) ; and
    (if (wcmatch etype "LINE,ARC,ELLIPSE")  ; outer then
      (command "_.lengthen" "_dynamic" pt pause "") ; inner then
      (progn  ; inner else [= *POLYLINE]
             (setq gw (assoc 43 edata)) ; = global width if it has one [nil if varying]
             (command "_.explode" ent)
             (setq ss (ssget "_P")) ; the pieces
             (command "_.lengthen" "_dynamic" pt pause "" ; on resulting Line/Arc
                      "_.pedit" "_last" "_join" ss "" ""
             ) ; command
             (if gw (entmod (append (entget (entlast)) (list gw)))) ; re-impose
      ) ; progn
    ) ; if [object type]
    (prompt "\nNot a qualifying object.") ; else
  ) ; if [closed]
  (prin1)
)
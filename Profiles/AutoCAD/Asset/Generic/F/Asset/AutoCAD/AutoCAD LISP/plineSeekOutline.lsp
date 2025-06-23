;; Outline Objects  -  Lee Mac
;; Attempts to generate a polyline outlining the selected objects.
;; sel - [sel] Selection Set to outline
;; Returns: [sel] A selection set of all objects created

(defun c:seekOutline  (/ *error* sel)
  (defun	*error*	 (msg)
    (LM:endundo (LM:acdoc))
    (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
      (princ (strcat "\nError: " msg)))
    (princ))

  (if (= *tchLoaded* 1)
    (command "._TSeOutline")
    (if (setq sel (ssget))
      (progn (LM:startundo (LM:acdoc))
             (LM:outline sel)
             (LM:endundo (LM:acdoc)))
      )
  )
  (princ)
)

(defun LM:outline  (sel / app are box cmd dis enl ent lst obj rtn tmp)
 (if (setq box (LM:ssboundingbox sel))
(progn (setq app (vlax-get-acad-object)
	       dis (/ (apply 'distance box) 20.0)
	       lst (mapcar '(lambda (a o) (mapcar o a (list dis dis))) box '(- +))
	       are (apply '* (apply 'mapcar (cons '- (reverse lst))))
	       dis (* dis 1.5)
	       ent (entmakex
		    (append '((000 . "LWPOLYLINE")
			      (100 . "AcDbEntity")
			      (100 . "AcDbPolyline")
			      (090 . 4)
			      (070 . 1))
			    (mapcar '(lambda (x) (cons 10 (mapcar '(lambda (y) ((eval y) lst)) x)))
				    '((caar cadar) (caadr cadar) (caadr cadadr) (caar cadadr))))))
	 (apply	'vlax-invoke
		(vl-list* app
			  'zoomwindow
			  (mapcar '(lambda (a o) (mapcar o a (list dis dis 0.0))) box '(- +))))
	 (setq cmd (getvar 'cmdecho)
	       enl (entlast)
	       rtn (ssadd))
	 (while (setq tmp (entnext enl)) (setq enl tmp))
	 (setvar 'cmdecho 0)
	 (command "_.-boundary"
		  "_a"
		  "_b"
		  "_n"
		  sel
		  ent
		  ""
		  "_i"
		  "_y"
		  "_o"
		  "_p"
		  ""
		  "_non"
		  (trans (mapcar '- (car box) (list (/ dis 3.0) (/ dis 3.0))) 0 1)
		  "")
	 (while (< 0 (getvar 'cmdactive)) (command ""))
	 (entdel ent)
	 (while	(setq enl (entnext enl))
	  (if (and (vlax-property-available-p
		    (setq obj (vlax-ename->vla-object enl))
		    'area)
		   (equal (vla-get-area obj) are 1e-4))
	   (entdel enl)
	   (ssadd enl rtn)))
	 (vla-zoomprevious app)
	 (setvar 'cmdecho cmd)
	 rtn)))

;; Selection Set Bounding Box  -  Lee Mac
;; Returns a list of the lower-left and upper-right WCS coordinates of a
;; rectangular frame bounding all objects in a supplied selection set.
;; s - [sel] Selection set for which to return bounding box

(defun LM:ssboundingbox	 (s / a b i m n o)
 (repeat (setq i (sslength s))
(if (and (setq o (vlax-ename->vla-object (ssname s (setq i (1- i)))))
	   (vlax-method-applicable-p o 'getboundingbox)
	   (not	(vl-catch-all-error-p
		 (vl-catch-all-apply 'vla-getboundingbox (list o 'a 'b)))))
 (setq m (cons (vlax-safearray->list a) m)
	 n (cons (vlax-safearray->list b) n))))
 (if (and m n)
(mapcar '(lambda (a b) (apply 'mapcar (cons a b)))
	  '(min max)
	  (list m n))))

;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo (doc) (LM:endundo doc) (vla-startundomark doc))

;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo  (doc)
 (while (= 8 (logand 8 (getvar 'undoctl))) (vla-endundomark doc)))

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc	 nil
 (eval (list 'defun
	     'LM:acdoc
	     'nil
	     (vla-get-activedocument (vlax-get-acad-object))))
 (LM:acdoc))

(vl-load-com)
(princ)




;;By Juan Villarreal
;;This routine will recreate hatches into a single hatch as if "create separate hatches" was not selected.
;;
;;Known Problems and Solutions:
;;1. Associativity can be lost.
;; - To retain the associative parameter, all selected objects must be associative.
;; - Non-Associative Hatch Objects do not contain the entity name of the object used during hatch creation within their DXF group codes.
;;
;;2. The routine may not function if joining a hatch object created via "select object" with that created via "select point".
;; - If joining hatch objects created using both methods, select the hatch object created via "select point" as the "Hatch Pattern to MATCH".

;;---------------------------------------------------------------------------
(defun massoc  (key alst /)
 (vl-remove-if '(lambda (pair) (/= (car pair) key)) alst))
;;---------------------------------------------------------------------------
(defun c:MergeHatch  (/	hentinfo ss i ent ent# seedpt# entinfo entinfo2	seedpts	MergedHatchList	Turnoff	BGColor)
 (vl-load-com)
 (while	(/= (cdr (assoc 0 hentinfo)) "HATCH")
(setq hentinfo (car (entsel "\nSelect Hatch Pattern to MATCH:")))
(If hentinfo
 (setq BGColor  (vla-get-backgroundcolor (vlax-ename->vla-object hentinfo))
	 hentinfo (entget hentinfo))
 (princ "\nMissed. Try again.")))
 (while	(not ss)
(princ "\nSelect ALL hatch entities to merge:")
(setq ss (ssget '((0 . "HATCH")))))
 (setq MergedHatchList (list (cons 0 "HATCH") (cons 100 "AcDbEntity") (assoc 8 hentinfo)))
 (if (assoc 62 hentinfo)
(setq MergedHatchList (append MergedHatchList (list (assoc 62 hentinfo)))))
 (setq MergedHatchList (append MergedHatchList
			       (list (cons 100 "AcDbHatch")
				     (assoc 10 hentinfo)
				     (assoc 210 hentinfo)
				     (assoc 2 hentinfo)
				     (assoc 70 hentinfo)
				     (assoc 71 hentinfo)
				     (cons 91 (sslength ss))))
     i	       -1
     seedpt#	       0
     ent#	       0)
 (setvar 'cmdecho 0)
 (repeat (sslength ss)
(setq	entinfo		(entget (ssname ss (setq i (1+ i))))
	entinfo2	(member (assoc 92 entinfo) entinfo)
	entinfo2	(reverse (cdr (member (assoc 75 entinfo2) (reverse entinfo2))))
	ent#		(+ ent# (cdr (assoc 91 entinfo)))
	seedpt#		(+ seedpt# (cdr (assoc 98 entinfo)))
	seedpts		(append seedpts (massoc 10 (member (assoc 98 entinfo) entinfo)))
	MergedHatchList	(append MergedHatchList entinfo2))
(if (zerop (cdr (assoc 71 entinfo)))
 (setq turnoff T))
(entdel (ssname ss i)))
 (setq MergedHatchList (subst (cons 91 ent#) (assoc 91 MergedHatchList) MergedHatchList)
     MergedHatchList (append MergedHatchList
			       (append (reverse
					(cdr (member (assoc 98 hentinfo)
						     (reverse (member (assoc 75 hentinfo) hentinfo)))))
				       (cons (cons 98 seedpt#) seedpts))))
 (if (= (cdr (assoc 70 hentinfo)) 1)
(setq MergedHatchList (append MergedHatchList (member (assoc 450 hentinfo) hentinfo))))
 (entmake MergedHatchList)
 (setq ent (entlast))
 (if (and (= (cdr (assoc 71 hentinfo)) 1) (not turnoff))
(mapcar '(lambda (x / entlist)
	    (setq entlist (entget (cdr x)))
	    (entmod (subst (cons 330 ent) (assoc 330 entlist) entlist)))
	  (massoc 330 MergedHatchList))
(princ "\nAll Hatch Associativity has been removed."))
 (vla-put-backgroundcolor (vlax-ename->vla-object ent) BGColor)
 (princ))
(defun c:MH () (c:MergeHatch))


;;; Changes objects that are set to one text style to another text style. Both styles need to be defined in the drawing.
;;; Posted by Peter
;;; http://forums.augi.com/showthread.php?22959-Help-Changing-text-style-in-blocks
;;;
;;; Use the foloowing format in the command line after loading the routine:
;;; (changestyle "oldtextstylename" "newtextstylename")
;;;
(defun ChangeStyle  (strStyle1 strStyle2 / entItem objBlock objDocument objItem)
 (vl-load-com)
 (setq objDocument (vla-get-activedocument (vlax-get-acad-object)))
 (if (and (tblobjname "style" strStyle1) (tblobjname "style" strStyle2))
(vlax-for objBlock  (vla-get-blocks objDocument)
 (if (> (vla-get-count objBlock) 0)
  (progn (setq objItem (vla-item objBlock 0)
		 entItem (vlax-vla-object->ename objItem))
	   (while entItem
	    (if	(and (vlax-property-available-p
		      (setq objItem (vlax-ename->vla-object entItem))
		      "StyleName")
		     (= (strcase (vla-get-stylename objItem)) (strcase strStyle1)))
	     (vla-put-stylename objItem strStyle2))
	    (setq entItem (entnext entItem))))))
(princ "\nError check if styles exist: "))
 (vla-regen objDocument 0))

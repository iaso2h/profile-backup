;;; ------------------------------------------------------------------------
;;;	FilteredSelection.lsp v1.4
;;;
;;;	Copyright© 11.03.09
;;;	Alan J. Thompson (alanjt)
;;;
;;;	Contact: alanjt @ TheSwamp.org, CADTutor.net
;;;
;;;	Permission to use, copy, modify, and distribute this software
;;;	for any purpose and without fee is hereby granted, provided
;;;	that the above copyright notice appears in all copies and
;;;	that both that copyright notice and the limited warranty and
;;;	restricted rights notice below appear in all supporting
;;;	documentation.
;;;
;;;	The following program(s) are provided "as is" and with all faults.
;;;	Alan J. Thompson DOES NOT warrant that the operation of the program(s)
;;;	will be uninterrupted and/or error free.
;;;
;;;	Allows user to create a filtered selection, based on certain criteria.
;;;	(Block Name, Color [object level], Entity Type, Layer, Linetype [object level])
;;;	Previous selection filter for each option is stored and user has option
;;;	to ignore locked layers.
;;;
;;;	Revision History:
;;;
;;;	v1.1 (09.24.10)    1. Complete rewrite.
;;;			   2. Added Color and Linetype as additional filter options.
;;;
;;;	v1.2 (10.05.10)    1. Reworked block name filtering sub (Dynamic blocks).
;;;			   3. Added "Previous" specified filtering selection option.
;;;			   2. Random cleanup.
;;;
;;;	v1.3 (10.07.10)	   1. Fixed "Previous" option.
;;;			   2. Tweaked _sel sub to display last stored selection filter.
;;;			   3. Now last stored selection filter for each option is
;;;			      stored in global variable.
;;;
;;;     v1.4 (2016.08.30)  1. Updated to account for True Color (_color sub updated)
;;;
;;; ------------------------------------------------------------------------

(defun c:FT (/ *error* foo _color _name _trim _fix _pretty _sel _unlocked _ssFix ent flt nice ss)
  ;; GLOBAL VARIABLES
  ;;  *FT:Option*
  ;;  *FT:Priors*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SUBROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*QUIT*,")))
      (progn (vl-bt) (princ (strcat "\nError: " msg)))
    )
  )

  (defun foo (x f)
    (wcmatch (vl-princ-to-string
               (cond ((cdr (assoc (car f) (entget x))))
                     ("")
               )
             )
             (cdr f)
    )
  )


  (defun _color (n)
    (vl-princ-to-string
      (cond
        ((assoc n
                '((0 . "ByBlock")
                  (1 . "Red")
                  (2 . "Yellow")
                  (3 . "Green")
                  (4 . "Cyan")
                  (5 . "Blue")
                  (6 . "Magenta")
                  (7 . "White")
                  (256 . "ByLayer")
                 )
         )
        )
        ((> n 256)
         (apply 'strcat
                (list (itoa (lsh (lsh n 8) -24))
                      ","
                      (itoa (lsh (lsh n 16) -24))
                      ","
                      (itoa (lsh (lsh n 24) -24))
                )
         )
        )
        (n)
      )
    )
  )


;;;  (defun _color (n)
;;;    (vl-princ-to-string
;;;      (cond ((assoc n
;;;                    '((0 . "ByBlock")
;;;                      (1 . "Red")
;;;                      (2 . "Yellow")
;;;                      (3 . "Green")
;;;                      (4 . "Cyan")
;;;                      (5 . "Blue")
;;;                      (6 . "Magenta")
;;;                      (7 . "White")
;;;                      (256 . "ByLayer")
;;;                     )
;;;             )
;;;            )
;;;            (n)
;;;      )
;;;    )
;;;  )

  (defun _name (e)
    ((lambda (o / n a)
       (cons 2
             (strcat (if (eq (vla-get-IsDynamicBlock o) :vlax-true)
                       "`*U*"
                       (_fix (vla-get-name o))
                     )
                     (cond ((vlax-property-available-p o 'EffectiveName)
                            (_fix (strcat "," (vla-get-effectivename o)))
                           )
                           ("")
                     )
             )
       )
     )
      (vlax-ename->vla-object e)
    )
  )


  (defun _trim (s)
    (substr s
            (+ 2
               (cond ((vl-string-search "," s))
                     (-1)
               )
            )
    )
  )

  (defun _fix (str)
    (if (eq (type str) 'STR)
      (vl-list->string
        (apply 'append
               (mapcar (function (lambda (i)
                                   (if (vl-position i '(35 42 46 64 91 125 126))
                                     (list 96 i)
                                     (list i)
                                   )
                                 )
                       )
                       (vl-string->list str)
               )
        )
      )
    )
  )

  (defun _pretty (s)
    (if (eq (type s) 'STR)
      (vl-list->string (vl-remove 96 (vl-string->list s)))
    )
  )


  (defun _sel (m f / e g)
    (setvar 'errno 0)
    (while (and (not g) (/= 52 (getvar 'errno)))
      (initget 0 "Yes No")
      (setq e (entsel (strcat "\nIgnore locked layers? [Yes/No] <"
                              (cond (*FT:Lock*)
                                    ((setq *FT:Lock* "No"))
                              )
                              ">: "
                              m
                              ((lambda (v)
                                 (cond (v (strcat " <" v ">: "))
                                       (": ")
                                 )
                               )
                                (cadr (assoc *FT:Option* *FT:Priors*))
                              )
                      )
              )
      )
      (cond
        ((vl-consp e)
         (setq g (cond ((and (_unlocked (car e)) (or (not f) (f (car e)))) (car e))
                       ((prompt "\nInvalid object!"))
                 )
         )
        )
        ((eq (type e) 'STR) (setq *FT:Lock* e))
        ((setq g (eq 52 (getvar 'errno))) nil)
        ((eq 7 (getvar 'errno)) (setq g (prompt "\nMissed, try again.")))
      )
    )
  )


  (defun _unlocked (e / n)
    (or
      (eq *FT:Lock* "No")
      (/= 4
          (logand 4
                  (cdr (assoc 70 (entget (tblobjname "LAYER" (setq n (cdr (assoc 8 (entget e))))))))
          )
      )
      ((lambda (msg)
         (if acet-ui-message
           (not (acet-ui-message msg "Error" 48))
           (progn (alert msg) nil)
         )
       )
        (strcat "Object on locked layer: \"" n "\"")
      )
    )
  )


  (defun _ssFix (ss dyn?)
    (if (and ss (eq *FT:Option* "Block") dyn?)
      ((lambda (i new / e o)
         (while (setq e (ssname ss (setq i (1+ i))))
           (and (eq (vla-get-IsDynamicBlock (setq o (vlax-ename->vla-object e))) :vlax-true)
                (wcmatch (vla-get-Effectivename o) (cdr (assoc 2 flt)))
                (ssadd e new)
           )
         )
         (if (> (sslength new) 0)
           (progn (sssetfirst nil new) (ssget "_I") (sssetfirst nil nil) new)
         )
       )
        -1
        (ssadd)
      )
      ss
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (vl-load-com)

  (if *FT:Priors*
    (initget 0 "Block Color Entity Layer linetYpe Previous")
    (initget 0 "Block Color Entity Layer linetYpe")
  )
  (setq *FT:Option*
         (cond
           ((getkword (strcat
                        (if *FT:Priors*
                          (strcat "\nPrevious \""
                                  (caar *FT:Priors*)
                                  "\" option filter: \""
                                  (cadar *FT:Priors*)
                                  "\"\nFilter choice: [Block/Color/Entity/Layer/linetYpe/Previous] <"
                          )
                          "\nFilter choice: [Block/Color/Entity/Layer/linetYpe] <"
                        )
                        (cond (*FT:Option*)
                              ((setq *FT:Option* "Layer"))
                        )
                        ">: "
                      )
            )
           )
           (*FT:Option*)
         )
  )

  (if
    (and
      (cond
        ((eq *FT:Option* "Previous")
         (setq flt (caddar *FT:Priors*))
         (princ
           (strcat "\nPrevious filter set: \"" (caar *FT:Priors*) "\" \"" (cadar *FT:Priors*) "\"")
         )
        )
        ((eq *FT:Option* "Block")
         (if (or (setq ent (_sel "\nSelect block for name" (lambda (x) (foo x '(0 . "INSERT")))))
                 (assoc *FT:Option* *FT:Priors*)
             )
           (princ
             (strcat
               "\nBlock: \""
               (setq
                 nice (_pretty (_trim (cdadr (setq flt (cond (ent (list '(0 . "INSERT") (_name ent)))
                                                             ((caddr (assoc *FT:Option* *FT:Priors*)))
                                                       )
                                             )
                                      )
                               )
                      )
               )
               "\" selected."
             )
           )
         )
        )
        ((eq *FT:Option* "Color")
         (if (setq ent (or (_sel "\nSelect object for color"
                                 (lambda (x)
                                   (setq flt (list (cond ((assoc 420 (entget x)))
                                                         ((assoc 62 (entget x)))
                                                         ('(62 . 256))
                                                   )
                                             )
                                   )
                                 )
                           )
                           (assoc *FT:Option* *FT:Priors*)
                       )
             )
           ((lambda (f)
              (princ (strcat (if (eq (caar f) 420)
                               "\nTrue color: \""
                               "\nColor: \""
                             )
                             (setq nice (_color (cdar f)))
                             "\" selected."
                     )
              )
            )
             (cond (flt)
                   ((setq flt (caddr (assoc *FT:Option* *FT:Priors*))))
             )
           )

;;;           (princ             
;;;             (strcat "\nColor: \""
;;;                     (setq nice (_color (cdar (cond (flt)
;;;                                                    ((setq flt (caddr (assoc *FT:Option* *FT:Priors*))))
;;;                                              )
;;;                                        )
;;;                                )
;;;                     )
;;;                     "\" selected."
;;;             )
;;;           )
         )
        )
        ((eq *FT:Option* "Entity")
         (if (or (setq ent (_sel "\nSelect object for entity type" nil))
                 (assoc *FT:Option* *FT:Priors*)
             )
           (princ (strcat "\nObject type: \""
                          (setq nice (cdar (setq flt (cond (ent (list (assoc 0 (entget ent))))
                                                           ((caddr (assoc *FT:Option* *FT:Priors*)))
                                                     )
                                           )
                                     )
                          )
                          "\" selected."
                  )
           )
         )
        )
        ((eq *FT:Option* "Layer")
         (if (or (setq ent (_sel "\nSelect object for layer" nil)) (assoc *FT:Option* *FT:Priors*))
           (princ
             (strcat
               "\nObject on layer: \""
               (setq nice
                      (_pretty (cdar (setq flt (cond (ent (list (cons 8 (_fix (cdr (assoc 8 (entget ent)))))))
                                                     ((caddr (assoc *FT:Option* *FT:Priors*)))
                                               )
                                     )
                               )
                      )
               )
               "\" selected."
             )
           )
         )
        )
        ((eq *FT:Option* "linetYpe")
         (if (or (setq ent (_sel "\nSelect object for linetype"
                                 (lambda (x)
                                   (setq flt (list (cond ((assoc 6 (entget x)))
                                                         ('(6 . "ByLayer"))
                                                   )
                                             )
                                   )
                                 )
                           )
                 )
                 (assoc *FT:Option* *FT:Priors*)
             )
           (princ (strcat "\nLinetype: \""
                          (setq nice (cond ((cdar flt))
                                           ((setq flt (caddr (assoc *FT:Option* *FT:Priors*))))
                                     )
                          )
                          "\" selected."
                  )
           )
         )
        )
      )
      (or (eq *FT:Option* "Previous")
          (setq *FT:Priors*
                 (cons
                   (list *FT:Option*
                         (cond (nice)
                               ((cadar *FT:Priors*))
                         )
                         flt
                         (if (eq *FT:Option* "Block")
                           (if ent
                             (eq (vla-get-IsDynamicBlock (vlax-ename->vla-object ent)) :vlax-true)
                             (last (car *FT:Priors*))
                           )
                         )
                   )
                   (cond ((vl-remove (assoc *FT:Option* *FT:Priors*) *FT:Priors*))
                         ('())
                   )
                 )
          )
      )
      (sssetfirst nil nil)
      (setq ss (_ssfix (if (eq *FT:Lock* "Yes")
                         (ssget "_:L" flt)
                         (ssget flt)
                       )
                       (last (car *FT:Priors*))
               )
      )
    )
     (progn (sssetfirst nil ss)
            (princ (strcat "\n" (itoa (sslength ss)) " object(s) selected."))
     )
  )
  (princ)
)
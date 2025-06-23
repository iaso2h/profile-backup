;;; ------------------------------------------------------------------------
;;;	SelectionFilter.lsp v1.2
;;;
;;;	Copyright© 03.31.09
;;;	Alan J. Thompson (alanjt)
;;;	alanjt@gmail.com
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
;;;	Allows user to either, select all objects on currrent layer, or
;;;	select all objects on layer, or all objects similar to, or all
;;;	objects similar to and on layer of selected object.
;;;
;;;	Revision History:
;;;
;;;	v1.1 (05.18.09) 1. Added subroutine 'AT:Entsel' for easier selection.
;;;			2. Removed current "ctab" filter from ssget to account
;;;			   for trying to select objects from within a viewport.
;;;
;;;	v1.2 (11.13.09) 1. Updated AT:Entsel subroutine & added AT:TabFilter.
;;;			2. Minor revision to coding.
;;;http://www.theswamp.org/index.php?topic=35028.msg402471#msg402471
;;; ------------------------------------------------------------------------

(defun c:SE (/) (c:SelectionFilter))
(defun c:SelectionFilter (/ *error* AT:Entsel AT:TabFilter #Ent #List #LayerName #SSGet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SUBROUTINES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; error handler
  (defun *error* (#Message)
    (and #Message
         (not (wcmatch (strcase #Message) "*BREAK*,*CANCEL*,*QUIT*"))
         (princ (strcat "\nError: " #Message))
    ) ;_ and
  ) ;_ defun


;;; Entsel or NEntsel with options
;;; #Nested - Entsel or Nentsel (T for Nentsel, nil for Entsel)
;;; #Message - Selection message (if nil, "\nSelect object: " is used)
;;; #FilterList - DXF ssget style filtering (nil if not required)
;;;               "V" as first item in list to convert object to VLA-OBJECT (must be in list if no DXF filtering)
;;;               "L" as first item in list to ignore locked layers (must be in list if no DXF filtering)
;;; #Keywords - Keywords to match instead of object selection (nil if not required)
;;; Example: (AT:Entsel nil "\nSelect MText not on 0 layer [Settings]: " '("LV" (0 . "MTEXT")(8 . "~0")) "Settings")
;;; Example: (AT:Entsel T "\nSelect object [Settings]: " '("LV") "Settings")
;;; Alan J. Thompson, 04.16.09
;;; Updated: Alan J. Thompson, 06.04.09 (changed filter coding to work as ssget style dxf filtering)
;;; Updated: Alan J. Thompson, 09.07.09 (added option to ignore locked layers and convert object to VLA-OBJECT
;;; Updated: Alan J. Thompson, 09.18.09 (fixed 'missed pick' alert)
  (defun AT:Entsel (#Nested #Message #FilterList #Keywords / #Count #Message #Choice #Ent
                    #VLA&Locked #FilterList
                   )
    (vl-load-com)
    (setvar "errno" 0)
    (setq #Count 0)
    ;; fix message
    (or #Message (setq #Message "\nSelect object: "))
    ;; set entsel/nentsel
    (if #Nested
      (setq #Choice nentsel)
      (setq #Choice entsel)
    ) ;_ if
    ;; check if option to convert to vla-object or ignore locked layers in #FilterList variable
    (and (vl-consp #FilterList)
         (eq (type (car #FilterList)) 'STR)
         (setq #VLA&Locked (car #FilterList)
               #FilterList (cdr #FilterList)
         ) ;_ setq
    ) ;_ and
    ;; select object
    (while (and (not #Ent) (/= (getvar "errno") 52))
      ;; if keywords
      (and #Keywords (initget #Keywords))
      (cond
        ((setq #Ent (#Choice #Message))
         ;; if ignore locked layers
         (and #VLA&Locked
              (vl-consp #Ent)
              (wcmatch (strcase #VLA&Locked) "*L*")
              (not
                (zerop
                  (cdr (assoc 70
                              (entget (tblobjname
                                        "layer"
                                        (cdr (assoc 8 (entget (car #Ent))))
                                      ) ;_ tblobjname
                              ) ;_ entget
                       ) ;_ assoc
                  ) ;_ cdr
                ) ;_ zerop
              ) ;_ not
              (setq #Ent nil
                    #Flag T
              ) ;_ setq
         ) ;_ and
         ;; #FilterList check
         (if (and #FilterList (vl-consp #Ent))
           ;; process filtering from #FilterList
           (or
             (not
               (member
                 nil
                 (mapcar '(lambda (x)
                            (wcmatch
                              (strcase
                                (vl-princ-to-string
                                  (cdr (assoc (car x) (entget (car #Ent))))
                                ) ;_ vl-princ-to-string
                              ) ;_ strcase
                              (strcase (vl-princ-to-string (cdr x)))
                            ) ;_ wcmatch
                          ) ;_ lambda
                         #FilterList
                 ) ;_ mapcar
               ) ;_ member
             ) ;_ not
             (setq #Ent nil
                   #Flag T
             ) ;_ setq
           ) ;_ or
         ) ;_ if
        )
      ) ;_ cond
      (and (or (= (getvar "errno") 7) #Flag)
           (/= (getvar "errno") 52)
           (not #Ent)
           (setq #Count (1+ #Count))
           (prompt (strcat "\nNope, keep trying!  "
                           (itoa #Count)
                           " missed pick(s)."
                   ) ;_ strcat
           ) ;_ prompt
      ) ;_ and
    ) ;_ while
    (if (and (vl-consp #Ent)
             #VLA&Locked
             (wcmatch (strcase #VLA&Locked) "*V*")
        ) ;_ and
      (vlax-ename->vla-object (car #Ent))
      #Ent
    ) ;_ if
  ) ;_ defun


;;; Tab filter for ssget selection filtering
;;; Must use (list instead of '( to work
;;; Alan J. Thompson, 06.05.09
  (defun AT:TabFilter (/)
    (if (eq 2 (getvar "cvport"))
      (cons 410 "Model")
      (cons 410 (getvar "ctab"))
    ) ;_ if
  ) ;_ defun



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN ROUTINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (or SE:#Choice (setq SE:#Choice "Layer"))

  ;; select object or issue keyword choice
  (if (or (and (setq #Ent (ssget "_I")) (setq #Ent (list (ssname #Ent 0))))
          (setq #Ent (AT:Entsel nil "\nSelect object on layer or [Current]: " nil "Current"))
      ) ;_ or
    (progn

      ;; object/choice executed, time to decide what to do
      (cond

        ;; object selected
        ((vl-consp #Ent)
         (initget 0 "Layer Objects Both")
         (setq #Choice
                (getkword
                  (strcat
                    "\nSelect all [Layer/Object type/Both (object type on layer)] <"
                    SE:#Choice
                    ">: "
                  ) ;_ strcat
                ) ;_ getkword
         ) ;_ setq
         (if #Choice
           (setq SE:#Choice #Choice)
           (setq #Choice SE:#Choice)
         ) ;_ if
         (cond
           ;; list for selection set of all objects on object's layer
           ((eq #Choice "Layer")
            (setq #List
                   (list (cons 8 (cdr (assoc 8 (entget (car #Ent))))))
            ) ;_ setq
           )
           ;; list for selection set of all objects of specified object name
           ((eq #Choice "Objects")
            (setq #List
                   (list (cons 0 (cdr (assoc 0 (entget (car #Ent))))))
            ) ;_ setq
           )
           ;; list for selection set of all objects of specified object name on object's layer
           ((eq #Choice "Both")
            (setq #List
                   (list (cons 8 (cdr (assoc 8 (entget (car #Ent)))))
                         (cons 0 (cdr (assoc 0 (entget (car #Ent)))))
                   ) ;_ list
            ) ;_ setq
           )
         ) ;_ cond
        )

        ;; Current layer to be selected
        ((eq #Ent "Current")
         (setq #List (list (cons 8 (getvar "clayer"))))
        )
      ) ;_ cond

      ;;filter parameters set, time to create the selection set
      (if
        (and #List
             (setq #SSGet (ssget "_X" (cons (AT:TabFilter) #List)))
        ) ;_ and
         (progn
           (cond
             ;; Current layer
             ((eq #Ent "Current")
              (princ (strcat "\nAll "
                             (rtos (sslength #SSGet) 2 0)
                             " objects on \""
                             (getvar "clayer")
                             "\" (the current layer), have been selected!"
                     ) ;_ strcat
              ) ;_ princ
             )
             ;; all objects on layer
             ((eq #Choice "Layer")
              (princ (strcat "\nAll "
                             (rtos (sslength #SSGet) 2 0)
                             " objects on layer \""
                             (cdr (assoc 8 (entget (car #Ent))))
                             "\" have been selected!"
                     ) ;_ strcat
              ) ;_ princ
             )
             ;; all objects
             ((eq #Choice "Objects")
              (princ (strcat "\nAll "
                             (rtos (sslength #SSGet) 2 0)
                             " \""
                             (cdr (assoc 0 (entget (car #Ent))))
                             "\" objects have been selected!"
                     ) ;_ strcat
              ) ;_ princ
             )
             ;; all objects on layer
             ((eq #Choice "Both")
              (princ (strcat "\nAll "
                             (rtos (sslength #SSGet) 2 0)
                             " \""
                             (cdr (assoc 0 (entget (car #Ent))))
                             "\" objects on layer \""
                             (cdr (assoc 8 (entget (car #Ent))))
                             "\" have been selected!"
                     ) ;_ strcat
              ) ;_ princ
             )
           ) ;_ cond
           (sssetfirst nil #SSGet)
         ) ;_ progn
         (prompt "\nNothing on layer to select!")
      ) ;_ if
    ) ;_ progn
  ) ;_ if
  (princ)
) ;_ defun
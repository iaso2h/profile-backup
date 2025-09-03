; by PBE
; http://www.cadtutor.net/forum/showthread.php?73328-Joining-2-commands
; erase a block and join the lines that the block broke
;; (defun c:hint () 
;;   (if (setq ss (ssget '((0 . "INSERT")))) 
;;     (repeat (setq i (sslength ss)) 
;;       (setq pt (cdr 
;;                  (assoc 10 
;;                         (entget 
;;                           (setq e (ssname ss (setq i (1- i))))
;;                         )
;;                  )
;;                )
;;       )
;;       (command "_rotate" e "" "_non" pt "180")
;;     )
;;   )
;;   (princ)
;; )
(defun c:`h () (c:plineHeal) (princ))
(defun c:plineHeal (/ pea $blk block i ll ur objs p1 p2) 
  (vl-load-com)
  (setq pea (getvar 'Peditaccept))
  (setvar 'PeditAccept 1)
  (if (setq $blk (ssget '((0 . "insert")))) 
    (repeat (setq i (sslength $blk)) 
      (setq e (ssname $blk (setq i (1- i))))
      (vla-getboundingbox (vlax-ename->vla-object e) 'll 'ur)
      (entdel e)
      (setq objs (ssget "C" 
                        (setq p1 (vlax-safearray->list ll))
                        (setq p2 (vlax-safearray->list ur))
                 )
      )
      (if (eq (cdr (assoc 0 (entget (ssname objs 0)))) "LWPOLYLINE") 
        (command "_.pedit" 
                 "_m"
                 objs
                 ""
                 "_join"
                 "_Joint"
                 "_Both"
                 (distance p1 p2)
                 ""
        )
        (command "_.join" (ssname objs 0) (ssname objs 1) "")
      )
    )
    (princ "\nNo Blocks Selected")
  )
  (setvar 'PeditAccept pea)
  (princ)
)
; by PBE
; http://www.cadtutor.net/forum/showthread.php?73328-Joining-2-commands
; erase a block and join the lines that the block broke
;; (defun c:hint () 
;;   (if (setq ss (ssget '((0 . "INSERT")))) 
;;     (repeat (setq i (sslength ss)) 
;;       (setq pt (cdr 
;;                  (assoc 10 
;;                         (entget 
;;                           (setq e (ssname ss (setq i (1- i))))
;;                         )
;;                  )
;;                )
;;       )
;;       (command "_rotate" e "" "_non" pt "180")
;;     )
;;   )
;;   (princ)
;; )
(defun c:`h () (c:plineHeal) (princ))
(defun c:plineHeal (/ pea $blk block i ll ur objs p1 p2) 
  (vl-load-com)
  (setq pea (getvar 'Peditaccept))
  (setvar 'PeditAccept 1)
  (if (setq $blk (ssget '((0 . "insert")))) 
    (repeat (setq i (sslength $blk)) 
      (setq e (ssname $blk (setq i (1- i))))
      (vla-getboundingbox (vlax-ename->vla-object e) 'll 'ur)
      (entdel e)
      (setq objs (ssget "C" 
                        (setq p1 (vlax-safearray->list ll))
                        (setq p2 (vlax-safearray->list ur))
                 )
      )
      (if (eq (cdr (assoc 0 (entget (ssname objs 0)))) "LWPOLYLINE") 
        (command "_.pedit" 
                 "_m"
                 objs
                 ""
                 "_join"
                 "_Joint"
                 "_Both"
                 (distance p1 p2)
                 ""
        )
        (command "_.join" (ssname objs 0) (ssname objs 1) "")
      )
    )
    (princ "\nNo Blocks Selected")
  )
  (setvar 'PeditAccept pea)
  (princ)
)
; by PBE
; http://www.cadtutor.net/forum/showthread.php?73328-Joining-2-commands
; erase a block and join the lines that the block broke
;; (defun c:hint () 
;;   (if (setq ss (ssget '((0 . "INSERT")))) 
;;     (repeat (setq i (sslength ss)) 
;;       (setq pt (cdr 
;;                  (assoc 10 
;;                         (entget 
;;                           (setq e (ssname ss (setq i (1- i))))
;;                         )
;;                  )
;;                )
;;       )
;;       (command "_rotate" e "" "_non" pt "180")
;;     )
;;   )
;;   (princ)
;; )
(defun c:`h () (c:plineHeal) (princ))
(defun c:plineHeal (/ pea $blk block i ll ur objs p1 p2) 
  (vl-load-com)
  (setq pea (getvar 'Peditaccept))
  (setvar 'PeditAccept 1)
  (if (setq $blk (ssget '((0 . "insert")))) 
    (repeat (setq i (sslength $blk)) 
      (setq e (ssname $blk (setq i (1- i))))
      (vla-getboundingbox (vlax-ename->vla-object e) 'll 'ur)
      (entdel e)
      (setq objs (ssget "C" 
                        (setq p1 (vlax-safearray->list ll))
                        (setq p2 (vlax-safearray->list ur))
                 )
      )
      (if (eq (cdr (assoc 0 (entget (ssname objs 0)))) "LWPOLYLINE") 
        (command "_.pedit" 
                 "_m"
                 objs
                 ""
                 "_join"
                 "_Joint"
                 "_Both"
                 (distance p1 p2)
                 ""
        )
        (command "_.join" (ssname objs 0) (ssname objs 1) "")
      )
    )
    (princ "\nNo Blocks Selected")
  )
  (setvar 'PeditAccept pea)
  (princ)
)
; by PBE
; http://www.cadtutor.net/forum/showthread.php?73328-Joining-2-commands
; erase a block and join the lines that the block broke
;; (defun c:hint () 
;;   (if (setq ss (ssget '((0 . "INSERT")))) 
;;     (repeat (setq i (sslength ss)) 
;;       (setq pt (cdr 
;;                  (assoc 10 
;;                         (entget 
;;                           (setq e (ssname ss (setq i (1- i))))
;;                         )
;;                  )
;;                )
;;       )
;;       (command "_rotate" e "" "_non" pt "180")
;;     )
;;   )
;;   (princ)
;; )
(defun c:`h () (c:plineHeal) (princ))
(defun c:plineHeal (/ pea $blk block i ll ur objs p1 p2) 
  (vl-load-com)
  (setq pea (getvar 'Peditaccept))
  (setvar 'PeditAccept 1)
  (if (setq $blk (ssget '((0 . "insert")))) 
    (repeat (setq i (sslength $blk)) 
      (setq e (ssname $blk (setq i (1- i))))
      (vla-getboundingbox (vlax-ename->vla-object e) 'll 'ur)
      (entdel e)
      (setq objs (ssget "C" 
                        (setq p1 (vlax-safearray->list ll))
                        (setq p2 (vlax-safearray->list ur))
                 )
      )
      (if (eq (cdr (assoc 0 (entget (ssname objs 0)))) "LWPOLYLINE") 
        (command "_.pedit" 
                 "_m"
                 objs
                 ""
                 "_join"
                 "_Joint"
                 "_Both"
                 (distance p1 p2)
                 ""
        )
        (command "_.join" (ssname objs 0) (ssname objs 1) "")
      )
    )
    (princ "\nNo Blocks Selected")
  )
  (setvar 'PeditAccept pea)
  (princ)
)

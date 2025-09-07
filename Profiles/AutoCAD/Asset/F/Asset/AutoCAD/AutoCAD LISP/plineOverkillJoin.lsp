(defun c:jj () (c:plineOverkillJoin) (princ))
(defun c:plineOverkillJoin (/ i ss ent countOptimized countJoined ssToJoin ssToJoinTypes 
                            savedEntLast savedEntLastValid
                           ) 

  (defun *error* (msg) 
    (if oldCmdEcho (setvar "CMDECHO" oldCmdEcho))
  )
  (if (setq ss (ssget "_:L")) 
    (progn 
      (terpri)

      (setq oldCmdEcho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)

      (setq savedEntLast (entlast))
      (setq savedEntLastValid savedEntLast)
      (command "undo" "be")
      (command "._-overkill" ss "" "_I" "_N" "_P" "_Y" "_T" "_Y" "_E" "_Y" "_A" "_Y" 
               ""
      )

      (setq countOptimized 0)
      (setq ssToJoinTypes '())
      (setq ssToJoin (ssadd))
      (setq i 0)
      ; Add existing entities to new selection set
      (repeat (sslength ss) 
        (setq ent (ssname ss i))
        (if (entget ent) 
          (progn 
            (ssadd ent ssToJoin)
            (setq ssToJoinTypes (append ssToJoinTypes 
                                     (list (cdr (assoc 0 (entget ent))))
                             )
            )
          )
          (setq countOptimized (1+ countOptimized))
        )
        (setq i (1+ i))
      )
      (princ (strcat "Optimized " (itoa countOptimized) " entities.\n"))

      ; Add new entities to new selection set
      (while (setq savedEntLast (entnext savedEntLast)) 
        (ssadd savedEntLast ssToJoin)
        (setq ssToJoinTypes (append ssToJoinTypes list ((assoc 0 (entget savedEntLast)))))
        (setq savedEntLastValid savedEntLast)
      )

      ; Check if "LWPolyline" entities exist in new selection set to determine whether there is an extra step when executing the `pedit` command
      (if (vl-remove "LWPOLYLINE" (LM:Unique ssToJoinTypes)) 
        (command "._pedit" "m" ssToJoin "" "Y" "J" "") ; There're other entities other than LWPOLYLINE
        (command "._pedit" "m" ssToJoin "" "J" "") ; Only contains LWPOLYLINE entities
      )
      (command)

      
      (setq countJoined 0)
      (while (setq savedEntLastValid (entnext savedEntLastValid))
        (setq countJoined (1+ countJoined))
      )

      ; Report
      (princ 
        (strcat "Joined " 
                (itoa (sslength ssToJoin))
                " entities into "
                (itoa countJoined)
                " polylines.\n"
        )
      )

      (command "undo" "e")
      (setvar "CMDECHO" oldCmdEcho)
    )
  )


  (princ)
)
;;; Function to close layers of selected entities
(defun c:layerCloseSelected (/ ss ent layname)
  ;; Start error handling
  (vl-load-com) ; Load Visual LISP extensions for ActiveX access
  (princ "\nSelect entities whose layers you want to close.")

  ;; Get a selection set from the user
  (setq ss (ssget))

  ;; Check if any entities were selected
  (if ss
    (progn
      ;; Iterate through each entity in the selection set
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)) ; Get the entity name
        (setq layname (cdr (assoc 8 (entget ent)))) ; Get the layer name of the entity

        ;; Get the layer object
        (setq layObj (vla-item (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))) layname))

        ;; Check if the layer exists and is not already off
        (if (and layObj (= :vlax-true (vla-get-layeron layObj)))
          (progn
            ;; Turn off the layer
            (vla-put-layeron layObj :vlax-false)
            (princ (strcat "\nLayer '" layname "' has been closed."))
          )
          (princ (strcat "\nLayer '" layname "' is already closed or does not exist."))
        )
        (setq i (1+ i)) ; Increment counter
      )
      (princ "\nAll selected layers have been processed.")
    )
    (princ "\nNo entities selected. Command aborted.")
  )
  (princ) ; Suppress the last expression's return value
)

;; Provide a command prompt message
(princ);;; Function to close layers of selected entities
(defun c:layerCloseSelected (/ ss ent layname)
  ;; Start error handling
  (vl-load-com) ; Load Visual LISP extensions for ActiveX access
  (princ "\nSelect entities whose layers you want to close.")

  ;; Get a selection set from the user
  (setq ss (ssget))

  ;; Check if any entities were selected
  (if ss
    (progn
      ;; Iterate through each entity in the selection set
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)) ; Get the entity name
        (setq layname (cdr (assoc 8 (entget ent)))) ; Get the layer name of the entity

        ;; Get the layer object
        (setq layObj (vla-item (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))) layname))

        ;; Check if the layer exists and is not already off
        (if (and layObj (= :vlax-true (vla-get-layeron layObj)))
          (progn
            ;; Turn off the layer
            (vla-put-layeron layObj :vlax-false)
            (princ (strcat "\nLayer '" layname "' has been closed."))
          )
          (princ (strcat "\nLayer '" layname "' is already closed or does not exist."))
        )
        (setq i (1+ i)) ; Increment counter
      )
      (princ "\nAll selected layers have been processed.")
    )
    (princ "\nNo entities selected. Command aborted.")
  )
  (princ) ; Suppress the last expression's return value
)

;; Provide a command prompt message
(princ);;; Function to close layers of selected entities
(defun c:layerCloseSelected (/ ss ent layname)
  ;; Start error handling
  (vl-load-com) ; Load Visual LISP extensions for ActiveX access
  (princ "\nSelect entities whose layers you want to close.")

  ;; Get a selection set from the user
  (setq ss (ssget))

  ;; Check if any entities were selected
  (if ss
    (progn
      ;; Iterate through each entity in the selection set
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)) ; Get the entity name
        (setq layname (cdr (assoc 8 (entget ent)))) ; Get the layer name of the entity

        ;; Get the layer object
        (setq layObj (vla-item (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))) layname))

        ;; Check if the layer exists and is not already off
        (if (and layObj (= :vlax-true (vla-get-layeron layObj)))
          (progn
            ;; Turn off the layer
            (vla-put-layeron layObj :vlax-false)
            (princ (strcat "\nLayer '" layname "' has been closed."))
          )
          (princ (strcat "\nLayer '" layname "' is already closed or does not exist."))
        )
        (setq i (1+ i)) ; Increment counter
      )
      (princ "\nAll selected layers have been processed.")
    )
    (princ "\nNo entities selected. Command aborted.")
  )
  (princ) ; Suppress the last expression's return value
)

;; Provide a command prompt message
(princ);;; Function to close layers of selected entities
(defun c:layerCloseSelected (/ ss ent layname)
  ;; Start error handling
  (vl-load-com) ; Load Visual LISP extensions for ActiveX access
  (princ "\nSelect entities whose layers you want to close.")

  ;; Get a selection set from the user
  (setq ss (ssget))

  ;; Check if any entities were selected
  (if ss
    (progn
      ;; Iterate through each entity in the selection set
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)) ; Get the entity name
        (setq layname (cdr (assoc 8 (entget ent)))) ; Get the layer name of the entity

        ;; Get the layer object
        (setq layObj (vla-item (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))) layname))

        ;; Check if the layer exists and is not already off
        (if (and layObj (= :vlax-true (vla-get-layeron layObj)))
          (progn
            ;; Turn off the layer
            (vla-put-layeron layObj :vlax-false)
            (princ (strcat "\nLayer '" layname "' has been closed."))
          )
          (princ (strcat "\nLayer '" layname "' is already closed or does not exist."))
        )
        (setq i (1+ i)) ; Increment counter
      )
      (princ "\nAll selected layers have been processed.")
    )
    (princ "\nNo entities selected. Command aborted.")
  )
  (princ) ; Suppress the last expression's return value
)

;; Provide a command prompt message
(princ)
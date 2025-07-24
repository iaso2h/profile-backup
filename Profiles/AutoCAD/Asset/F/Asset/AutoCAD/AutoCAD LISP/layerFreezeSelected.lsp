;;; Function to freeze layers of selected entities
(defun c:layerFreezeSelected (/ ss ent layname layObj)
  ;; Load Visual LISP extensions for ActiveX access
  (vl-load-com)
  (princ "\nSelect entities whose layers you want to freeze.")

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

        ;; Check if the layer exists and is not already frozen
        (if (and layObj (= :vlax-false (vla-get-freeze layObj)))
          (progn
            ;; Freeze the layer
            (vla-put-freeze layObj :vlax-true)
            (princ (strcat "\nLayer '" layname "' has been frozen."))
          )
          (princ (strcat "\nLayer '" layname "' is already frozen or does not exist."))
        )
        (setq i (1+ i)) ; Increment counter
      )
      (princ "\nAll selected layers have been processed (frozen).")
    )
    (princ "\nNo entities selected. Command aborted.")
  )
  (princ) ; Suppress the last expression's return value
)
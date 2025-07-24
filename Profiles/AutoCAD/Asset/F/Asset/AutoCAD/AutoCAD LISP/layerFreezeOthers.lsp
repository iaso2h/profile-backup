;;; Function to freeze all layers except for the one(s) of the selected entities
(defun c:layerFreezeOthers (/ ss ent layname layersToKeep allLayers layObj layerName)
  ;; Load Visual LISP extensions for ActiveX access
  (vl-load-com)
  (princ "\nSelect entities whose layers you want to keep unfrozen (all other layers will be frozen).")

  ;; Get a selection set from the user
  (setq ss (ssget))

  ;; Check if any entities were selected
  (if ss
    (progn
      ;; Initialize a list to store unique layer names of selected entities
      (setq layersToKeep '())

      ;; Iterate through each entity in the selection set to collect unique layer names
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)) ; Get the entity name
        (setq layname (cdr (assoc 8 (entget ent)))) ; Get the layer name of the entity

        ;; Add the layer name to the list if it's not already there
        (if (not (member layname layersToKeep))
          (setq layersToKeep (cons layname layersToKeep))
        )
        (setq i (1+ i)) ; Increment counter
      )

      ;; Get the collection of all layers in the active document
      (setq allLayers (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))

      ;; Iterate through all layers in the drawing
      (vlax-for layObj allLayers
        (setq layerName (vla-get-name layObj))

        ;; Check if the current layer's name is NOT in the list of layers to keep unfrozen
        ;; and if the layer is currently not frozen
        (if (and (not (member layerName layersToKeep))
                 (= :vlax-false (vla-get-freeze layObj)))
          (progn
            ;; Freeze the layer
            (vla-put-freeze layObj :vlax-true)
            (princ (strcat "\nLayer '" layerName "' has been frozen."))
          )
        )
      )
      (princ (strcat "\nAll layers except for: " (apply 'strcat (mapcar '(lambda (x) (strcat "'" x "' ")) layersToKeep)) " have been frozen."))
    )
    (progn
      (princ "\nNo entities selected. Command aborted.")
    )
  )
  (princ) ; Suppress the last expression's return value
)
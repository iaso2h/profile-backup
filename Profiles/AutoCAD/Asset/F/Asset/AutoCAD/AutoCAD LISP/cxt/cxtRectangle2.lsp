(defun c:gg (/ *error* p1 currentPoint grData grCode grVal continueLoop rectWidth 
             rectHeight boundaryOffset numLines rotationAngle lastRotationAngle p2 p4 
             p3 i currentOffset lineP1 lineP2 oldCmdEcho rawAngle
            ) 
  (terpri)
  (vl-load-com)
  ;; --- Error Handling Function ---
  ;; This function ensures that system variables are restored if the user cancels.
  (defun *error* (msg) 
    (if oldCmdEcho (setvar "CMDECHO" oldCmdEcho))
    (if oldFilletRad (setvar "FILLETRAD" oldFilletRad))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*BREAK*,*EXIT*"))) 
      (princ (strcat "\nError: " msg))
    )
    (princ) ; Suppress error message on quiet exit
  )
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; --- Setup and Initial Variables ---
  (setq CXTHeatingWireFilletRaidus 10)
  (setq oldCmdEcho (getvar "CMDECHO"))
  (setq oldFilletRad (getvar "FILLETRAD"))
  (setvar "CMDECHO" 0)

  ;; Define the geometry constants
  (setq rectWidth      600.0
        rectHeight     400.0
        boundaryOffset 5.0
        numLines       10
  )

  ;; Prompt the user for the first corner point of the rectangle.
  (setq p1 (getpoint "\nSpecify first corner point: "))

  (if (not p1) (exit))

  (setq continueLoop      T
        lastRotationAngle nil
  )

  (princ "\nMove mouse to rotate, click to place.")
  (while continueLoop 
    ;; grread arguments:
    ;; T: Allows tracking of mouse movement without requiring a point click.
    ;; 15: A bit-coded value to control grread's behavior.
    (setq grData (grread T 15))
    (setq grCode (car grData)) ; The event type (e.g., 5 for mouse move, 3 for click)
    (setq grVal (cadr grData)) ; The event data (e.g., cursor coordinates)

    (cond 
      ;; grCode 5: Mouse Movement (Update Preview)
      ((= grCode 5)
       (setq currentPoint grVal)

       ;; Calculate the raw angle from the first point to the current cursor position.
       (setq rawAngle (angle p1 currentPoint))
       ;; Snap the angle to the nearest 90-degree increment (0, 90, 180, 270) to create the "ortho-mode" rotation effect.
       (setq rotationAngle (* (/ PI 2.0) (fix (+ 0.5 (/ rawAngle (/ PI 2.0))))))

       ;; --- Redraw Preview ONLY if the angle has changed ---
       ;; This prevents flickering and erases the old preview before drawing the new one.
       (if (/= rotationAngle lastRotationAngle) 
         (progn 
           ;  (if lastRotationAngle
           ;    (progn
           ;      ;; Calculate points for the previous rectangle
           ;      (setq p2 (polar p1 lastRotationAngle rectWidth))
           ;      (setq p4 (polar p1 (+ lastRotationAngle (/ PI 2.0)) rectHeight))
           ;      (setq p3 (polar p2 (+ lastRotationAngle (/ PI 2.0)) rectHeight))

           ;      ;; Undraw by drawing again in XOR mode (highlightMode = 1)
           ;      (grdraw p1 p2 7 1)
           ;      (grdraw p2 p3 7 1)
           ;      (grdraw p3 p4 7 1)
           ;      (grdraw p4 p1 7 1)

           ;      ;; Undraw the previous internal lines
           ;      (setq i 1)
           ;      (while (<= i numLines)
           ;        (setq currentOffset (* i boundaryOffset))
           ;        (setq lineP1 (polar (polar p1 lastRotationAngle currentOffset)
           ;                            (+ lastRotationAngle (/ PI 2.0))
           ;                            currentOffset
           ;                     )
           ;        )
           ;        (setq lineP2 (polar (polar p1 lastRotationAngle currentOffset)
           ;                            (+ lastRotationAngle (/ PI 2.0))
           ;                            (- rectHeight currentOffset)
           ;                     )
           ;        )
           ;        (grdraw lineP1 lineP2 1 1)
           ;        (setq i (1+ i))
           ;      )
           ;    )
           ;  ) ; end if lastRotationAngle
           (if lastRotationAngle (redraw))

           ;; Calculate points for the new rectangle
           (setq p2 (polar p1 rotationAngle rectWidth))
           (setq p4 (polar p1 (+ rotationAngle (/ PI 2.0)) rectHeight))
           (setq p3 (polar p2 (+ rotationAngle (/ PI 2.0)) rectHeight))

           ;; Draw the new main rectangle outline
           (grdraw p1 p2 7 -1)
           (grdraw p2 p3 7 -1)
           (grdraw p3 p4 7 -1)
           (grdraw p4 p1 7 -1)

           ;; Draw the new internal lines
           ;  (setq i 1)
           ;  (while (<= i numLines)
           ;    (setq currentOffset (* i boundaryOffset))
           ;    (setq lineP1 (polar (polar p1 rotationAngle currentOffset)
           ;                        (+ rotationAngle (/ PI 2.0))
           ;                        currentOffset
           ;                 )
           ;    )
           ;    (setq lineP2 (polar (polar p1 rotationAngle currentOffset)
           ;                        (+ rotationAngle (/ PI 2.0))
           ;                        (- rectHeight currentOffset)
           ;                 )
           ;    )
           ;    (grdraw lineP1 lineP2 1 -1)
           ;    (setq i (1+ i))
           ;  )

           (setq lastRotationAngle rotationAngle)
         ) ; end progn for redraw
       ) ; end if angle changed
      ) ; End of mouse movement case

      ;; grCode 3: Left Mouse Click (Finalize and Draw)
      ((= grCode 3)
       ;; The user has clicked to confirm the placement.
       ;; We use the last calculated rotationAngle from the preview.

       ;; --- Draw Permanent Geometry ---
       ;; Use the 'command' function to draw the final, permanent entities.
       (princ "\nDrawing final geometry...")

       ;; Draw the main rectangle using the PLINE command.
       ;  (command "_.pline" p1 p2 p3 p4 "_C")
       (entmake 
         (append 
           (list '(0 . "LWPOLYLINE") 
                 '(100 . "AcDbEntity")
                 '(100 . "AcDbPolyline")
                 '(90 . 4)
                 '(70 . 1) ; Closed polyline
           )
           (mapcar '(lambda (pt) (cons 10 pt)) (list p1 p2 p3 p4))
         )
       )
       (command "_.fillet" "_r" CXTHeatingWireFilletRaidus "")
       (command "_.fillet" "_p" "_l")

       ;; Draw the 10 permanent internal lines using the LINE command.
       (setq i 1)
       (while (<= i numLines) 
         (setq currentOffset (* i boundaryOffset))
         (setq lineP1 (polar (polar p1 rotationAngle currentOffset) 
                             (+ rotationAngle (/ PI 2.0))
                             currentOffset
                      )
         )
         (setq lineP2 (polar (polar p1 rotationAngle currentOffset) 
                             (+ rotationAngle (/ PI 2.0))
                             (- rectHeight currentOffset)
                      )
         )
         (command "_.line" lineP1 lineP2 "")
         (setq i (1+ i))
       )

       (setq continueLoop nil) ; Set flag to exit the 'while' loop.
       (redraw)
      )

      ;; grCode 2 or 11: Keyboard Input (Cancel)
      ((or (= grCode 2) (= grCode 11))
       (princ "\n*Cancel*")
       (setq continueLoop nil) ; Exit the loop on key press (e.g., Escape).
      )
    ) ; End cond
  ) ; End while

  ;; --- Cleanup ---
  (setvar "CMDECHO" oldCmdEcho) ; Restore original command echo setting.
  (setvar "FILLETRAD" oldFilletRad)
  (princ) ; Suppress the echo of the last evaluation in the command line.
)

;;; --- Load Message ---
(princ "\nType 'gg' to run the custom rectangle command.")
(princ)
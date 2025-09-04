;;; -----------------------------------------------------------------------------
;;; Command: gg
;;; Author: Gemini
;;; Date: September 5, 2025
;;;
;;; Description:
;;; This program draws a 600x400 rectangle with detailed internal lines.
;;; The user first picks a corner point. As the mouse moves, a preview of the
;;; rectangle and its contents rotates in 90-degree increments around the
;;; initial point, following the cursor. A second click confirms the final
;;; position and rotation, and draws the permanent geometry.
;;; This interactive preview is achieved using the 'grread' function.
;;;
;;; Usage:
;;; 1. Load this file into AutoCAD using the APPLOAD command or by dragging
;;;    and dropping it into the drawing window.
;;; 2. Type "gg" in the command line and press Enter.
;;; 3. Click to specify the first corner of the rectangle.
;;; 4. Move the mouse to see the preview rotate.
;;; 5. Click a second time to place the rectangle.
;;; -----------------------------------------------------------------------------

(defun c:gg (/ *error* p1 currentPoint grData grCode grVal continueLoop rectWidth 
             rectHeight boundaryOffset numLines rotationAngle p2 p4 p3 i currentOffset 
             lineP1 lineP2 oldCmdEcho
            ) 

  ;; --- Error Handling Function ---
  ;; This function ensures that system variables are restored if the user cancels.
  (defun *error* (msg) 
    (if oldCmdEcho (setvar "CMDECHO" oldCmdEcho))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*BREAK*,*EXIT*"))) 
      (princ (strcat "\nError: " msg))
    )
    (princ) ; Suppress error message on quiet exit
  )

  ;; --- Setup and Initial Variables ---
  (setq oldCmdEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; Define the geometry constants
  (setq rectWidth      600.0
        rectHeight     400.0
        boundaryOffset 5.0
        numLines       10
  )

  ;; --- Get First User Input ---
  ;; Prompt the user for the first corner point of the rectangle.
  (setq p1 (getpoint "\nSpecify first corner point: "))

  ;; Proceed only if the user provides a point.
  (if p1 
    (progn 
      (setq continueLoop T)

      ;; --- Interactive Preview Loop using grread ---
      ;; grread is used to track mouse movement and clicks in real-time.
      (princ "\nMove mouse to rotate, click to place.")
      (while continueLoop 
        ;; grread arguments:
        ;; T: Allows tracking of mouse movement without requiring a point click.
        ;; 15: A bit-coded value to control grread's behavior.
        (setq grData (grread T 15))
        (setq grCode (car grData)) ; The event type (e.g., 5 for mouse move, 3 for click)
        (setq grVal (cadr grData)) ; The event data (e.g., cursor coordinates)

        ;; The 'cond' statement directs the program based on user action.
        (cond 
          ;; grCode 5: Mouse Movement (Update Preview)
          ((= grCode 5)
           (setq currentPoint grVal)

           ;; Calculate the raw angle from the first point to the current cursor position.
           (setq rawAngle (angle p1 currentPoint))

           ;; Snap the angle to the nearest 90-degree increment (0, 90, 180, 270).
           ;; This creates the "ortho-mode" rotation effect.
           (setq rotationAngle (* (/ PI 2.0) (fix (+ 0.5 (/ rawAngle (/ PI 2.0))))))

           ;; --- Calculate Preview Geometry ---
           ;; Calculate the other three corners of the main rectangle based on the
           ;; first point (p1) and the snapped rotation angle.
           (setq p2 (polar p1 rotationAngle rectWidth))
           (setq p4 (polar p1 (+ rotationAngle (/ PI 2.0)) rectHeight))
           (setq p3 (polar p2 (+ rotationAngle (/ PI 2.0)) rectHeight))

           ;; --- Draw the Preview using grdraw ---
           ;; 'grdraw' is used for drawing temporary graphics that don't get
           ;; added to the drawing database. It's perfect for previews.

           ;; Draw the main rectangle outline.
           (grdraw p1 p2 1 -1) ; from p1 to p2
           (grdraw p2 p3 1 -1) ; from p2 to p3
           (grdraw p3 p4 1 -1) ; from p3 to p4
           (grdraw p4 p1 1 -1) ; from p4 to p1 (close)

           ;; Draw the 10 internal lines.
           (setq i 1)
           (while (<= i numLines) 
             (setq currentOffset (* i boundaryOffset))

             ;; Calculate the start and end points for each internal line.
             ;; The logic uses polar coordinates to offset from the base point p1
             ;; along the rotated axes of the rectangle.
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

             ;; Draw the temporary line.
             (grdraw lineP1 lineP2 1 -1)

             (setq i (1+ i))
           )
          ) ; End of mouse movement case

          ;; grCode 3: Left Mouse Click (Finalize and Draw)
          ((= grCode 3)
           ;; The user has clicked to confirm the placement.
           ;; We use the last calculated rotationAngle from the preview.

           ;; --- Draw Permanent Geometry ---
           ;; Use the 'command' function to draw the final, permanent entities.
           (princ "\nDrawing final geometry...")

           ;; Draw the main rectangle using the PLINE command.
           (command "_.pline" p1 p2 p3 p4 "_C")

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
          )

          ;; grCode 2 or 11: Keyboard Input (Cancel)
          ((or (= grCode 2) (= grCode 11))
           (princ "\n*Cancel*")
           (setq continueLoop nil) ; Exit the loop on key press (e.g., Escape).
          )
        ) ; End cond
      ) ; End while
    ) ; End progn
  ) ; End if

  ;; --- Cleanup ---
  (setvar "CMDECHO" oldCmdEcho) ; Restore original command echo setting.
  (princ) ; Suppress the echo of the last evaluation in the command line.
)

;;; --- Load Message ---
(princ "\nType 'gg' to run the custom rectangle command.")
(princ)
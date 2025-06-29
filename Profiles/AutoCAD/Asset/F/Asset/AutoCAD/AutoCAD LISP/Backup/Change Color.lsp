; eoc is stand for Edit Object Color
; Design by : Adesu 
; Email : mteybid@yuasabattery.co.id
; Homepage : http://www.yuasa-battery.co.id
; Create : 24 February 2006
; Program no.: 329/02/2006
; Edit by :
(defun c:eoc (/ ss sse sf cur veo tc ci att opt el sse_el sf_el veo1 tc1 ci1) 
  (if (setq ss (car (entsel "\nSelect a object"))) 
    (progn 
      (setq sse (entget ss))
      (setq sf (cdr (assoc 62 sse)))
      (if sf 
        (setq cur (itoa sf))
        (progn 
          (vl-load-com)
          (setq veo (vlax-ename->vla-object ss))
          (setq tc (vlax-get-property veo 'TrueColor))
          (setq ci (vlax-get-property tc 'ColorIndex))
          (setq cur (itoa ci))
        ) ; progn
      ) ; if
      (setq att "\nEnter new value of color")
      (while 
        (setq opt (fix 
                    (getreal 
                      (strcat att " <" cur ">: ")
                    )
                  )
        ) ; check if user put value and hiwt
        ; enter,program would repeat ;again.
        ; if user only hit enter the ;program ; would stop
        (cond 
          ((= opt nil) (setq sf sf))
          ((/= opt nil) (setq sf opt))
        )
        (command "_change" ss "" "p" "c" sf "")
        (setq el (entlast))
        (setq sse_el (entget el))
        (setq sf_el (cdr (assoc 62 sse_el)))
        (if sf 
          (setq cur (itoa sf))
          (progn 
            (vl-load-com)
            (setq veo1 (vlax-ename->vla-object ss))
            (setq tc1 (vlax-get-property veo 'TrueColor))
            (setq ci1 (vlax-get-property tc 'ColorIndex))
            (setq cur (itoa ci))
          ) ; progn
        ) ; if
      ) ; while
    ) ; progn
    (alert "\nInvalid selected object,please try again")
  ) ; if
  (princ)
)
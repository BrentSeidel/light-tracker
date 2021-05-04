(defun sensors ()
  (print "Analog 1 = " (read-analog 1)) (terpri)
  (print "Analog 2 = " (read-analog 2)) (terpri)
  (print "Analog 3 = " (read-analog 3)) (terpri)
  (print "Analog 4 = " (read-analog 4)) (terpri))
;
;--
;  Initialize the stepper controls and global variables
;
(defun setup ()
  (pin-mode 52 0)
  (pin-pullup 52 t)
  (stepper-init 1 22 23 24 25)
  (stepper-init 2 28 29 30 31)
  (setq *PAN-DEAD* 10)
  (setq *TILT-DEAD* 10)
  (setq *ESC* (code-char 27))
  (stepper-off 1)
  (stepper-off 2))
;
;  Perform tracking for a specified number of steps.  Right now, the steps are
;  limited to help prevent runaway during testing.
;
(defun track ()
  (print *ESC* "[2J")
  (let (a1 a2 a3 a4 (tilt-pos 0) (pan-pos 0))
    (dowhile (= (pin-read 52) 1)
      (setq a1 (analog-read 1))
      (setq a2 (analog-read 2))
      (setq a3 (analog-read 3))
      (setq a4 (analog-read 4))
      (setq a1 (+ a1 (analog-read 1)))
      (setq a2 (+ a2 (analog-read 2)))
      (setq a3 (+ a3 (analog-read 3)))
      (setq a4 (+ a4 (analog-read 4)))
      (print *ESC* "[10;10H" *ESC* "[2K" a1)
      (print *ESC* "[10;30H" a2)
      (print *ESC* "[15;10H" *ESC* "[2K" a3)
      (print *ESC* "[15;30H" a4)
      (print *ESC* "[20;1H" *ESC* "[2K")
      (print "Sum " (+ a1 a2 a3 a4))
      (if (> (+ a1 a2) (+ a3 a4 *TILT-DEAD*))
        (progn
          (stepper-step 2 -1)
          (setq tilt-pos (- tilt-pos 1))))
      (if (< (+ a1 a2 *TILT-DEAD*) (+ a3 a4))
        (progn
          (stepper-step 2 1)
          (setq tilt-pos (+ tilt-pos 1))))
      (if (> (+ a1 a3) (+ a2 a4 *PAN-DEAD*))
        (progn
          (stepper-step 1 -1)
          (setq pan-pos (- pan-pos 1))))
      (if (< (+ a1 a3 *PAN-DEAD*) (+ a2 a4))
        (progn
          (stepper-step 1 1)
          (setq pan-pos (+ pan-pos 1)))))
    (print "Ending tilt " tilt-pos)
    (print ", Ending pan " pan-pos))
  (terpri)
  (stepper-off 1)
  (stepper-off 2))

(defun screen-clear ()
  (print *ESC* "[2J"))

(defun screen-home ()
  (print *ESC* "[1;1H"))
;
;  Doesn't work because of spaces inserted before the numbers.
;
(defun screen-pos (row col)
  (print *ESC* "[" row ";" col "H"))


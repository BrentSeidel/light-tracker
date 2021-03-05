(defun sensors ()
  (print "Analog 1 = " (read-analog 1)) (terpri)
  (print "Analog 2 = " (read-analog 2)) (terpri)
  (print "Analog 3 = " (read-analog 3)) (terpri)
  (print "Analog 4 = " (read-analog 4)) (terpri))
;
;  Sensors are:
;
;   4 | 3
;  ---+---
;   2 | 1
;        [connector]
;
;
;  Steppers are:
;  1 - Pan (step + is clockwise)
;  2 - Tilt (step+ is tilt up
;
;--
;  Initialize the stepper controls and global variables
;
(defun setup ()
  (stepper-init 1 22 23 24 25)
  (stepper-init 2 28 29 30 31)
  (setq *PAN_POS* 0)
  (setq *TILT_POS* 0)
  (setq *PAN-DEAD* 10)
  (setq *TILT-DEAD* 10))
;
;  Perform tracking for a specified number of steps.  Right now, the steps are
;  limited to help prevent runaway during testing.
;
(defun track (times)
  (let (a1 a2 a3 a4)
    (dotimes (x times)
      (setq a1 (read-analog 1))
      (setq a2 (read-analog 2))
      (setq a3 (read-analog 3))
      (setq a4 (read-analog 4))
      (print "Sum " (+ a1 a2 a3 a4))
      (terpri)
      (if (> (+ a1 a2) (+ a3 a4 *TILT-DEAD*))
        (progn
          (step 2 -1)
          (setq *TILT-POS* (- *TILT-POS* 1))))
      (if (< (+ a1 a2 *TILT-DEAD*) (+ a3 a4))
        (progn
          (step 2 1)
          (setq *TILT-POS* (+ *TILT-POS* 1))))
      (if (> (+ a1 a3) (+ a2 a4 *PAN-DEAD*))
        (progn
          (step 1 -1)
          (setq *PAN-POS* (- *PAN-POS* 1))))
      (if (< (+ a1 a3 *PAN-DEAD*) (+ a2 a4))
        (progn
          (step 1 1)
          (setq *PAN-POS* (+ *PAN-POS* 1))))))
  (stepper-off 1)
  (stepper-off 2))
;
;  Looking at the pins of a male connector, the pins are numbered as follows:
;  1   2   3   4   5
;    6   7   8   9

;  Connector pinout for stepper motors.
;  1 - H1, MA+ (gpio 22 green)
;  2 - H1, MB+ (gpio 24 red)
;  3 - n.c.
;  4 - H2, MA+ (gpio 28 green)
;  5 - H2, MB+ (gpio 30 red)
;  6 - H1, MA- (gpio 23 grey)
;  7 - H1, MB- (gpio 25 yellow)
;  8 - H2, MA- (gpio 29 grey)
;  9 - H2, MB- (gpio 31 yellow)


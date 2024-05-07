;;;; Sensors: objects for mouse point detection customization

(in-package #:kslgui)

(export 'sensor)
(defstruct (sensor (:copier nil))
  (hitp (error "Default SENSOR-HITP used") :type (function (ui sensor single-float single-float single-float single-float single-float single-float) (values boolean &optional))))

(export 'hit-sensor-p)
(declaim
  (inline hit-sensor-p)
  (ftype (function (ui (or (member nil :visual) sensor)
                       (or null visual-state (vector visual-state))
                       single-float single-float single-float single-float single-float single-float)
                   (values boolean &optional))
         hit-sensor-p))
(defun hit-sensor-p (ui sensor vstate x y w h mx my)
  (if (symbolp sensor)
      (if (eq :visual sensor)
          (let ((x (coerce x 'double-float))
                (y (coerce y 'double-float))
                (w (coerce w 'double-float))
                (h (coerce h 'double-float))
                (mx (coerce mx 'double-float))
                (my (coerce my 'double-float)))
            (cond
             ((null vstate) nil)
             ((arrayp vstate) (reduce (lambda (acc vstate)
                                        (or acc (and vstate
                                                     (visual-state-hitp vstate)
                                                     (funcall (visual-state-hitp vstate) ui vstate x y w h mx my))))
                                  vstate :initial-value nil))
             (t (if (symbolp (visual-state-hitp vstate))
                    (visual-state-hitp vstate)
                    (funcall (visual-state-hitp vstate) ui vstate x y w h mx my)))))
          sensor)
      (funcall (sensor-hitp sensor) ui sensor x y w h mx my)))

(export 'hit-sensor-p*)
(declaim (inline hit-sensor-p*))
(defun hit-sensor-p* (ui sensor-computed vstate x y w h mx my)
  (hit-sensor-p ui (sdet:compute sensor-computed) vstate x y w h mx my))


;;; Rectangle

(defun rectangle-sensor-hitp-impl (ui sensor x y w h mx my)
  (declare (ignore ui sensor))
  (and (>= mx x)
       (>= my y)
       (< mx (+ x w))
       (< my (+ y h))))

(export 'make-rectangle-sensor)
(defstruct (rectangle-sensor (:include sensor)
                             (:copier nil)
                             (:constructor make-rectangle-sensor (&aux (hitp #'rectangle-sensor-hitp-impl)))))

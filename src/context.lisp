(in-package #:kslgui)

(export '(ui create-ui))
(defstruct (ui (:copier nil)
               (:constructor create-ui (&key
                                        (sdet-context (sdet:make-context))
                                        cursor-renderer
                                        &aux
                                        (kb-focus-st (multiple-value-list (sdet:make-state sdet-context nil)))
                                        (get-keyboard-focus (first kb-focus-st))
                                        (set-keyboard-focus (second kb-focus-st)))))
  (windows (make-hash-table) :type hash-table)
  (scroll-sensitivity-x +base-scroll-sensitivity-x+ :type double-float)
  (scroll-sensitivity-y +base-scroll-sensitivity-y+ :type double-float)
  (reset-focus-on-mouse-down t :type boolean)
  (delta-time 0.0d0 :type double-float)
  (time 0.0d0 :type double-float)
  (start-time nil :type (or null double-float))
  (previous-frame-time nil :type (or null double-float))
  (cursor-visible t :type boolean)
  (cursor-renderer nil :type t)
  (on-text-input-started nil :type (or null (function () (values &optional))))
  (on-text-input-finished nil :type (or null (function () (values &optional))))
  (key-to-character nil :type (or null (function (t) (or null character))))
  (key-to-action nil :type (or null (function (t) (or null key-action))))
  (yoga-config (yogalayout:config-new) :type %yogalayout:config-ref :read-only t)
  (mouse-owner nil :type (or null widget)) ; nil = all widgets recieve events when mouse is over them
  (cursor :default :type cursor)
  (cursor-layer nil :type (or null layer))
  (cursor-x 0.0 :type single-float)
  (cursor-y 0.0 :type single-float)
  (focused-window nil :type (or null window))
  (get-keyboard-focus (unreachable) :type (function () (values (or null widget) &optional)) :read-only t)
  (set-keyboard-focus (unreachable) :type (function ((or null widget)) (values &optional)) :read-only t)
  (sdet-context (unreachable) :type sdet:context :read-only t)
  (temp-parent nil :type (or null widget))
  (temp-sibling-index 0 :type fixnum)
  (temp-first-widget nil :type (or null widget))
  (temp-root nil :type (or null widget))
  (temp-layer nil :type (or null layer))
  (blend2d-rect-pool (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (blend2d-point-pool (make-array 0 :adjustable t :fill-pointer 0) :type vector))

(export 'destroy-ui)
(declaim (ftype (function (ui) (values &optional)) destroy-ui))
(defun destroy-ui (ui)
  (yogalayout:config-free (ui-yoga-config ui))
  (clear-blend2d-rect-pool ui)
  (clear-blend2d-point-pool ui)
  (values))

(export 'time)
(declaim
  (inline time)
  (ftype (function (ui) (values double-float &optional)) time))
(defun time (ui)
  (ui-time ui))

(export 'delta-time)
(declaim
  (inline delta-time)
  (ftype (function (ui) (values double-float &optional)) delta-time))
(defun delta-time (ui)
  (ui-delta-time ui))

(export 'time-sf)
(declaim
  (inline time-sf)
  (ftype (function (ui) (values single-float &optional)) time-sf))
(defun time-sf (ui)
  (coerce (ui-time ui) 'single-float))

(export 'delta-time-sf)
(declaim
  (inline delta-time-sf)
  (ftype (function (ui) (values single-float &optional)) delta-time-sf))
(defun delta-time-sf (ui)
  (coerce (ui-delta-time ui) 'single-float))

(export 'sdet-context)
(declaim
  (inline sdet-context)
  (ftype (function (ui) (values sdet:context  &optional)) sdet-context))
(defun sdet-context (ui)
  (ui-sdet-context ui))

(export 'insert-window)
(defun insert-window (ui window)
  (multiple-value-bind (windows present)
      (gethash (window-layer window) (ui-windows ui))
    (unless present
      (setf windows
        (setf (gethash (window-layer window) (ui-windows ui))
          (make-array 0 :adjustable t :fill-pointer 0))))
    (vector-push-extend window windows))
  (values))

(export 'delete-window)
(defun delete-window (ui window)
  (let ((layer (window-layer window)))
    (multiple-value-bind (windows present) (gethash layer (ui-windows ui))
      (if (not present)
          (error "Internal: trying to delete a window with unregistered layer.")
          (let ((count (count window windows)))
            (if (/= 1 count)
                (error "Internal: invalid count of the same window: ~a" count)
                (let ((new-windows (vector-delete window windows)))
                  (if (= 0 (length new-windows))
                      (remhash layer (ui-windows ui))
                      (setf (gethash layer (ui-windows ui)) new-windows)))))))
    (values)))

(export 'set-window-layer)
(declaim (ftype (function (ui window (or null layer)) (values &optional)) set-window-layer))
(defun set-window-layer (ui window layer)
  (multiple-value-bind (windows present)
      (gethash (window-layer window) (ui-windows ui))
    (unless present
      (error "Window not found in context."))
    (setf (gethash (window-layer window) (ui-windows ui)) (vector-delete window windows))
    (setf (window-layer window) layer)
    (insert-window ui window))
  (values))

(export 'set-scroll-sensitivity)
(declaim (ftype (function (ui float float) (values &optional)) set-scroll-sensitivity))
(defun set-scroll-sensitivity (ui x y)
  (setf (ui-scroll-sensitivity-x ui) (* +base-scroll-sensitivity-x+ (coerce x 'double-float)))
  (setf (ui-scroll-sensitivity-y ui) (* +base-scroll-sensitivity-y+ (coerce y 'double-float)))
  (values))

(export 'set-reset-focus-on-mouse-down)
(declaim (ftype (function (ui boolean) (values &optional)) set-reset-focus-on-mouse-down))
(defun set-reset-focus-on-mouse-down (ui reset-focus-on-mouse-down)
  (setf (ui-reset-focus-on-mouse-down ui) reset-focus-on-mouse-down)
  (values))

(export 'set-cursor-visibility)
(declaim (ftype (function (ui boolean) (values &optional)) set-cursor-visibility))
(defun set-cursor-visibility (ui visible)
  (setf (ui-cursor-visible ui) visible)
  (values))

(export 'set-cursor-renderer)
(declaim (ftype (function (ui t) (values &optional)) set-cursor-renderer))
(defun set-cursor-renderer (ui renderer)
  (setf (ui-cursor-renderer ui) renderer)
  (values))

(export 'set-key-to-character)
(declaim (ftype (function (ui (or null (function (t) (or null character)))) (values &optional)) set-key-to-character))
(defun set-key-to-character (ui key-to-character)
  (setf (ui-key-to-character ui) key-to-character)
  (values))

(export 'set-key-to-action)
(declaim (ftype (function (ui (or null (function (t) (or null key-action)))) (values &optional)) set-key-to-action))
(defun set-key-to-action (ui key-to-action)
  (setf (ui-key-to-action ui) key-to-action)
  (values))

(defmacro define-pool-functions (type-name pool-accessor alloc-form (freed-sym free-form))
  `(progn
    (defun ,(intern (concatenate 'string "ALLOC-" type-name)) (ui)
      (if (= 0 (length (,pool-accessor ui)))
          ,alloc-form
          (progn
           (decf (fill-pointer (,pool-accessor ui)))
           (aref (,pool-accessor ui) (fill-pointer (,pool-accessor ui))))))

    (defun ,(intern (concatenate 'string "FREE-" type-name)) (ui x)
      (vector-push-extend x (,pool-accessor ui)))

    (defun ,(intern (concatenate 'string "CLEAR-" type-name "-POOL")) (ui)
      (loop #:for ,freed-sym #:across (,pool-accessor ui)
            #:do ,free-form)
      (setf (fill-pointer (,pool-accessor ui)) 0))))

(define-pool-functions "BLEND2D-RECT" ui-blend2d-rect-pool (autowrap:alloc '%blend2d:rect) (x (autowrap:free x)))
(define-pool-functions "BLEND2D-POINT" ui-blend2d-point-pool (autowrap:alloc '%blend2d:point) (x (autowrap:free x)))

(export '*ui*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ui*
                nil
                "A variable used by widgets and their argument expressions to access the library context.
                Stores a form.
                Provided by MACROEXPAND-WITH-UI, for example in COMPOSE and property expansions."))

(export 'macroexpand-with-ui)
(defmacro macroexpand-with-ui (ui &body body)
  (assert (= 1 (length body)))
  (alexandria:with-gensyms (previous-ui)
    `(let ((,previous-ui *ui*))
       (setf *ui* (or ,ui ,previous-ui))
       (multiple-value-prog1
           (trivial-macroexpand-all:macroexpand-all ,(first body))
         (setf *ui* ,previous-ui)))))

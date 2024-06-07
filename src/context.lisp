(in-package #:kslgui)

(export '(ui create-ui))
;; NOTE: sometimes changing UI may cause strange errors in compiler.
;; Solution: clear fasls (or try fiddling with files loaded before this file (esp. visual))
(defstruct (ui (:copier nil)
               (:constructor create-ui (&key
                                        (sdet-context (sdet:make-context))
                                        cursor-renderer
                                        &aux
                                        (kb-focus-st (multiple-value-list (sdet:make-state sdet-context nil)))
                                        (get-keyboard-focus (first kb-focus-st))
                                        (set-keyboard-focus (second kb-focus-st)))))
  (values (make-hash-table) :type hash-table)
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
  (on-copy nil :type (or null (function (string) (values &optional))))
  (on-paste nil :type (or null (function () (values (or null string) &optional))))
  (key-to-character nil :type (or null (function (t) (values (or null character) &optional))))
  (key-to-mod nil :type (or null (function (t) (values key-modifier &optional))))
  (key-to-action nil :type (or null (function (t) (values (or null key-action) &optional))))
  (fonts (make-hash-table) :type hash-table)
  (yoga-config (yogalayout:config-new) :type %yogalayout:config-ref :read-only t)
  (mouse-owner nil :type (or null widget))
  (cursor :default :type cursor)
  (cursor-layer nil :type (or null layer))
  (cursor-x 0.0 :type single-float)
  (cursor-y 0.0 :type single-float)
  (focused-window nil :type (or null window))
  (get-keyboard-focus (unreachable) :type (function () (values (or null widget) &optional)) :read-only t)
  (set-keyboard-focus (unreachable) :type (function ((or null widget)) (values &optional)) :read-only t)
  (sdet-context (unreachable) :type sdet:context :read-only t)
  (temp-parent nil :type (or null widget))
  (temp-index 0 :type fixnum)
  (temp-yoga-index 0 :type fixnum)
  (temp-root nil :type (or null widget))
  (temp-layer nil :type (or null layer))
  (blend2d-rect-pool (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (blend2d-point-pool (make-array 0 :adjustable t :fill-pointer 0) :type vector)
  (drag-value nil)
  (drag-owner nil :type (or null widget))
  (drag-on-drop nil :type (or null (function () (values &optional)))))

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
  (ftype (function (ui) (values sdet:context &optional)) sdet-context))
(defun sdet-context (ui)
  (ui-sdet-context ui))

(export 'get-font)
(declaim
  (inline get-font)
  (ftype (function (ui symbol) (values (or null %blend2d:font-face-core) boolean &optional)) get-font))
(defun get-font (ui key)
  (gethash key (ui-fonts ui)))

(export 'set-font)
(declaim
  (inline set-font)
  (ftype (function (ui symbol %blend2d:font-face-core) (values &optional)) set-font))
(defun set-font (ui key font-face)
  (setf (gethash key (ui-fonts ui)) font-face)
  (values))

(export 'remove-font)
(declaim
  (inline remove-font)
  (ftype (function (ui symbol) (values %blend2d:font-face-core boolean &optional)) remove-font))
(defun remove-font (ui key)
  (multiple-value-bind (font present)
      (gethash key (ui-fonts ui))
    (remhash key (ui-fonts ui))
    (values font present)))

(export 'map-fonts)
(declaim
  (inline map-fonts)
  (ftype (function (ui (function (symbol %blend2d:font-face-core) (values &optional))) (values &optional)) map-fonts))
(defun map-fonts (ui f)
  (maphash f (ui-fonts ui))
  (values))

(export 'clear-fonts)
(declaim
  (inline clear-fonts)
  (ftype (function (ui) (values &optional)) clear-fonts))
(defun clear-fonts (ui)
  (clrhash (ui-fonts ui))
  (values))

(export 'set-text-input-started-handler)
(defun set-text-input-started-handler (ui text-input-started-handler)
  (setf (ui-on-text-input-started ui) text-input-started-handler)
  (values))

(export 'set-text-input-finished-handler)
(defun set-text-input-finished-handler (ui text-input-finished-handler)
  (setf (ui-on-text-input-finished ui) text-input-finished-handler)
  (values))

(export 'set-copy-handler)
(defun set-copy-handler (ui copy-handler)
  (setf (ui-on-copy ui) copy-handler)
  (values))

(export 'set-paste-handler)
(defun set-paste-handler (ui paste-handler)
  (setf (ui-on-paste ui) paste-handler)
  (values))

(export 'on-copy)
(defun on-copy (ui value)
  (when (ui-on-copy ui)
        (funcall (ui-on-copy ui) value))
  (values))

(export 'on-paste)
(defun on-paste (ui)
  (when (ui-on-paste ui)
        (funcall (ui-on-paste ui))))

(export 'insert-window)
(defun insert-window (ui window)
  (multiple-value-bind (windows present)
      (gethash (window-layer window) (ui-windows ui))
    (unless present
      (setf windows
        (setf (gethash (window-layer window) (ui-windows ui))
          (make-array 0 :adjustable t :fill-pointer 0))))
    (let ((inserted nil)
          (z-index (window-z-index window)))
      (loop #:for index #:below (length windows)
            #:do
            (when (< z-index (window-z-index (aref windows index)))
                  (vector-insert windows window index)
                  (setf inserted t)
                  (return)))
      (unless inserted
        (vector-push-extend window windows))))
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
  (when (window-layer window)
        (delete-window ui window))
  (setf (window-layer window) layer)
  (when layer
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

(export 'cursor)
(declaim
  (inline cursor)
  (ftype (function (ui) (values cursor &optional)) cursor))
(defun cursor (ui)
  (ui-cursor ui))

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

(export 'current-layer)
(declaim
  (inline current-layer)
  (ftype (function (ui) (values (or null layer) &optional)) current-layer))
(defun current-layer (ui)
  (ui-temp-layer ui))

(export 'focused-window)
(declaim
  (inline focused-window)
  (ftype (function (ui) (values (or null window) &optional)) focused-window))
(defun focused-window (ui)
  (ui-focused-window ui))

(defmacro define-pool-functions (type-name pool-accessor alloc-form (freed-sym free-form))
  (let ((alloc-fn-sym (intern (concatenate 'string "ALLOC-" type-name)))
        (free-fn-sym (intern (concatenate 'string "FREE-" type-name))))
    `(progn
      (export ',alloc-fn-sym)
      (defun ,alloc-fn-sym (ui)
        (if (= 0 (length (,pool-accessor ui)))
            ,alloc-form
            (progn
             (decf (fill-pointer (,pool-accessor ui)))
             (aref (,pool-accessor ui) (fill-pointer (,pool-accessor ui))))))

      (export ',free-fn-sym)
      (defun ,free-fn-sym (ui x)
        (vector-push-extend x (,pool-accessor ui)))

      (defun ,(intern (concatenate 'string "CLEAR-" type-name "-POOL")) (ui)
        (loop #:for ,freed-sym #:across (,pool-accessor ui)
              #:do ,free-form)
        (setf (fill-pointer (,pool-accessor ui)) 0)))))

(define-pool-functions "BLEND2D-RECT" ui-blend2d-rect-pool (autowrap:alloc '%blend2d:rect) (x (autowrap:free x)))
(define-pool-functions "BLEND2D-POINT" ui-blend2d-point-pool (autowrap:alloc '%blend2d:point) (x (autowrap:free x)))

(export 'start-dragging)
(defun start-dragging (ui value on-drop)
  (assert (and (null (ui-drag-value ui))
               (null (ui-drag-owner ui))))
  (setf (ui-drag-value ui) value)
  (setf (ui-drag-on-drop ui) on-drop)
  (values))

(export 'stop-dragging)
(defun stop-dragging (ui)
  (let* ((owner (ui-drag-owner ui))
         (value-taken (and owner
                           (widget-on-drag-drop owner)
                           (funcall (widget-on-drag-drop owner) ui owner))))
    (when (and value-taken (ui-drag-on-drop ui))
          (funcall (ui-drag-on-drop ui)))
    (setf (ui-drag-value ui) nil)
    (setf (ui-drag-on-drop ui) nil)
    value-taken))

(export 'dragged-value)
(defun dragged-value (ui)
  (ui-drag-value ui))

(export '*ui*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ui*
                nil
                "A variable used by widgets and their argument expressions to access the library context.
                Stores a form.
                Provided by MACROEXPAND-WITH-UI, for example in COMPOSE and property expansions."))

(export 'macroexpand-with-ui*)
(defmacro macroexpand-with-ui* (ui &body body)
  (alexandria:with-gensyms (previous-ui)
    `(let ((,previous-ui *ui*))
       (setf *ui* (or ,ui ,previous-ui))
       (multiple-value-prog1
           (trivial-macroexpand-all:macroexpand-all (progn ,@body))
         (setf *ui* ,previous-ui)))))

(export 'macroexpand-with-ui)
(defmacro macroexpand-with-ui (ui &body body)
  (let ((previous-ui *ui*))
    (setf *ui* (or ui previous-ui))
    (multiple-value-prog1
        (trivial-macroexpand-all:macroexpand-all `(progn ,@body))
      (setf *ui* previous-ui))))

(export 'get-value)
(declaim
  (inline get-value)
  (ftype (function (ui symbol) (values t boolean &optional)) get-value))
(defun get-value (ui key)
  "Finds a value in the context's hash-table.
  Returns the value or NIL and a boolean indicating whether the value was found."
  (multiple-value-bind (value present-p)
      (gethash key (ui-values ui))
    (values value (not (null present-p)))))

(export 'get-value!)
(declaim
  (inline get-value!)
  (ftype (function (ui symbol) (values t &optional)) get-value!))
(defun get-value! (ui key)
  "Finds a value in the context's hash-table.
  Invokes ERROR if the key wasn't found."
  (multiple-value-bind (value present-p)
      (gethash key (ui-values ui))
    (if present-p
        value
        (error "Context doesn't provide a value with key '~a'" key))))

(export 'with-values)
;;; TODO: use UNWIND-PROTECT
(defmacro with-values (ui key-value-pairs &body body)
  (labels ((rec (ctx-sym kvps body)
                (if (null kvps)
                    `(progn ,@body)
                    (let ((key (caar kvps))
                          (value (cadar kvps)))
                      (alexandria:with-gensyms (key-sym old-value old-value-p)
                        `(let ((,key-sym ,key))
                           (multiple-value-bind (,old-value ,old-value-p)
                               (get ,ctx-sym ,key-sym)
                             (alexandria:multiple-value-prog2
                               (setf (gethash ,key-sym (ui-values ,ctx-sym)) ,value)
                               ,(rec ctx-sym (cdr kvps) body)
                               (if ,old-value-p
                                   (setf (gethash ,key-sym (ui-values ,ctx-sym)) ,old-value)
                                   (remhash ,key-sym (ui-values ,ctx-sym)))))))))))
    (let ((ctx-sym (gensym)))
      `(let ((,ctx-sym ,ui))
         ,(rec ctx-sym key-value-pairs body)))))

(export 'set-value)
(declaim
  (inline set-value)
  (ftype (function (ui symbol t) (values t &optional)) set-value))
(defun set-value (ui key value)
  (setf (gethash key (ui-values ui)) value))

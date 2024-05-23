(in-package #:kslgui)

(export '(window create-window window-layer))
(defstruct (window (:copier nil)
                   (:constructor create-window (layer width height)))
  (layer (unreachable) :type (or null layer))
  (sdet-root (sdet:make-root) :type sdet:root :read-only t)
  (widget nil :type (or null widget))
  (x 0.0d0 :type double-float)
  (y 0.0d0 :type double-float)
  (height 0.0d0 :type double-float)
  (width 0.0d0 :type double-float)
  (on-key-action nil :type (or null (function (ui window t key-action) (values boolean &optional))))
  (on-key-down nil :type (or null (function (ui window t) (values &optional))))
  (on-key-up nil :type (or null (function (ui window t) (values &optional))))
  (on-text-input nil :type (or null (function (ui window string) (values &optional))))
  (on-copy nil :type (or null (function (ui window) (values (or null string) &optional))))
  (on-paste nil :type (or null (function (ui window string) (values &optional))))
  (on-focus-recieved nil :type (or null (function (ui window) (values &optional))))
  (on-focus-lost nil :type (or null (function (ui window) (values &optional))))
  (on-render-begin nil :type (or null (function (ui window) (values &optional))))
  (on-render-end nil :type (or null (function (ui window) (values &optional)))))

(export 'destroy-window)
(declaim (ftype (function (window) (values &optional)) destroy-window))
(defun destroy-window (window)
  (sdet:clean-root (window-sdet-root window))
  (values))

(export 'set-window-layout)
(declaim (ftype (function (window double-float double-float double-float double-float) (values &optional)) set-window-layout))
(defun set-window-layout (window x y width height)
  (setf (window-x window) x)
  (setf (window-y window) y)
  (setf (window-width window) width)
  (setf (window-height window) height)
  (values))

(export 'set-window-key-action-handler)
(declaim (ftype (function (window (or null (function (ui window t key-action) (values boolean &optional)))) (values &optional)) set-window-key-action-handler))
(defun set-window-key-action-handler (window on-key-action)
  (setf (window-on-key-action window) on-key-action)
  (values))

(export 'set-window-key-down-handler)
(declaim (ftype (function (window (or null (function (ui window t) (values &optional)))) (values &optional)) set-window-key-down-handler))
(defun set-window-key-down-handler (window on-key-down)
  (setf (window-on-key-down window) on-key-down)
  (values))

(export 'set-window-key-up-handler)
(declaim (ftype (function (window (or null (function (ui window) (values &optional)))) (values &optional)) set-window-key-up-handler))
(defun set-window-key-up-handler (window on-key-up)
  (setf (window-on-key-up window) on-key-up)
  (values))

(export 'set-window-focus-recieved-handler)
(declaim (ftype (function (window (or null (function (ui window t) (values &optional)))) (values &optional)) set-window-focus-recieved-handler))
(defun set-window-focus-recieved-handler (window on-focus-recieved)
  (setf (window-on-focus-recieved window) on-focus-recieved)
  (values))

(export 'set-window-focus-lost-handler)
(declaim (ftype (function (window (or null (function (ui window) (values &optional)))) (values &optional)) set-window-focus-lost-handler))
(defun set-window-focus-lost-handler (window on-focus-lost)
  (setf (window-on-focus-lost window) on-focus-lost)
  (values))

(export 'set-window-render-begin-handler)
(declaim (ftype (function (window (or null (function (ui window) (values &optional)))) (values &optional)) set-window-render-begin-handler))
(defun set-window-render-begin-handler (window on-render-begin)
  (setf (window-on-render-begin window) on-render-begin)
  (values))

(export 'set-window-render-end-handler)
(declaim (ftype (function (window (or null (function (ui window) (values &optional)))) (values &optional)) set-window-render-end-handler))
(defun set-window-render-end-handler (window on-render-end)
  (setf (window-on-render-end window) on-render-end)
  (values))

(export 'compose)
(defmacro compose (ui window widget-tree)
  (alexandria:once-only (ui window)
    (macroexpand-with-ui* ui
      `(progn
        (sdet:clean-root (window-sdet-root ,window))
        (sdet:with-root (ui-sdet-context ,ui) (window-sdet-root ,window)
          (sdet:make-effect (ui-sdet-context ,ui) ,widget-tree))
        (if (ui-temp-root ,*ui*)
            (setf (window-widget ,window) (ui-temp-root ,*ui*))
            (error "UI composition didn't produce any widgets."))
        (values)))))

(declaim (ftype (function (ui widget) (values (or null layer) &optional)) widget-layer))
(defun widget-layer (ui widget)
  (loop #:while (widget-parent widget)
        #:do (setf widget (the 'widget (widget-parent widget))))
  (maphash (lambda (layer windows)
             (loop #:for window #:across windows
                   #:do (if (eq widget (window-widget window))
                            (return-from widget-layer layer))))
           (ui-windows ui))
  nil)

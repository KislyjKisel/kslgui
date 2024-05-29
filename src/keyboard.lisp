(in-package #:kslgui)

(export 'set-keyboard-focus)
(defun set-keyboard-focus (ui &optional widget)
  (check-type widget (or null widget))
  (let ((previous-widget (sdet:unobserved (ui-sdet-context ui) (funcall (ui-get-keyboard-focus ui)))))
    (when (and previous-widget (widget-on-focus-lost previous-widget))
          (funcall (widget-on-focus-lost previous-widget) ui previous-widget)))
  (funcall (ui-set-keyboard-focus ui) widget)
  (when (and widget (widget-on-focus-recieved widget))
        (funcall (widget-on-focus-recieved widget) ui widget))
  (values))

(export 'keyboard-focus)
(defun keyboard-focus (ui)
  "Returns widget that is currently in focus (target for keyboard events).
  Reactive. May return NIL."
  (funcall (ui-get-keyboard-focus ui)))

(defun scroll-after-focus-changed (ui action new-focus)
  (loop #:with parent = (widget-parent new-focus)
        #:while parent
        #:do
        (when (widget-scroll-on-focus-changed parent)
              (funcall (widget-scroll-on-focus-changed parent)
                ui parent action
                (widget-layer-x new-focus)
                (widget-layer-y new-focus)
                (widget-width new-focus)
                (widget-height new-focus)))
        (setf parent (widget-parent parent))))

(export 'emit-text-input)
(declaim (ftype (function (ui string &key (:window (or null window))) (values &optional)) emit-text-input))
(defun emit-text-input (ui text &key window)
  (or
   (let ((kb-focus (sdet:unobserved (ui-sdet-context ui) (keyboard-focus ui))))
     (when (and kb-focus (widget-on-text-input kb-focus))
           (setf (layer-dirty (widget-layer ui kb-focus)) t)
           (funcall (widget-on-text-input kb-focus) ui kb-focus text)))
   (let ((win-focus (or window (ui-focused-window ui))))
     (when win-focus
           (setf (layer-dirty (window-layer win-focus)) t)
           nil ; todo: key events without focus
           )))
  (values))

(export 'emit-key-down)
(declaim (ftype (function (ui t &key (:window (or null window))) (values &optional)) emit-key-down))
(defun emit-key-down (ui key &key window)
  (let ((kb-focus (sdet:unobserved (ui-sdet-context ui) (keyboard-focus ui)))
        (win-focus (or window (ui-focused-window ui)))
        (action (and (ui-key-to-action ui) (funcall (ui-key-to-action ui) key)))
        (keymod (or (and (ui-key-to-mod ui) (funcall (ui-key-to-mod ui) key)) 0)))
    (if action
        (or (when (and kb-focus (widget-on-key-action kb-focus))
                  (setf (layer-dirty (widget-layer ui kb-focus)) t)
                  (funcall (widget-on-key-action kb-focus) ui kb-focus key keymod action))
            (when win-focus
                  (setf (layer-dirty (window-layer win-focus)) t)
                  nil ; todo: key events without focus
                  )
            (cond
             ((and kb-focus (or (eq action :up)
                                (eq action :down)
                                (eq action :left)
                                (eq action :right)))
               (setf (layer-dirty (widget-layer ui kb-focus)) t)
               (let ((new-focus (directional-navigation ui kb-focus action)))
                 (when new-focus
                       (scroll-after-focus-changed ui action new-focus)
                       (set-keyboard-focus ui new-focus))))))
        (or
         (when (and kb-focus (widget-on-key-down kb-focus))
               (setf (layer-dirty (widget-layer ui kb-focus)) t)
               (funcall (widget-on-key-down kb-focus) ui kb-focus key keymod))
         (when win-focus
               (setf (layer-dirty (window-layer win-focus)) t)
               nil ; todo: key events without focus
               ))))
  (values))

(export 'emit-key-up)
(declaim (ftype (function (ui t &key (:window (or null window))) (values &optional)) emit-key-up))
(defun emit-key-up (ui key &key window)
  (let ((keymod (or (and (ui-key-to-mod ui) (funcall (ui-key-to-mod ui) key)) 0)))
    (or
     (let ((kb-focus (sdet:unobserved (ui-sdet-context ui) (keyboard-focus ui))))
       (when (and kb-focus (widget-on-key-up kb-focus))
             (setf (layer-dirty (widget-layer ui kb-focus)) t)
             (funcall (widget-on-key-up kb-focus) ui kb-focus key keymod)))
     (let ((win-focus (or window (ui-focused-window ui))))
       (when win-focus
             (setf (layer-dirty (window-layer win-focus)) t)
             nil ; todo: key events without focus
             ))))
  (values))

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
(declaim (ftype (function (ui string) (values &optional)) emit-text-input))
(defun emit-text-input (ui text)
  (or
   (let ((kb-focus (sdet:unobserved (ui-sdet-context ui) (keyboard-focus ui))))
     (and kb-focus
          (widget-on-text-input kb-focus)
          (funcall (widget-on-text-input kb-focus) ui kb-focus text)))
   (let ((win-focus (ui-focused-window ui)))
     (and win-focus
          (window-on-text-input win-focus)
          (funcall (window-on-text-input win-focus) ui win-focus text))))
  (values))

(export 'emit-copy)
(declaim (ftype (function (ui) (values (or null string) &optional)) emit-copy))
(defun emit-copy (ui)
  (or
   (let ((kb-focus (sdet:unobserved (ui-sdet-context ui) (keyboard-focus ui))))
     (and kb-focus
          (widget-on-copy kb-focus)
          (funcall (widget-on-copy kb-focus) ui kb-focus)))
   (let ((win-focus (ui-focused-window ui)))
     (and win-focus
          (window-on-text-input win-focus)
          (funcall (window-on-copy win-focus) ui win-focus)))))

(export 'emit-paste)
(declaim (ftype (function (ui string) (values &optional)) emit-paste))
(defun emit-paste (ui text)
  (or
   (let ((kb-focus (sdet:unobserved (ui-sdet-context ui) (keyboard-focus ui))))
     (and kb-focus
          (widget-on-paste kb-focus)
          (funcall (widget-on-paste kb-focus) ui kb-focus text)))
   (let ((win-focus (ui-focused-window ui)))
     (and win-focus
          (window-on-paste win-focus)
          (funcall (window-on-paste win-focus) ui win-focus text))))
  (values))

(export 'emit-key-down)
(declaim (ftype (function (ui t) (values &optional)) emit-key-down))
(defun emit-key-down (ui key)
  (let ((kb-focus (sdet:unobserved (ui-sdet-context ui) (keyboard-focus ui)))
        (win-focus (ui-focused-window ui))
        (action (and (ui-key-to-action ui) (funcall (ui-key-to-action ui) key))))
    (if action
        (or (and kb-focus
                 (widget-on-key-action kb-focus)
                 (funcall (widget-on-key-action kb-focus) ui kb-focus key action))
            (and win-focus
                 (window-on-key-action win-focus)
                 (funcall (window-on-key-action win-focus) ui win-focus key action))
            (cond
             ((and kb-focus (or (eq action :up)
                                (eq action :down)
                                (eq action :left)
                                (eq action :right)))
               (let ((new-focus (directional-navigation ui kb-focus action)))
                 (when new-focus
                       (scroll-after-focus-changed ui action new-focus)
                       (set-keyboard-focus ui new-focus))))
             (t (and (ui-generic-action-handler ui) (funcall (ui-generic-action-handler ui) key action)))))
        (or
         (and kb-focus
              (widget-on-key-down kb-focus)
              (funcall (widget-on-key-down kb-focus) ui kb-focus key))
         (and win-focus
              (window-on-key-down win-focus)
              (funcall (window-on-key-down win-focus) ui win-focus key)))))
  (values))

(export 'emit-key-up)
(declaim (ftype (function (ui t) (values &optional)) emit-key-up))
(defun emit-key-up (ui key)
  (or
   (let ((kb-focus (sdet:unobserved (ui-sdet-context ui) (keyboard-focus ui))))
     (and kb-focus
          (widget-on-key-up kb-focus)
          (funcall (widget-on-key-up kb-focus) ui kb-focus key)))
   (let ((win-focus (ui-focused-window ui)))
     (when (and win-focus (window-on-key-up win-focus))
           (funcall (window-on-key-up win-focus) ui win-focus key))))
  (values))

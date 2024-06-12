(in-package #:kslgui)

(export 'placeholder)
(defstruct (placeholder (:copier nil)
                        (:predicate nil)
                        (:constructor make-placeholder ()))
  (parent nil :type (or null widget))
  (index 0 :type fixnum)
  (yoga-index 0 :type fixnum))

(defun widget-print-function (w s d)
  (declare (ignore d))
  #+sbcl (format s "#<widget ~x>" (sb-kernel:get-lisp-obj-address w))
  #-sbcl (format s "#<widget>"))

(export '(widget
          make-widget widgetp
          widget-parent widget-yoga-node
          widget-yoga-x widget-yoga-y widget-layer-x widget-layer-y widget-width widget-height
          hitp on-render-begin on-render-end on-layout-changed
          on-mouse-enter on-mouse-leave on-mouse-click-left on-mouse-click-middle on-mouse-click-right
          on-mouse-move on-mouse-scroll on-mouse-down-left on-mouse-down-middle on-mouse-down-right
          on-mouse-up-left on-mouse-up-middle on-mouse-up-right on-mouse-ownership-lost cursor
          on-key-action on-key-down on-key-up on-focus-recieved on-focus-lost on-text-input
          on-drag-enter on-drag-move on-drag-leave on-drag-drop
          widget-hitp widget-on-render-begin widget-on-render-end widget-on-layout-changed
          widget-on-mouse-enter widget-on-mouse-leave widget-on-mouse-click-left
          widget-on-mouse-click-middle widget-on-mouse-click-right
          widget-on-mouse-move widget-on-mouse-scroll widget-on-mouse-down-left
          widget-on-mouse-down-middle widget-on-mouse-down-right widget-on-mouse-up-left
          widget-on-mouse-up-middle widget-on-mouse-up-right widget-on-mouse-ownership-lost widget-cursor
          widget-on-key-action widget-on-key-down widget-on-key-up widget-on-focus-recieved
          widget-on-focus-lost widget-on-text-input
          widget-on-drag-enter widget-on-drag-move widget-on-drag-leave widget-on-drag-drop
          widget-get-mouse-hover widget-get-mouse-active widget-enabled-computed))
(defstruct (widget (:include placeholder)
                   (:copier nil)
                   (:predicate widgetp)
                   (:print-function widget-print-function))
  (children (make-array 0 :fill-pointer 0 :adjustable t) :type (vector placeholder))
  (yoga-node nil :type (or null yogalayout:node-ref))
  (yoga-x 0.0 :type single-float)
  (yoga-y 0.0 :type single-float)
  (layer-x 0.0 :type single-float)
  (layer-y 0.0 :type single-float)
  (width 0.0 :type single-float)
  (height 0.0 :type single-float)
  (notify-layout-changed)
  (subscribe-layout-changed)
  (children-scroll-x 0.0 :type single-float)
  (children-scroll-y 0.0 :type single-float)
  (children-mouse-events-clip nil :type (or null aabb))
  (force-isolated-rendering nil :type boolean)
  (rendering-order-cache nil :type (or null (vector widget)))
  (rendering-order-cache-dirty t :type boolean)
  (position-type-computed nil)
  (z-index-computed nil)
  (z-index :auto)
  (on-render-begin nil :type (or null (function (ui widget) (values &optional))))
  (on-render-end nil :type (or null (function (ui widget) (values &optional))))
  (on-layout-changed nil :type (or null (function (ui widget) (values &optional))))
  (hitp nil :type (or null (function (ui widget single-float single-float) (values boolean &optional)))) ; nil = test if mouse is inside yoga layout rectangle
  (get-mouse-hover nil) ; todo: lazy init? (note: may be used without mouse enter/leave handlers present)
  (set-mouse-hover nil)
  (get-mouse-active nil) ; bitmask
  (set-mouse-active nil)
  (on-mouse-enter nil :type (or null (function (ui widget single-float single-float t) (values &optional))))
  (on-mouse-leave nil :type (or null (function (ui widget single-float single-float t) (values &optional))))
  (on-mouse-click-left nil :type (or null (function (ui widget single-float single-float t) (values boolean &optional))))
  (on-mouse-click-middle nil :type (or null (function (ui widget single-float single-float t) (values boolean &optional))))
  (on-mouse-click-right nil :type (or null (function (ui widget single-float single-float t) (values boolean &optional))))
  (on-mouse-move nil :type (or null (function (ui widget single-float single-float t) (values boolean &optional))))
  (on-mouse-scroll nil :type (or null (function (ui widget single-float single-float single-float single-float t) (values boolean &optional))))
  (on-mouse-down-left nil :type (or null (function (ui widget single-float single-float t) (values boolean &optional))))
  (on-mouse-down-middle nil :type (or null (function (ui widget single-float single-float t) (values boolean &optional))))
  (on-mouse-down-right nil :type (or null (function (ui widget single-float single-float t) (values boolean &optional))))
  (on-mouse-up-left nil :type (or null (function (ui widget single-float single-float t) (values boolean &optional))))
  (on-mouse-up-middle nil :type (or null (function (ui widget single-float single-float t) (values boolean &optional))))
  (on-mouse-up-right nil :type (or null (function (ui widget single-float single-float t) (values boolean &optional))))
  (on-mouse-ownership-lost nil :type (or null (function (ui widget) (values &optional))))
  (cursor nil :type (or null cursor (function (ui widget single-float single-float) (or null cursor))))
  (focus-behavior-as-sibling :skip :type (or focus-behavior-as-sibling (function (ui widget) (values focus-behavior-as-sibling &optional))))
  (focus-behavior-as-parent-x :passthrough :type (or focus-behavior-as-parent (function (ui widget key-action) (values (or null focus-behavior-as-parent widget) &optional))))
  (focus-behavior-as-parent-y :passthrough :type (or focus-behavior-as-parent (function (ui widget key-action) (values (or null focus-behavior-as-parent widget) &optional))))
  (scroll-on-focus-changed nil :type (or null (function (ui widget key-action single-float single-float single-float single-float) (values &optional))))
  (on-key-action nil :type (or null (function (ui widget key-action key-modifier t) (values boolean &optional))))
  (on-key-down nil :type (or null (function (ui widget (or nil character) key-modifier t) (values boolean &optional))))
  (on-key-up nil :type (or null (function (ui widget (or nil character) key-modifier t) (values boolean &optional))))
  (on-focus-recieved nil :type (or null (function (ui widget) (values &optional))))
  (on-focus-lost nil :type (or null (function (ui widget) (values &optional))))
  (on-text-input nil :type (or null (function (ui widget string) (values boolean &optional))))
  (on-drag-enter nil :type (or null (function (ui widget x y) (values boolean &optional))))
  (on-drag-move nil :type (or null (function (ui widget x y) (values boolean &optional))))
  (on-drag-leave nil :type (or null (function (ui widget) (values &optional))))
  (on-drag-drop nil :type (or null (function (ui widget) (values boolean &optional))))
  (enabled-computed nil))

(defun compute-widget-coordinates (ui widget yoga-offset-x yoga-offset-y layer-offset-x layer-offset-y)
  (let* ((yoga-node (widget-yoga-node widget))
         (width (yogalayout:node-layout-get-width yoga-node))
         (height (yogalayout:node-layout-get-height yoga-node))
         (yoga-x (+ yoga-offset-x (yogalayout:node-layout-get-left yoga-node)))
         (yoga-y (+ yoga-offset-y (yogalayout:node-layout-get-top yoga-node)))
         (layer-x (+ layer-offset-x yoga-x))
         (layer-y (+ layer-offset-y yoga-y))
         (layout-changed (and (or (/= yoga-x (widget-yoga-x widget))
                                  (/= yoga-y (widget-yoga-y widget))
                                  (/= width (widget-width widget))
                                  (/= height (widget-height widget)))))
         (layer-layout-changed (or layout-changed
                                   (/= layer-x (widget-layer-x widget))
                                   (/= layer-y (widget-layer-y widget)))))
    (setf (widget-width widget) width)
    (setf (widget-height widget) height)
    (setf (widget-yoga-x widget) yoga-x)
    (setf (widget-yoga-y widget) yoga-y)
    (setf (widget-layer-x widget) layer-x)
    (setf (widget-layer-y widget) layer-y)
    (when (and layout-changed (widget-on-layout-changed widget))
          ;; the callback is deferred until all layout fields are updated
          (funcall (widget-on-layout-changed widget) ui widget))
    (when layer-layout-changed
          (funcall (widget-notify-layout-changed widget)))
    (let ((children-layer-offset-x (+ layer-offset-x (widget-children-scroll-x widget)))
          (children-layer-offset-y (+ layer-offset-y (widget-children-scroll-y widget))))
      (loop #:for child #:across (widget-children widget)
            #:do (when (widgetp child)
                       (compute-widget-coordinates ui child
                                                   yoga-x yoga-y
                                                   children-layer-offset-x children-layer-offset-y))))))

(defun widget-rendering-isolated-p (widget)
  (not (eq nil (widget-rendering-order-cache widget))))

(defun widget-plane-layer-x (widget) ;; todo: clarify
  (- (widget-yoga-x widget) (widget-layer-x widget)))

(defun widget-plane-layer-y (widget)
  (- (widget-yoga-y widget) (widget-layer-y widget)))

(export 'widget-window-right)
(defun widget-window-right (widget)
  (+ (widget-yoga-x widget) (widget-width widget)))

(export 'widget-window-bottom)
(defun widget-window-bottom (widget)
  (+ (widget-yoga-y widget) (widget-height widget)))

(export 'observe-layout)
(defun observe-layout (widget)
  (funcall (widget-subscribe-layout-changed widget))
  (values))

(export 'widget-enabled-p)
(declaim (ftype (function (widget) (values boolean &optional)) widget-enabled-p))
(defun widget-enabled-p (widget)
  (sdet:compute (widget-enabled-computed widget)))

(in-package #:kslgui)

(defun widget-children-rendering-order-changed (widget)
  (loop #:until (or (widget-rendering-order-cache widget) (null (widget-parent widget)))
        #:do (setf widget (widget-parent widget)))
  (setf (widget-rendering-order-cache-dirty widget) t))

(defun widget-rendering-order-changed (widget)
  (if (widget-parent widget)
      (widget-children-rendering-order-changed (widget-parent widget))
      (setf (widget-rendering-order-cache-dirty widget) t)))

(defstruct (rendering-order-entry (:copier nil))
  (step 0 :type fixnum)
  (z 0 :type fixnum)
  (tree-index 0 :type fixnum)
  (widget nil :type widget))

(defun collect-widget-rendering-order (ui order tree-index widget)
  (loop #:for child #:across (widget-children widget)
        #:do
        (incf tree-index)
        (multiple-value-bind (z-index position-type)
            (sdet:unobserved (ui-sdet-context ui)
              (values (or (sdet:compute (widget-z-index-computed child)) :auto)
                (or (sdet:compute (widget-position-type-computed child)) yogalayout:+position-type-static+)))
          (cond
           ((and (not (widget-force-isolated-rendering child))
                 (= yogalayout:+position-type-static+ position-type))
             (vector-push-extend (make-rendering-order-entry :step 1
                                                             :z 0
                                                             :tree-index tree-index
                                                             :widget child)
                                 order)
             (setf tree-index (collect-widget-rendering-order ui order tree-index child)))
           ((and (not (widget-force-isolated-rendering child))
                 (eq z-index :auto))
             (vector-push-extend (make-rendering-order-entry :step 3
                                                             :z 0
                                                             :tree-index tree-index
                                                             :widget child)
                                 order)
             (setf tree-index (collect-widget-rendering-order ui order tree-index child)))
           ((or (and (widget-force-isolated-rendering child) (eq z-index :auto))
                (= z-index 0))
             (vector-push-extend (make-rendering-order-entry :step 3
                                                             :z 0
                                                             :tree-index tree-index
                                                             :widget child)
                                 order)
             (update-widget-rendering-order-cache ui child))
           (t (vector-push-extend (make-rendering-order-entry :step (if (< z-index 0) 2 4)
                                                              :z z-index
                                                              :tree-index tree-index
                                                              :widget child)
                                  order)
              (update-widget-rendering-order-cache ui child)))))
  tree-index)

(defun update-widget-rendering-order-cache (ui widget)
  (unless (widget-rendering-order-cache-dirty widget)
    (return-from update-widget-rendering-order-cache))
  (let ((order (make-array 0 :adjustable t :fill-pointer 0)))
    (collect-widget-rendering-order ui order 0 widget)
    (setf order
      (sort order
          (lambda (x y)
            (if (= (rendering-order-entry-step x) (rendering-order-entry-step y))
                (if (= (rendering-order-entry-z x) (rendering-order-entry-z y))
                    (< (rendering-order-entry-tree-index x) (rendering-order-entry-tree-index y))
                    (< (rendering-order-entry-z x) (rendering-order-entry-z y)))
                (< (rendering-order-entry-step x) (rendering-order-entry-step y))))))
    (setf (fill-pointer (widget-rendering-order-cache widget)) 0)
    (loop #:for entry #:across order
          #:do (vector-push-extend (rendering-order-entry-widget entry) (widget-rendering-order-cache widget))))
  (setf (widget-rendering-order-cache-dirty widget) nil)
  (values))

(defun render-widget (ui widget)
  (when (widget-on-render-begin widget)
        (funcall (widget-on-render-begin widget) ui widget))
  (when (widget-rendering-isolated-p widget)
        (update-widget-rendering-order-cache ui widget)
        (loop #:for entry #:across (widget-rendering-order-cache widget)
              #:do (render-widget ui entry)))
  (when (widget-on-render-end widget)
        (funcall (widget-on-render-end widget) ui widget))
  (values))

(declaim (ftype (function (ui window %blend2d:rect %blend2d:point) (values &optional)) render-window))
(defun render-window (ui window clip-rect translate-point)
  (when (not (window-widget window))
        (return-from render-window (values)))
  (yogalayout:node-calculate-layout (widget-yoga-node (window-widget window))
                                    (coerce (window-width window) 'single-float)
                                    (coerce (window-height window) 'single-float)
                                    yogalayout:+direction-ltr+)
  (compute-widget-coordinates ui (window-widget window) 0.0 0.0 (window-x window) (window-y window))
  (when (window-layer window)
        (when (window-on-render-begin window)
              (funcall (window-on-render-begin window) ui window))
        (let ((blend2d-context (layer-context (window-layer window))))
          (%blend2d:context-save blend2d-context (cffi:null-pointer))
          (setf (%blend2d:rect.x clip-rect) (window-x window))
          (setf (%blend2d:rect.y clip-rect) (window-y window))
          (setf (%blend2d:rect.w clip-rect) (window-width window))
          (setf (%blend2d:rect.h clip-rect) (window-height window))
          (%blend2d:context-clip-to-rect-d blend2d-context clip-rect)
          (setf (%blend2d:point.x translate-point) (window-x window))
          (setf (%blend2d:point.y translate-point) (window-y window))
          (%blend2d:context-apply-transform-op blend2d-context
                                               %blend2d:+transform-op-translate+
                                               translate-point)
          (let ((previous-layer (ui-temp-layer ui)))
            (setf (ui-temp-layer ui) (window-layer window))
            (render-widget ui (window-widget window))
            (setf (ui-temp-layer ui) previous-layer))
          (%blend2d:context-restore blend2d-context (cffi:null-pointer)))
        (when (window-on-render-end window)
              (funcall (window-on-render-end window) ui window)))
  (values))

(export 'render)
(declaim (ftype (function (ui &key (:clear boolean) (:defer-flush boolean)) (values &optional)) render))
(defun render (ui &key (clear t) (defer-flush nil))
  (sdet:run-effects (ui-sdet-context ui))
  (let ((clip-rect (alloc-blend2d-rect ui))
        (translate-point (alloc-blend2d-point ui)))
    (maphash (lambda (layer windows)
               (when clear
                     (%blend2d:context-clear-all (layer-context layer)))
               (loop #:for window #:across windows
                     #:do
                     (render-window ui window clip-rect translate-point))
               (when (and (ui-cursor-visible ui)
                          (not (eq :none (ui-cursor ui)))
                          (eq layer (ui-cursor-layer ui)))
                     (render-cursor (ui-cursor-renderer ui)
                                    (layer-context (ui-cursor-layer ui))
                                    (ui-cursor ui)
                                    (coerce (ui-cursor-x ui) 'double-float)
                                    (coerce (ui-cursor-y ui) 'double-float)))
               (unless defer-flush
                 (%blend2d:context-flush (layer-context layer)
                                         (uint32->int32 %blend2d:+context-flush-sync+))))
             (ui-windows ui))
    (when defer-flush
          (alexandria:maphash-keys
            (lambda (layer)
              (%blend2d:context-flush (layer-context layer)
                                      (uint32->int32 %blend2d:+context-flush-sync+)))
            (ui-windows ui)))
    (free-blend2d-point ui translate-point)
    (free-blend2d-rect ui clip-rect))
  (values))

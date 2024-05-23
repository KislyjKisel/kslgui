(in-package #:kslgui)

(defstruct (scroll-widget (:include widget)
                          (:copier nil)
                          (:constructor make-scroll-widget (axis
                                                            &aux
                                                            (force-isolated-rendering t)
                                                            (children-mouse-events-clip (make-aabb)))))
  (axis :x :type (member :x :y)) ; todo: both?
  (scroll-sensitivity-computed)
  (min-bar-width 0.0d0 :type double-float)
  (max-bar-width 0.0d0 :type double-float)
  (bar-width-base 0.0d0 :type double-float)
  (bar-width-scale 0.0d0 :type double-float)
  (min-thumb-width 0.0d0 :type double-float)
  (max-thumb-width 0.0d0 :type double-float)
  (thumb-width-base 0.0d0 :type double-float)
  (thumb-width-scale 0.0d0 :type double-float)
  (min-thumb-length 0.0d0 :type double-float)
  (max-thumb-length 0.0d0 :type double-float)
  (bar-width 0.0d0 :type double-float)
  (thumb-width 0.0d0 :type double-float)
  (thumb-length 0.0d0 :type double-float)
  (background-visual)
  (bar-visual)
  (thumb-visual)
  (thumb-sensor)
  (offset 0.0d0 :type double-float)
  (prev-scroll-length 0.0d0 :type double-float)
  (mouse-origin nil :type (or null single-float))
  (origin-offset 0.0d0 :type double-float)
  (delta-offset 0.0d0 :type double-float))

(declaim (inline scroll-widget-mouse-event-aux))
(defun scroll-widget-mouse-event-aux (ui widget x y)
  (let ((thumb-length (coerce (scroll-widget-thumb-length widget) 'single-float))
        (bar-width (coerce (scroll-widget-bar-width widget) 'single-float))
        (offset (coerce (scroll-widget-offset widget) 'single-float)))
    (ecase (scroll-widget-axis widget)
      (:x (let ((bar-layer-y (+ (widget-layer-y widget) (- (widget-height widget) bar-width)))
                (thumb-layer-x (+ (widget-layer-x widget) (* offset (- (widget-width widget) thumb-length)))))
            (values (hit-sensor-p* ui
                                   (scroll-widget-thumb-sensor widget)
                                   (scroll-widget-thumb-visual widget)
                                   thumb-layer-x bar-layer-y thumb-length bar-width x y)
              (>= y bar-layer-y)
              x
              thumb-layer-x
              (- (widget-width widget) thumb-length))))
      (:y (let ((bar-layer-x (+ (widget-layer-x widget) (- (widget-width widget) bar-width)))
                (thumb-layer-y (+ (widget-layer-y widget) (* offset (- (widget-height widget) thumb-length)))))
            (values (hit-sensor-p* ui
                                   (scroll-widget-thumb-sensor widget)
                                   (scroll-widget-thumb-visual widget)
                                   bar-layer-x thumb-layer-y bar-width thumb-length x y)
              (>= x bar-layer-x)
              y
              thumb-layer-y
              (- (widget-height widget) thumb-length)))))))

(defun scroll-content-max-x (widget max-x)
  (loop #:for child #:across (widget-children widget)
        #:do
        (unless (placeholderp child)
          (setf max-x (max max-x (+ (widget-yoga-x child) (widget-width child))))
          (setf max-x (scroll-content-max-x child max-x))))
  max-x)

(defun scroll-content-max-y (widget max-y)
  (loop #:for child #:across (widget-children widget)
        #:do
        (unless (placeholderp child)
          (setf max-y (max max-y (+ (widget-yoga-y child) (widget-height child))))
          (setf max-y (scroll-content-max-y child max-y))))
  max-y)

(defun w-scroll-impl (ui &key
                         set-layout
                         make-children
                         z-index position-type
                         axis
                         scroll-sensitivity
                         min-bar-width max-bar-width bar-width-base bar-width-scale
                         min-thumb-width max-thumb-width thumb-width-base thumb-width-scale
                         min-thumb-length max-thumb-length
                         focus-behavior-as-sibling
                         focus-behavior-as-parent
                         background-visual
                         bar-visual
                         thumb-visual thumb-sensor)
  (let ((widget (make-scroll-widget axis)))
    (setf (scroll-widget-min-bar-width widget) min-bar-width)
    (setf (scroll-widget-max-bar-width widget) max-bar-width)
    (setf (scroll-widget-bar-width-base widget) bar-width-base)
    (setf (scroll-widget-bar-width-scale widget) bar-width-scale)
    (setf (scroll-widget-min-thumb-width widget) min-thumb-width)
    (setf (scroll-widget-max-thumb-width widget) max-thumb-width)
    (setf (scroll-widget-thumb-width-base widget) thumb-width-base)
    (setf (scroll-widget-thumb-width-scale widget) thumb-width-scale)
    (setf (scroll-widget-min-thumb-length widget) min-thumb-length)
    (setf (scroll-widget-max-thumb-length widget) max-thumb-length)
    (setf (widget-focus-behavior-as-sibling widget) focus-behavior-as-sibling)
    (ecase axis
      (:x (setf (widget-focus-behavior-as-parent-x widget) :contain-if-any)
          (setf (widget-focus-behavior-as-parent-y widget) focus-behavior-as-parent))
      (:y (setf (widget-focus-behavior-as-parent-x widget) focus-behavior-as-parent)
          (setf (widget-focus-behavior-as-parent-y widget) :contain-if-any)))
    (initialize-widget ui widget :z-index z-index :position-type position-type)
    (when set-layout (funcall set-layout widget))
    (yogalayout:node-style-set-overflow (widget-yoga-node widget) yogalayout:+overflow-scroll+)
    (setf (scroll-widget-thumb-sensor widget) (init-computed-prop widget thumb-sensor))
    (run-visual-prop widget background-visual)
    (run-visual-prop widget bar-visual)
    (run-visual-prop widget thumb-visual)
    (setf (scroll-widget-scroll-sensitivity-computed widget)
      (init-computed-prop widget scroll-sensitivity))
    (when make-children (append-children ui widget make-children))
    (setf (widget-cursor widget)
      (lambda (ui widget x y)
        (multiple-value-bind (thumb-p bar-p)
            (scroll-widget-mouse-event-aux ui widget x y)
          (cond
           ((< (scroll-widget-prev-scroll-length widget) 0.5d0) nil)
           ((scroll-widget-mouse-origin widget)
             (ecase (scroll-widget-axis widget) (:x :move-x) (:y :move-y)))
           (thumb-p :grab)
           (bar-p (if (= +active-mouse-button-left+ (funcall (widget-get-mouse-active-p widget)))
                      :release
                      :press))
           (t nil)))))
    (setf (widget-on-mouse-scroll widget)
      (lambda (ui widget x y sx sy other)
        (declare (ignore x y other))
        (if (scroll-widget-mouse-origin widget)
            nil
            (let ((offset (max 0.0d0
                            (min 1.0d0
                              (+ (scroll-widget-offset widget)
                                 (coerce
                                   (* (sdet:unobserved (ui-sdet-context ui)
                                        (sdet:compute (scroll-widget-scroll-sensitivity-computed widget)))
                                      (ecase (scroll-widget-axis widget)
                                        (:x (* sx (ui-scroll-sensitivity-x ui)))
                                        (:y (* sy (ui-scroll-sensitivity-y ui)))))
                                   'double-float))))))
              (when (/= offset (scroll-widget-offset widget))
                    (setf (scroll-widget-offset widget) offset)
                    t)))))
    (setf (widget-on-mouse-click-left widget)
      (lambda (ui widget x y other)
        (declare (ignore other))
        (multiple-value-bind (thumb-p bar-p mouse-pos thumb-pos bar-movable-length)
            (scroll-widget-mouse-event-aux ui widget x y)
          (cond
           (thumb-p t)
           ((not bar-p) nil)
           ((< mouse-pos thumb-pos)
             (setf (scroll-widget-offset widget)
               (max 0.0d0 (- (scroll-widget-offset widget)
                             (/ (scroll-widget-thumb-length widget) bar-movable-length)))))
           ((> mouse-pos thumb-pos)
             (setf (scroll-widget-offset widget)
               (min 1.0d0 (+ (scroll-widget-offset widget)
                             (/ (scroll-widget-thumb-length widget) bar-movable-length)))))
           (t t)))))
    (setf (widget-on-mouse-down-left widget)
      (lambda (ui widget x y other)
        (declare (ignore other))
        (when (scroll-widget-mouse-event-aux ui widget x y)
              (own-mouse ui widget)
              (setf (scroll-widget-mouse-origin widget)
                (ecase (scroll-widget-axis widget) (:x x) (:y y)))
              (setf (scroll-widget-origin-offset widget) (scroll-widget-offset widget))
              (setf (scroll-widget-delta-offset widget) 0.0d0)
              t)))
    (setf (widget-on-mouse-up-left widget)
      (lambda (ui widget x y other)
        (declare (ignore x y other))
        (when (scroll-widget-mouse-origin widget)
              (disown-mouse ui widget)
              (setf (scroll-widget-mouse-origin widget) nil)
              t)))
    (setf (widget-on-mouse-down-right widget)
      ;; Can't test MMB on a laptop (?), but some systems use MMB for this. (todo: make configurable)
      (lambda (ui widget x y other)
        (declare (ignore other))
        (multiple-value-bind (thumb-p bar-p mouse-pos thumb-pos bar-movable-length)
            (scroll-widget-mouse-event-aux ui widget x y)
          (declare (ignore thumb-p thumb-pos))
          (when bar-p
                (own-mouse ui widget)
                ;; Don't clamp offset: it will be clamped either
                ;; after adjusting to new widget/content dimensions or
                ;; when moving mouse while holding RMB.
                ;; Clamping there would also move mouse off thumb center
                ;; even after moving to a point with enough space around.
                (setf (scroll-widget-offset widget)
                  (/ (- mouse-pos
                        (ecase (scroll-widget-axis widget)
                          (:x (scroll-widget-layer-x widget))
                          (:y (scroll-widget-layer-y widget)))
                        (* 0.5d0 (scroll-widget-thumb-length widget)))
                     bar-movable-length))
                (setf (scroll-widget-mouse-origin widget) mouse-pos)
                (setf (scroll-widget-origin-offset widget) (scroll-widget-offset widget))
                (setf (scroll-widget-delta-offset widget) 0.0d0)
                t))))
    (setf (widget-on-mouse-up-right widget)
      (lambda (ui widget x y other)
        (declare (ignore x y other))
        (when (scroll-widget-mouse-origin widget)
              (disown-mouse ui widget)
              (setf (scroll-widget-mouse-origin widget) nil)
              t)))
    (setf (widget-on-mouse-move widget)
      (lambda (ui widget x y other)
        (declare (ignore ui other))
        (when (scroll-widget-mouse-origin widget)
              (multiple-value-bind (scroll-size mouse-current)
                  (ecase (scroll-widget-axis widget)
                    (:x (values (widget-width widget) x))
                    (:y (values (widget-height widget) y)))
                (setf (scroll-widget-delta-offset widget)
                  (/ (- mouse-current (scroll-widget-mouse-origin widget))
                     (- scroll-size (scroll-widget-thumb-length widget))))
                (setf (scroll-widget-offset widget)
                  (max 0.0d0 (min 1.0d0 (+ (scroll-widget-origin-offset widget)
                                           (scroll-widget-delta-offset widget))))))
              t)))
    (setf (widget-scroll-on-focus-changed widget)
      (lambda (ui widget action x y w h)
        (declare (ignore ui))
        (ecase (scroll-widget-axis widget)
          (:x (case action
                (:left
                 (let ((dx (- (widget-layer-x widget) x)))
                   (when (< 0.0f0 dx)
                         (setf (scroll-widget-offset widget)
                           (max 0.0d0 (- (scroll-widget-offset widget) (/ (coerce dx 'double-float)
                                                                          (scroll-widget-prev-scroll-length widget))))))))
                (:right
                 (let ((dx (- (+ x w) (+ (widget-layer-x widget) (widget-width widget)))))
                   (when (< 0.0f0 dx)
                         (setf (scroll-widget-offset widget)
                           (min 1.0d0 (+ (scroll-widget-offset widget) (/ (coerce dx 'double-float)
                                                                          (scroll-widget-prev-scroll-length widget))))))))))
          (:y (case action
                (:up
                 (let ((dy (- (widget-layer-y widget) y)))
                   (when (< 0.0f0 dy)
                         (setf (scroll-widget-offset widget)
                           (max 0.0d0 (- (scroll-widget-offset widget) (/ (coerce dy 'double-float)
                                                                          (scroll-widget-prev-scroll-length widget))))))))
                (:down
                 (let ((dy (- (+ y h) (+ (widget-layer-y widget) (widget-height widget)))))
                   (when (< 0.0f0 dy)
                         (setf (scroll-widget-offset widget)
                           (min 1.0d0 (+ (scroll-widget-offset widget) (/ (coerce dy 'double-float)
                                                                          (scroll-widget-prev-scroll-length widget)))))))))))
        (values)))
    (setf (widget-on-render-begin widget)
      (lambda (ui widget)
        (render-visual ui
                       (scroll-widget-background-visual widget)
                       (coerce (widget-yoga-x widget) 'double-float)
                       (coerce (widget-yoga-y widget) 'double-float)
                       (coerce (widget-width widget) 'double-float)
                       (coerce (widget-height widget) 'double-float))
        (multiple-value-bind (widget-size-main widget-size-side)
            (ecase (scroll-widget-axis widget)
              (:x (values
                    (coerce (widget-width widget) 'double-float)
                    (coerce (widget-height widget) 'double-float)))
              (:y (values
                    (coerce (widget-height widget) 'double-float)
                    (coerce (widget-width widget) 'double-float))))
          (setf (scroll-widget-bar-width widget)
            (max (scroll-widget-min-bar-width widget)
              (min (scroll-widget-max-bar-width widget)
                (+ (scroll-widget-bar-width-base widget)
                   (* (scroll-widget-bar-width-scale widget) widget-size-side)))))
          (setf (scroll-widget-thumb-width widget)
            (max (scroll-widget-min-thumb-width widget)
              (min (scroll-widget-max-thumb-width widget)
                (+ (scroll-widget-thumb-width-base widget)
                   (* (scroll-widget-thumb-width-scale widget) widget-size-side)))))
          (let* ((content-size-main (coerce
                                      (ecase (scroll-widget-axis widget)
                                        (:x (- (scroll-content-max-x widget 0.0) (widget-yoga-x widget)))
                                        (:y (- (scroll-content-max-y widget 0.0) (widget-yoga-y widget))))
                                      'double-float))
                 (scroll-length (- content-size-main widget-size-main))
                 (offset (alloc-blend2d-point ui))
                 (clip (alloc-blend2d-rect ui))
                 ;; Don't move view in case content/thumb length changed
                 (adjusted-offset (if (< scroll-length 0.5)
                                      0.0d0
                                      (max 0.0d0
                                        (min 1.0d0
                                          (* (scroll-widget-offset widget)
                                             (/ (scroll-widget-prev-scroll-length widget) scroll-length))))))
                 (offset-length (fround (- (* scroll-length adjusted-offset)))))

            (setf (scroll-widget-thumb-length widget)
              (max (scroll-widget-min-thumb-length widget)
                (min (scroll-widget-max-thumb-length widget) widget-size-main
                  (* widget-size-main (/ widget-size-main content-size-main)))))

            (setf (scroll-widget-prev-scroll-length widget) scroll-length)
            (setf (scroll-widget-offset widget) adjusted-offset)
            (%blend2d:context-save (layer-context (ui-temp-layer ui)) (cffi:null-pointer)) ; TODO: why double-save ?
            (setf (aabb-min-x (widget-children-mouse-events-clip widget)) (widget-layer-x widget))
            (setf (aabb-min-y (widget-children-mouse-events-clip widget)) (widget-layer-y widget))
            (setf (aabb-max-x (widget-children-mouse-events-clip widget)) (+ (widget-layer-x widget) (widget-width widget)))
            (setf (aabb-max-y (widget-children-mouse-events-clip widget)) (+ (widget-layer-y widget) (widget-height widget)))
            (setf (%blend2d:rect.x clip) (coerce (widget-yoga-x widget) 'double-float))
            (setf (%blend2d:rect.y clip) (coerce (widget-yoga-y widget) 'double-float))
            (setf (%blend2d:rect.w clip) (coerce (widget-width widget) 'double-float))
            (setf (%blend2d:rect.h clip) (coerce (widget-height widget) 'double-float))
            (%blend2d:context-clip-to-rect-d (layer-context (ui-temp-layer ui)) clip)
            (ecase (scroll-widget-axis widget)
              (:x (setf (%blend2d:point.x offset) offset-length)
                  (setf (%blend2d:point.y offset) 0.0d0)
                  (setf (widget-children-scroll-x widget) (coerce offset-length 'single-float)))
              (:y (setf (%blend2d:point.x offset) 0.0d0)
                  (setf (%blend2d:point.y offset) offset-length)
                  (setf (widget-children-scroll-y widget) (coerce offset-length 'single-float))))
            (%blend2d:context-apply-transform-op (layer-context (ui-temp-layer ui))
                                                 %blend2d:+transform-op-translate+
                                                 offset)
            (%blend2d:context-save (layer-context (ui-temp-layer ui)) (cffi:null-pointer)) ; TODO: why double-save ?
            (free-blend2d-rect ui clip)
            (free-blend2d-point ui offset)))
        (values)))
    (setf (widget-on-render-end widget)
      (lambda (ui widget)
        (%blend2d:context-restore (layer-context (ui-temp-layer ui)) (cffi:null-pointer))
        (%blend2d:context-restore (layer-context (ui-temp-layer ui)) (cffi:null-pointer))
        (when (>= (scroll-widget-prev-scroll-length widget) 0.5d0)
              (let ((x (coerce (widget-yoga-x widget) 'double-float))
                    (y (coerce (widget-yoga-y widget) 'double-float))
                    (width (coerce (widget-width widget) 'double-float))
                    (height (coerce (widget-height widget) 'double-float))
                    (offset (scroll-widget-offset widget))
                    (bar-width (scroll-widget-bar-width widget))
                    (thumb-width (scroll-widget-thumb-width widget))
                    (thumb-length (scroll-widget-thumb-length widget))
                    (thumb-bar-offset (* 0.5d0 (- (scroll-widget-bar-width widget)
                                                  (scroll-widget-thumb-width widget))))
                    (bar-visual (scroll-widget-bar-visual widget))
                    (thumb-visual (scroll-widget-thumb-visual widget)))
                (ecase (scroll-widget-axis widget)
                  (:x (let ((thumb-x (+ x (* offset (- width thumb-length))))
                            (bar-y (+ y (- height bar-width))))
                        (render-visual ui bar-visual x bar-y width bar-width)
                        (render-visual ui thumb-visual thumb-x (+ bar-y thumb-bar-offset) thumb-length thumb-width)))
                  (:y (let ((thumb-y (+ y (* offset (- height thumb-length))))
                            (bar-x (+ x (- width bar-width))))
                        (render-visual ui bar-visual bar-x y bar-width height)
                        (render-visual ui thumb-visual (+ bar-x thumb-bar-offset) thumb-y thumb-width thumb-length))))))
        (values)))
    (lambda ()
      (destroy-visual (scroll-widget-background-visual widget))
      (destroy-visual (scroll-widget-bar-visual widget))
      (destroy-visual (scroll-widget-thumb-visual widget))
      (destroy-widget widget)
      (values))))

(export 'w-scroll)
(defmacro w-scroll (layout (&key ui let z-index position-type
                                 (axis :y)
                                 (scroll-sensitivity 1.0)
                                 (min-bar-width 0.0d0) (max-bar-width 1.0d9) (bar-width-base 4.0d0) (bar-width-scale 0.03d0)
                                 (min-thumb-width 0.0d0) (max-thumb-width 1.0d9) (thumb-width-base 4.0d0) (thumb-width-scale 0.03d0)
                                 (min-thumb-length 1.0d0) (max-thumb-length 1.0d9)
                                 (focus-behavior-as-sibling :to-children)
                                 (focus-behavior-as-parent :passthrough)
                                 background-visual
                                 bar-visual
                                 thumb-visual (thumb-sensor :visual))
                           &body children)
  (macroexpand-with-ui* ui
    `(w-scroll-impl ,*ui*
                    :set-layout ,(make-layout-setting-lambda *ui* layout)
                    :make-children ,(make-children-making-lambda *ui* children :let let)
                    :z-index ,(make-computed-prop z-index :let let)
                    :position-type ,(make-computed-prop position-type :let let)
                    :axis ,axis
                    :scroll-sensitivity ,(make-computed-prop scroll-sensitivity :let let)
                    :min-bar-width ,min-bar-width
                    :max-bar-width ,max-bar-width
                    :bar-width-base ,bar-width-base
                    :bar-width-scale ,bar-width-scale
                    :min-thumb-width ,min-thumb-width
                    :max-thumb-width ,max-thumb-width
                    :thumb-width-base ,thumb-width-base
                    :thumb-width-scale ,thumb-width-scale
                    :min-thumb-length ,min-thumb-length
                    :max-thumb-length ,max-thumb-length
                    :focus-behavior-as-sibling ,focus-behavior-as-sibling
                    :focus-behavior-as-parent ,focus-behavior-as-parent
                    :background-visual ,(make-visual-prop *ui* background-visual :let let :widget-visual 'scroll-widget-background-visual)
                    :bar-visual ,(make-visual-prop *ui* bar-visual :let let :widget-visual 'scroll-widget-bar-visual)
                    :thumb-visual ,(make-visual-prop *ui* thumb-visual :let let :widget-visual 'scroll-widget-thumb-visual)
                    :thumb-sensor ,(make-computed-prop thumb-sensor :let let))))

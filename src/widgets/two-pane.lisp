(in-package #:kslgui)

(defstruct (two-pane-separator-widget (:include widget)
                                      (:copier nil)
                                      (:constructor make-two-pane-separator-widget
                                                    (axis threshold on-threshold-changed
                                                          &aux (hitp #'default-widget-hitp))))
  (axis)
  (threshold)
  (on-threshold-changed)
  (style nil)
  (drag-mouse-start 0.0 :type single-float)
  (drag-threshold-start 0.0 :type single-float)
  (drag-active nil :type boolean))

(defun w-two-pane-separator-impl (ui &key axis thickness style threshold on-threshold-changed)
  (let ((widget (make-two-pane-separator-widget axis
                                                threshold
                                                on-threshold-changed)))
    (initialize-widget ui widget)
    (sdet:on-cleanup (sdet-context ui)
      (destroy-widget widget)
      (values))
    (flet ((set-two-pane-separator-widget-layout
            (widget axis thickness)
            (let ((yoga-node (widget-yoga-node widget)))
              (if (eq :x axis)
                  (progn
                   (yogalayout:node-style-set-width yoga-node (coerce thickness 'single-float))
                   (yogalayout:node-style-set-height-percent yoga-node 100.0))
                  (progn
                   (yogalayout:node-style-set-width-percent yoga-node 100.0)
                   (yogalayout:node-style-set-height yoga-node (coerce thickness 'single-float)))))
            (values)))
      (sdet:make-effect (sdet-context ui)
        (set-two-pane-separator-widget-layout widget
                                              (sdet:compute (two-pane-separator-widget-axis widget))
                                              (sdet:compute thickness))))
    (sdet:make-effect (sdet-context ui)
      (setf (two-pane-separator-widget-style widget)
        (create-style (two-pane-separator-widget-style widget) (sdet:compute style)))
      (values))
    (setf (widget-on-render-begin widget)
      (lambda (ui widget)
        (blend2d-context-set-fill-style (layer-context (ui-temp-layer ui))
                                        (two-pane-separator-widget-style widget))
        (fill-rectangle ui
                        (widget-yoga-x widget)
                        (widget-yoga-y widget)
                        (widget-width widget)
                        (widget-height widget))
        (values)))
    (setf (widget-cursor widget) (if (eq :x (sdet:unobserved (sdet-context ui)
                                              (sdet:compute (two-pane-separator-widget-axis widget))))
                                     :move-x
                                     :move-y))
    (setf (widget-on-mouse-down-left widget)
      (lambda (ui widget x y other)
        (declare (ignore other))
        (own-mouse ui widget)
        (setf (two-pane-separator-widget-drag-active widget) t)
        (multiple-value-bind (axis threshold)
            (sdet:unobserved (sdet-context ui)
              (values
                (sdet:compute (two-pane-separator-widget-axis widget))
                (sdet:compute (two-pane-separator-widget-threshold widget))))
          (setf (two-pane-separator-widget-drag-mouse-start widget)
            (if (eq :x axis) x y))
          (setf (two-pane-separator-widget-drag-threshold-start widget) (coerce threshold 'single-float)))
        (values)))
    (flet ((calc-new-threshold
            (widget x y)
            (let ((axis (sdet:unobserved (sdet-context ui) (sdet:compute (two-pane-separator-widget-axis widget)))))
              (max 0.0 (min 1.0 (+ (two-pane-separator-widget-drag-threshold-start widget)
                                   (/ (- (if (eq :x axis) x y)
                                         (two-pane-separator-widget-drag-mouse-start widget))
                                      (max 1.0
                                        (if (eq :x axis)
                                            (widget-width (widget-parent widget))
                                            (widget-height (widget-parent widget)))))))))))
      (setf (widget-on-mouse-move widget)
        (lambda (ui widget x y other)
          (declare (ignore ui other))
          (let ((on-threshold-changed (two-pane-separator-widget-on-threshold-changed widget)))
            (when (and on-threshold-changed
                       (two-pane-separator-widget-drag-active widget))
                  (funcall on-threshold-changed (calc-new-threshold widget x y))))
          (values)))
      (setf (widget-on-mouse-up-left widget)
        (lambda (ui widget x y other)
          (declare (ignore other))
          (when (two-pane-separator-widget-drag-active widget)
                (disown-mouse ui widget)
                (setf (two-pane-separator-widget-drag-active widget) nil)
                (let ((on-threshold-changed (two-pane-separator-widget-on-threshold-changed widget)))
                  (when on-threshold-changed
                        (funcall on-threshold-changed (calc-new-threshold widget x y)))))
          (values)))
      (setf (widget-on-mouse-ownership-lost widget)
        (lambda (ui widget)
          (declare (ignore ui))
          (setf (two-pane-separator-widget-drag-active widget) nil)
          (values))))
    widget))

(defun w-two-pane-impl (ui &key
                           axis separator-thickness separator-style threshold on-threshold-changed
                           compose-low compose-high set-layout)
  (w-block* 'set-layout (:ui ui :let container)
    (sdet:make-effect (sdet-context ui)
      (let ((axis (sdet:compute axis)))
        (yogalayout:node-style-set-flex-direction (widget-yoga-node container)
                                                  (if (eq :x axis)
                                                      yogalayout:+flex-direction-row+
                                                      yogalayout:+flex-direction-column+))))
    (w-block* () (:ui ui :let low-container)
      (sdet:make-effect (sdet-context ui)
        (let ((axis (sdet:compute axis))
              (low-yoga-node (widget-yoga-node low-container)))
          (yogalayout:node-style-set-flex-basis low-yoga-node 0.0)
          (yogalayout:node-style-set-flex-grow low-yoga-node
                                               (coerce (sdet:compute threshold) 'single-float))
          (if (eq :x axis)
              (yogalayout:node-style-set-height-percent low-yoga-node 100.0)
              (yogalayout:node-style-set-width-percent low-yoga-node 100.0)))
        (values))
      (when compose-low
            (funcall compose-low low-container))
      (values))
    (w-two-pane-separator-impl ui
                               :axis axis
                               :thickness separator-thickness
                               :style separator-style
                               :threshold threshold
                               :on-threshold-changed on-threshold-changed)
    (w-block* () (:ui ui :let high-container)
      (sdet:make-effect (sdet-context ui)
        (let ((axis (sdet:compute axis))
              (high-yoga-node (widget-yoga-node high-container)))
          (yogalayout:node-style-set-flex-basis high-yoga-node 0.0)
          (yogalayout:node-style-set-flex-grow high-yoga-node
                                               (- 1.0 (coerce (sdet:compute threshold) 'single-float)))
          (if (eq :x axis)
              (yogalayout:node-style-set-height-percent high-yoga-node 100.0)
              (yogalayout:node-style-set-width-percent high-yoga-node 100.0)))
        (values))
      (when compose-high
            (funcall compose-high high-container))
      (values))))

(export 'w-two-pane)
(defmacro w-two-pane ((&key
                       (ui nil)
                       (axis :x)
                       (threshold 0.5)
                       (on-threshold-changed nil)
                       (layout ())
                       (separator-thickness 2.0)
                       (separator-style #xFF000000))
                      compose-low
                      compose-high)
  (macroexpand-with-ui* ui
    `(w-two-pane-impl ,*ui*
                      :axis ,(make-computed-prop axis :initialized nil)
                      :threshold ,(make-computed-prop threshold :initialized nil)
                      :on-threshold-changed ,(make-callback-prop on-threshold-changed :parameter-count 1)
                      :set-layout ,(make-layout-setting-lambda *ui* layout)
                      :separator-thickness ,(make-computed-prop separator-thickness :initialized nil)
                      :separator-style ,(make-computed-prop separator-style :initialized nil)
                      :compose-low ,(make-callback-prop compose-low :parameter-count 1)
                      :compose-high ,(make-callback-prop compose-high :parameter-count 1))))

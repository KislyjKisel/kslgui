(in-package #:kslgui)

(defstruct (button-widget (:include widget)
                          (:copier nil)
                          (:constructor make-button-widget (on-click
                                                            &aux
                                                            (focus-behavior-as-sibling :focus)
                                                            (hitp #'button-widget-hitp-impl))))
  (visual nil)
  (sensor)
  (on-click))

(defun button-widget-hitp-impl (ui widget mx my)
  (hit-sensor-p* ui
                 (button-widget-sensor widget)
                 (button-widget-visual widget)
                 (widget-layer-x widget)
                 (widget-layer-y widget)
                 (widget-width widget)
                 (widget-height widget)
                 mx my))

(defun w-button-impl (ui &key
                         set-layout
                         z-index position-type
                         on-click enabled focus
                         visual sensor)
  (let ((widget (make-button-widget on-click)))
    (initialize-widget ui widget :z-index z-index :position-type position-type :enabled enabled)
    (sdet:on-cleanup (sdet-context ui)
      (destroy-visual (button-widget-visual widget))
      (destroy-widget widget)
      (values))
    (when set-layout (funcall set-layout widget))
    (when focus (set-keyboard-focus ui widget))
    (setf (button-widget-sensor widget) (init-computed-prop widget sensor))
    (setf (widget-cursor widget)
      (lambda (ui widget x y)
        (declare (ignore ui x y))
        (if (and (= +active-mouse-button-left+ (funcall (widget-get-mouse-active widget)))
                 (sdet:compute (widget-enabled-computed widget)))
            :release
            :press)))
    (when on-click
          (setf (widget-on-mouse-click-left widget)
            (lambda (ui widget x y other)
              (declare (ignore ui x y other))
              (when (sdet:compute (widget-enabled-computed widget))
                    (funcall (button-widget-on-click widget))
                    t)))
          (setf (widget-on-mouse-down-left widget)
            (lambda (ui widget x y other)
              (declare (ignore x y other))
              (when (sdet:compute (widget-enabled-computed widget))
                    (own-mouse ui widget)
                    (set-keyboard-focus ui widget)
                    t)))
          (setf (widget-on-mouse-up-left widget)
            (lambda (ui widget x y other)
              (declare (ignore x y other))
              (disown-mouse ui widget)
              t))
          (setf (widget-on-key-action widget)
            (lambda (ui widget key mod action)
              (declare (ignore ui key mod))
              (and (eq :enter action)
                   (sdet:compute (widget-enabled-computed widget))
                   (funcall (button-widget-on-click widget))))))
    (run-visual-prop widget visual #'button-widget-visual #'(setf button-widget-visual))
    (setf (widget-on-render-begin widget)
      (lambda (ui widget)
        (render-visual ui
                       (button-widget-visual widget)
                       (coerce (widget-yoga-x widget) 'double-float)
                       (coerce (widget-yoga-y widget) 'double-float)
                       (coerce (widget-width widget) 'double-float)
                       (coerce (widget-height widget) 'double-float))
        (values)))
    (values)))

(export 'w-button)
(defmacro w-button (&key ui layout let z-index position-type
                         on-click (enabled t) focus
                         visual (sensor :visual))
  (macroexpand-with-ui* ui
    `(w-button-impl ,*ui*
                    :set-layout ,(make-layout-setting-lambda *ui* layout)
                    :z-index ,(make-computed-prop z-index :let let)
                    :position-type ,(make-computed-prop position-type :let let)
                    :on-click ,(when on-click `(lambda () ,on-click))
                    :enabled ,(make-computed-prop enabled :let let)
                    :focus ,focus
                    :visual ,(make-visual-prop *ui* visual :let let)
                    :sensor ,(make-computed-prop sensor :let let))))

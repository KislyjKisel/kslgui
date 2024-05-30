(in-package #:kslgui)

(defstruct (visual-widget (:include widget)
                          (:copier nil)
                          (:constructor make-visual-widget (focus-behavior-as-sibling
                                                            focus-behavior-as-parent-x
                                                            focus-behavior-as-parent-y)))
  (visual nil))

(defun w-visual-impl (ui &key
                         set-layout
                         make-children
                         z-index position-type
                         focus-behavior-as-sibling
                         focus-behavior-as-parent-x
                         focus-behavior-as-parent-y
                         visual)
  (let ((widget (make-visual-widget focus-behavior-as-sibling
                                    focus-behavior-as-parent-x
                                    focus-behavior-as-parent-y)))
    (initialize-widget ui widget :z-index z-index :position-type position-type)
    (run-visual-prop widget visual #'visual-widget-visual #'(setf visual-widget-visual))
    (when set-layout (funcall set-layout widget))
    (when make-children (append-children ui widget make-children))
    (setf (widget-on-render-begin widget)
      (lambda (ui widget)
        (render-visual ui
                       (visual-widget-visual widget)
                       (coerce (widget-yoga-x widget) 'double-float)
                       (coerce (widget-yoga-y widget) 'double-float)
                       (coerce (widget-width widget) 'double-float)
                       (coerce (widget-height widget) 'double-float))
        (values)))
    (lambda ()
      (destroy-visual (visual-widget-visual widget))
      (destroy-widget widget)
      (values))))

(export 'w-visual)
(defmacro w-visual (visual (&key ui layout let z-index position-type
                                 (focus-behavior-as-sibling :to-children)
                                 (focus-behavior-as-parent-x :passthrough)
                                 (focus-behavior-as-parent-y :passthrough))
                           &body children)
  (macroexpand-with-ui* ui
    `(w-visual-impl ,*ui*
                    :set-layout ,(make-layout-setting-lambda *ui* layout)
                    :make-children ,(make-children-making-lambda *ui* children :let let)
                    :z-index ,(make-computed-prop z-index :let let)
                    :position-type ,(make-computed-prop position-type :let let)
                    :focus-behavior-as-sibling ,focus-behavior-as-sibling
                    :focus-behavior-as-parent-x ,focus-behavior-as-parent-x
                    :focus-behavior-as-parent-y ,focus-behavior-as-parent-y
                    :visual ,(make-visual-prop *ui* visual :let let))))

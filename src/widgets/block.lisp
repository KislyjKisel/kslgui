(in-package #:kslgui)

(defun w-block-impl (ui &key
                        set-layout
                        make-children
                        z-index position-type
                        focus-behavior-as-sibling
                        focus-behavior-as-parent-x
                        focus-behavior-as-parent-y)
  (let ((widget (make-widget :focus-behavior-as-sibling focus-behavior-as-sibling
                             :focus-behavior-as-parent-x focus-behavior-as-parent-x
                             :focus-behavior-as-parent-y focus-behavior-as-parent-y)))
    (initialize-widget ui widget :z-index z-index :position-type position-type)
    (when set-layout (funcall set-layout widget))
    (when make-children (append-children ui widget make-children))
    (lambda ()
      (dispose-widget widget)
      (values))))

(defmacro w-block* (layout (&key ui let z-index position-type
                                 (focus-behavior-as-sibling :to-children)
                                 (focus-behavior-as-parent-x :passthrough)
                                 (focus-behavior-as-parent-y :passthrough))
                           &body children)
  (macroexpand-with-ui ui
  `(w-block-impl ,*ui*
                 :set-layout ,(make-layout-setting-lambda *ui* layout)
                 :make-children ,(make-children-making-lambda *ui* children :let let)
                 :z-index ,(make-computed-prop z-index :let let)
                 :position-type ,(make-computed-prop position-type :let let)
                 :focus-behavior-as-sibling ,focus-behavior-as-sibling
                 :focus-behavior-as-parent-x ,focus-behavior-as-parent-x
                 :focus-behavior-as-parent-y ,focus-behavior-as-parent-y)))

(defmacro w-block (layout &body children)
  `(w-block* ,layout () ,@children))

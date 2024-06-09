(in-package #:kslgui)

(export '*textbox-cursor-blink-period*)
(defvar *textbox-cursor-blink-period* 1.5d0)

(defstruct (textbox-widget (:include widget)
                           (:copier nil)
                           (:constructor make-textbox-widget
                                         (label on-changed on-enter
                                                &aux
                                                (hitp #'default-widget-hitp)
                                                (focus-behavior-as-sibling :focus)
                                                (cursor :text))))
  (label)
  (on-changed)
  (on-enter)
  (background-visual nil)
  (cursor-visual nil)
  (cursor-index 0 :type fixnum)
  (cursor-x 0.0d0 :type double-float)
  (cursor-h 0.0d0 :type double-float)
  (padding-left)
  (padding-right)
  (padding-top)
  (padding-bottom))

(defun textbox-widget-clamped-cursor-index (widget)
  (max 0
    (min (length (label-text (textbox-widget-label widget)))
      (textbox-widget-cursor-index widget))))

(defun textbox-widget-update-cursor-layout (widget)
  (let ((label (textbox-widget-label widget))
        (glyph-buffer (autowrap:alloc '%blend2d:glyph-buffer-core))
        (text-metrics (autowrap:alloc '%blend2d:text-metrics)))
    (%blend2d:glyph-buffer-init glyph-buffer)
    (measure-text (label-font-cache label)
                  glyph-buffer
                  text-metrics
                  (subseq (label-text label)
                          0
                          (textbox-widget-clamped-cursor-index widget)))
    (setf (textbox-widget-cursor-x widget)
      (+ (coerce (sdet:compute (textbox-widget-padding-left widget)) 'double-float)
         (%blend2d:text-metrics.advance.x text-metrics)))))

(defun w-textbox-impl (ui &key
                          set-layout
                          z-index position-type enabled focus
                          text font font-size text-style
                          on-changed on-enter
                          background-visual
                          cursor-visual
                          padding-left padding-right padding-top padding-bottom)
  (let ((widget (make-textbox-widget (make-label*) on-changed on-enter)))
    (initialize-widget ui widget :z-index z-index :position-type position-type :enabled enabled)
    (sdet:on-cleanup (sdet-context ui)
      (destroy-label (textbox-widget-label widget))
      (destroy-visual (textbox-widget-background-visual widget))
      (destroy-visual (textbox-widget-cursor-visual widget))
      (destroy-widget widget)
      (values))
    (setf (textbox-widget-padding-left widget) (init-computed-prop widget padding-left))
    (setf (textbox-widget-padding-right widget) (init-computed-prop widget padding-right))
    (setf (textbox-widget-padding-top widget) (init-computed-prop widget padding-top))
    (setf (textbox-widget-padding-bottom widget) (init-computed-prop widget padding-bottom))
    (when set-layout (funcall set-layout widget))
    (when focus (set-keyboard-focus ui widget))
    (run-visual-prop widget background-visual #'textbox-widget-background-visual #'(setf textbox-widget-background-visual))
    (run-visual-prop widget cursor-visual #'textbox-widget-cursor-visual #'(setf textbox-widget-cursor-visual))
    (initialize-label ui
                      (textbox-widget-label widget)
                      :widget widget
                      :mark-dirty nil
                      :text text
                      :text-style text-style
                      :font font
                      :font-size font-size
                      :align-horz :start
                      :align-vert :center
                      :wrap :no)
    (let ((font-metrics (font-cache-metrics (label-font-cache (textbox-widget-label widget)))))
      (setf (textbox-widget-cursor-h widget)
        (coerce (- (%blend2d:font-metrics.y-max font-metrics) (%blend2d:font-metrics.y-min font-metrics))
                'double-float)))
    (setf (widget-on-mouse-down-left widget)
      (lambda (ui widget x y other)
        (declare (ignore x y other))
        (when (sdet:compute (widget-enabled-computed widget))
              (set-keyboard-focus ui widget))))
    (let ((text-input-callback
           (lambda (ui widget input-text)
             (declare (ignore ui))
             (let* ((text (label-text (textbox-widget-label widget)))
                    (cursor-index (textbox-widget-clamped-cursor-index widget))
                    (new-text (concatenate 'string
                                (subseq text 0 cursor-index)
                                input-text
                                (subseq text cursor-index))))
               (when (textbox-widget-on-changed widget) (funcall (textbox-widget-on-changed widget) new-text))
               (setf (textbox-widget-cursor-index widget)
                 (+ (textbox-widget-cursor-index widget)
                    (length input-text))))
             t)))
      (setf (widget-on-text-input widget) text-input-callback)
      (setf (widget-on-key-action widget)
        (lambda (ui widget key mod action)
          (declare (ignore key mod))
          (when (sdet:compute (widget-enabled-computed widget))
                (case action
                  (:backspace
                   (when (and (textbox-widget-on-changed widget) (> (textbox-widget-cursor-index widget) 0))
                         (decf (textbox-widget-cursor-index widget))
                         (let ((text (label-text (textbox-widget-label widget)))
                               (cursor-index (textbox-widget-clamped-cursor-index widget)))
                           (funcall (textbox-widget-on-changed widget) (concatenate 'string
                                                                         (subseq text 0 cursor-index)
                                                                         (subseq text (min (length text) (1+ cursor-index)))))))
                   t)
                  (:delete
                   (let ((text (label-text (textbox-widget-label widget)))
                         (cursor-index (textbox-widget-clamped-cursor-index widget)))
                     (when (and (textbox-widget-on-changed widget) (< (textbox-widget-cursor-index widget) (length text)))
                           (funcall (textbox-widget-on-changed widget) (concatenate 'string
                                                                         (subseq text 0 cursor-index)
                                                                         (subseq text (min (length text) (1+ cursor-index)))))))
                   t)
                  (:left
                   (setf (textbox-widget-cursor-index widget)
                     (max 0 (1- (textbox-widget-clamped-cursor-index widget))))
                   t)
                  (:right
                   (setf (textbox-widget-cursor-index widget)
                     (min (length (label-text (textbox-widget-label widget)))
                       (1+ (textbox-widget-clamped-cursor-index widget))))
                   t)
                  (:enter (when (textbox-widget-on-enter widget)
                                (funcall (textbox-widget-on-enter widget))
                                t))
                  (:copy (on-copy ui (label-text (textbox-widget-label widget))))
                  (:paste (let ((value (on-paste ui)))
                            (when value
                                  (funcall text-input-callback ui widget value))))
                  (t nil))))))
    (setf (widget-on-focus-recieved widget)
      (lambda (ui widget)
        (declare (ignore widget))
        (when (ui-on-text-input-started ui)
              (funcall (ui-on-text-input-started ui)))
        (values)))
    (setf (widget-on-focus-lost widget)
      (lambda (ui widget)
        (declare (ignore widget))
        (when (ui-on-text-input-finished ui)
              (funcall (ui-on-text-input-finished ui)))
        (values)))
    (setf (widget-on-render-begin widget)
      (lambda (ui widget)
        (render-visual ui (textbox-widget-background-visual widget)
                       (coerce (widget-yoga-x widget) 'double-float) ; todo... + initialize and COMPUTE! and put into structure
                       (coerce (widget-yoga-y widget) 'double-float)
                       (coerce (widget-width widget) 'double-float)
                       (coerce (widget-height widget) 'double-float))
        (render-label ui
                      (coerce (+ (sdet:compute (textbox-widget-padding-left widget))
                                 (widget-yoga-x widget))
                              'double-float)
                      (coerce (+ (sdet:compute (textbox-widget-padding-top widget))
                                 (widget-yoga-y widget))
                              'double-float)
                      (- (widget-width widget)
                         (coerce (sdet:compute (textbox-widget-padding-right widget)) 'single-float))
                      (- (widget-height widget)
                         (coerce (sdet:compute (textbox-widget-padding-bottom widget)) 'single-float))
                      (textbox-widget-label widget))
        (when (and (eq widget (sdet:unobserved (ui-sdet-context ui) (funcall (ui-get-keyboard-focus ui))))
                   ;  (< (* 0.5d0 *textbox-cursor-blink-period*)
                   ;     (rem (ui-time ui) *textbox-cursor-blink-period*))
                   )
              (textbox-widget-update-cursor-layout widget)
              (render-visual ui (textbox-widget-cursor-visual widget)
                             (+ (coerce (widget-yoga-x widget) 'double-float)
                                (textbox-widget-cursor-x widget))
                             (+ (coerce (widget-yoga-y widget) 'double-float)
                                (* 0.5d0 (- (coerce (widget-height widget) 'double-float) (textbox-widget-cursor-h widget))))
                             2.0d0
                             (textbox-widget-cursor-h widget)))))
    (values)))

(export 'w-textbox)
(defmacro w-textbox (&key ui layout let z-index position-type
                          on-enter on-changed (enabled t) focus
                          background-visual cursor-visual
                          text font font-size text-style
                          (padding 0.0d0)
                          (padding-left nil)
                          (padding-right nil)
                          (padding-top nil)
                          (padding-bottom nil))
  (macroexpand-with-ui* ui
    `(w-textbox-impl ,*ui*
                     :set-layout ,(make-layout-setting-lambda *ui* layout)
                     :z-index ,(make-computed-prop z-index :let let)
                     :position-type ,(make-computed-prop position-type :let let)
                     :on-enter ,(when on-enter `(lambda () ,on-enter))
                     :on-changed ,(when on-changed `(lambda (,(car on-changed)) ,@(cdr on-changed)))
                     :enabled ,(make-computed-prop enabled :let let)
                     :focus ,focus
                     :text ,(make-computed-prop text :let let)
                     :font ,(make-computed-prop font :let let)
                     :text-style ,(make-computed-prop text-style :let let)
                     :font-size ,(make-computed-prop font-size :let let)
                     :background-visual ,(make-visual-prop *ui* background-visual :let let)
                     :cursor-visual ,(make-visual-prop *ui* cursor-visual :let let)
                     :padding-left ,(make-computed-prop (or padding-left padding) :let let)
                     :padding-right ,(make-computed-prop (or padding-right padding) :let let)
                     :padding-top ,(make-computed-prop (or padding-top padding) :let let)
                     :padding-bottom ,(make-computed-prop (or padding-bottom padding) :let let))))

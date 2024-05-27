(in-package #:kslgui)

(declaim
  (inline insert-placeholder-impl)
  (ftype (function (ui placeholder) (values &optional)) insert-placeholder-impl))
(defun insert-placeholder-impl (ui placeholder)
  (let ((parent (ui-temp-parent ui))
        (index (ui-temp-index ui))
        (yoga-index (ui-temp-yoga-index ui)))
    (unless parent
      (error "Cannot insert placeholder: current parent is NIL"))
    (setf (placeholder-parent placeholder) parent)
    (setf (placeholder-index placeholder) index)
    (setf (placeholder-yoga-index placeholder) yoga-index)
    (let ((delta-yoga-index (if (widgetp placeholder) 1 0)))
      (dotimes (offset (- (length (widget-children parent)) index))
        (let ((child (aref (widget-children parent) (+ index offset))))
          (incf (placeholder-index child))
          (incf (placeholder-yoga-index child) delta-yoga-index))))
    (vector-insert (widget-children parent) placeholder index)
    (incf (ui-temp-index ui))
    (when (widgetp placeholder)
          (incf (ui-temp-yoga-index ui))))
  (values))

(declaim
  (inline delete-placeholder-impl)
  (ftype (function (placeholder) (values &optional)) delete-placeholder-impl))
(defun delete-placeholder-impl (placeholder)
  (let ((parent (placeholder-parent placeholder)))
    (setf (widget-children parent)
      (vector-delete placeholder (widget-children parent) :start (placeholder-index placeholder)))
    (let ((delta-yoga-index (if (widgetp placeholder) 1 0)))
      (dotimes (offset (- (length (widget-children parent)) (placeholder-index placeholder)))
        (let ((child (aref (widget-children parent) (+ (placeholder-index placeholder) offset))))
          (decf (placeholder-index child))
          (decf (placeholder-yoga-index child) delta-yoga-index)))))
  (values))

(export 'insert-placeholder)
(declaim (ftype (function (ui) (values placeholder &optional)) insert-placeholder))
(defun insert-placeholder (ui)
  (let ((placeholder (make-placeholder)))
    (insert-placeholder-impl ui placeholder)
    placeholder))

(export 'delete-placeholder)
(declaim (ftype (function (placeholder) (values &optional)) delete-placeholder))
(defun delete-placeholder (placeholder)
  (delete-placeholder-impl placeholder)
  (values))

(export 'with-composition-after-placeholder)
(defmacro with-composition-after-placeholder (ui placeholder &body body)
  (alexandria:once-only (ui placeholder)
    (alexandria:with-gensyms (previous-parent previous-index previous-yoga-index initial-new-index initial-new-yoga-index)
      `(let ((,previous-parent (ui-temp-parent ,ui))
             (,previous-index (ui-temp-index ,ui))
             (,previous-yoga-index (ui-temp-yoga-index ,ui))
             (,initial-new-index (1+ (placeholder-index ,placeholder)))
             (,initial-new-yoga-index (+ (placeholder-yoga-index ,placeholder)
                                         (if (widgetp ,placeholder) 1 0))))
         (setf (ui-temp-index ,ui) ,initial-new-index)
         (setf (ui-temp-yoga-index ,ui) ,initial-new-yoga-index)
         (setf (ui-temp-parent ,ui) (placeholder-parent ,placeholder))
         (unwind-protect
             (progn
              ,@body)
           (if (and (eq ,previous-parent (placeholder-parent ,placeholder))
                    (<= ,initial-new-index ,previous-index))
               (progn
                (setf (ui-temp-index ,ui) (+ ,previous-index (- (ui-temp-index ,ui) ,initial-new-index)))
                (setf (ui-temp-yoga-index ,ui) (+ ,previous-yoga-index (- (ui-temp-yoga-index ,ui) ,initial-new-yoga-index))))
               (progn
                (setf (ui-temp-index ,ui) ,previous-index)
                (setf (ui-temp-yoga-index ,ui) ,previous-yoga-index)
                (setf (ui-temp-parent ,ui) ,previous-parent))))))))

(export 'initialize-widget)
(declaim (ftype (function (ui widget &key (:z-index t)
                              (:position-type t)
                              (:enabled (or boolean function)))
                          (values &optional))
                initialize-widget))
(defun initialize-widget (ui widget &key z-index position-type (enabled t))
  (let* ((parent (ui-temp-parent ui)))
    (setf (widget-yoga-node widget) (yogalayout:node-new-with-config (ui-yoga-config ui)))
    (setf (widget-position-type-computed widget) (init-computed-prop widget position-type))
    (setf (widget-z-index-computed widget) (init-computed-prop widget z-index))
    (setf (widget-enabled-computed widget) (init-computed-prop widget enabled))
    (if parent
        (progn
         (insert-placeholder-impl ui widget)
         (yogalayout:node-insert-child (widget-yoga-node parent)
                                       (widget-yoga-node widget)
                                       (widget-yoga-index widget))
         (widget-children-rendering-order-changed parent))
        (if (ui-temp-root ui)
            (error "Root already set while creating new parentless widget.")
            (setf (ui-temp-root ui) widget)))
    (sdet:make-effect (ui-sdet-context ui)
      (let ((z-index (or (sdet:compute (widget-z-index-computed widget)) :auto))
            (position-type (or (sdet:compute (widget-position-type-computed widget))
                               yogalayout:+position-type-static+)))
        (setf (widget-z-index widget) z-index)
        (yogalayout:node-style-set-position-type (widget-yoga-node widget) position-type)
        (if (and
             (widget-parent widget)
             (not (widget-force-isolated-rendering widget))
             (or (= position-type yogalayout:+position-type-static+) (eq :auto z-index)))
            (setf (widget-rendering-order-cache widget) nil)
            (setf (widget-rendering-order-cache widget) (make-array 0 :adjustable t :fill-pointer 0)))
        (widget-rendering-order-changed widget))
      nil)
    (multiple-value-bind (subscribe notify) (sdet:make-notifier (ui-sdet-context ui))
      (setf (widget-subscribe-layout-changed widget) subscribe)
      (setf (widget-notify-layout-changed widget) notify))
    (multiple-value-bind (get set) (sdet:make-state (ui-sdet-context ui) nil)
      (setf (widget-get-mouse-hover-p widget) get)
      (setf (widget-set-mouse-hover-p widget) set))
    (multiple-value-bind (get set) (sdet:make-state (ui-sdet-context ui) (the (unsigned-byte 3) 0))
      (setf (widget-get-mouse-active-p widget) get)
      (setf (widget-set-mouse-active-p widget) set)))
  (values))

(export 'destroy-widget)
(defun destroy-widget (widget)
  (let ((parent (widget-parent widget)))
    (when parent
          (delete-placeholder-impl widget)
          (yogalayout:node-remove-child (widget-yoga-node parent) (widget-yoga-node widget))
          (widget-children-rendering-order-changed parent)))
  (yogalayout:node-reset (widget-yoga-node widget))
  (yogalayout:node-free (widget-yoga-node widget)))

(export 'append-children)
(defun append-children (ui widget f)
  (let ((parent (ui-temp-parent ui))
        (previous-index (ui-temp-index ui))
        (previous-yoga-index (ui-temp-yoga-index ui))
        (initial-children-count (length (widget-children widget))))
    (setf (ui-temp-index ui) initial-children-count)
    (setf (ui-temp-yoga-index ui) (if (< 0 initial-children-count)
                                      (let ((last-child (aref (widget-children widget) (1- initial-children-count))))
                                        (+ (placeholder-yoga-index last-child)
                                           (if (widgetp last-child) 1 0)))
                                      0))
    (setf (ui-temp-parent ui) widget)
    (funcall f widget)
    (setf (ui-temp-parent ui) parent)
    (setf (ui-temp-index ui) previous-index)
    (setf (ui-temp-yoga-index ui) previous-yoga-index))
  (values))

(alexandria:define-constant +yoga-style-prop-setter-prefix+ "NODE-STYLE-SET-" :test #'string=)

(export 'make-layout-setting-code)
(defun make-layout-setting-code (ui layout widget-sym)
  `(sdet:make-effect (ui-sdet-context ,ui)
     ,@(mapcar (lambda (stp)
                 (let ((st-set-sym (find-symbol (uiop:strcat +yoga-style-prop-setter-prefix+ (symbol-name (first stp))) '#:yogalayout)))
                   (if st-set-sym
                       `(,st-set-sym (widget-yoga-node ,widget-sym) ,@(rest stp))
                       (error "Unknown Yoga layout setter: ~a." (first stp)))))
           layout)))

(export 'make-layout-setting-lambda)
(defun make-layout-setting-lambda (ui layout)
  (cond
   ((null layout) 'nil)
   ((and (consp layout) (eq 'quote (first layout))) (second layout))
   (t
     (alexandria:with-gensyms (widget)
       `(lambda (,widget)
          ,(make-layout-setting-code ui layout widget))))))

(export 'make-children-making-lambda)
(defun make-children-making-lambda (ui children &key let)
  (if children
      (let ((widget (or let (gensym))))
        `(lambda (,widget)
           (declare (ignorable ,widget))
           ,@(mapcar (lambda (child)
                       (cond
                        ((and (consp child) (eq 'quote (first child))) (second child))
                        (t `(sdet:make-effect (ui-sdet-context ,ui) ,child)))
                       children))))
      'nil))

(defun wsd.test (ui tags widget-sym form)
  (cond
   ((consp form)
     (if (eq 'quote (car form))
         (cadr form)
         (cons (car form) (mapcar (lambda (sub-form)
                                    (wsd.test ui tags widget-sym sub-form)) (cdr form)))))
   ((keywordp form)
     (multiple-value-bind (tag-form tag-known-p) (if tags (gethash form tags) (values nil nil))
       (if tag-known-p
           tag-form
           (case form
             (:hover `(funcall (widget-get-mouse-hover-p ,widget-sym)))
             (:active `(< 0 (funcall (widget-get-mouse-active-p ,widget-sym))))
             (:active-left `(< 0 (logand +active-mouse-button-left+ (funcall (widget-get-mouse-active-p ,widget-sym)))))
             (:active-middle `(< 0 (logand +active-mouse-button-middle+ (funcall (widget-get-mouse-active-p ,widget-sym)))))
             (:active-right `(< 0 (logand +active-mouse-button-right+ (funcall (widget-get-mouse-active-p ,widget-sym)))))
             (:focus `(eq ,widget-sym (keyboard-focus ,ui)))
             (:enabled `(sdet:compute (widget-enabled-computed ,widget-sym)))
             (:disabled `(not (sdet:compute (widget-enabled-computed ,widget-sym))))
             (t (error "WSD: unknown keyword ~a" form))))))
   ((eq 't form) 't)
   (t (error "WSD: can't parse test form ~a" form))))

(export 'wsd)
(defmacro wsd (widget &body branches)
  "COND with conditional keywords :HOVER, :ENABLED etc.
  Conditions include quoting, functions (with arguments processed recursively) and T: (AND :HOVER T '(< 1 2))"
  `(cond ,@(mapcar (lambda (branch)
                     `(,(wsd.test *ui* nil widget (car branch))
                        ,@(cdr branch)))
                   branches)))

(export 'make-computed-prop)
(defun make-computed-prop (val &key let (initialized t))
  (assert (or initialized (null let)))
  (cond
   ((and (consp val) (eq 'quote (first val))) (second val))
   ((constantp val) val)
   ((not initialized) `(lambda () ,val))
   (t (let ((widget-sym (or let (gensym))))
        `(lambda (,widget-sym)
           (declare (ignorable ,widget-sym))
           (lambda () ,val))))))

(export 'init-computed-prop)
(defun init-computed-prop (widget x)
  (if (typep x 'function)
      (funcall x widget)
      x))

(export 'make-visual-prop)
(defun make-visual-prop (ui vdescr &key let widget-visual)
  (let ((widget (or let (gensym)))
        (vdescr-item-computed (gensym)))
    (cond
     ((null vdescr) 'nil)
     ((and (consp vdescr) (eq 'quote (first vdescr))) (second vdescr))
     (t `(lambda (,widget)
           (declare (ignorable ,widget))
           ,(if (= 1 (length vdescr))
                `(let ((,vdescr-item-computed (lambda () ,(first vdescr))))
                   (sdet:make-effect (ui-sdet-context ,ui)
                     (setf (,widget-visual ,widget)
                       (update-visual (sdet:compute ,vdescr-item-computed) (,widget-visual ,widget)))
                     nil))
                (let ((forms '()))
                  (loop #:for index #:from 0
                        #:for vdescr-item #:in vdescr
                        #:do (setf forms (cons
                                           `(let ((,vdescr-item-computed (lambda () ,vdescr-item)))
                                              (sdet:make-effect (ui-sdet-context ,ui)
                                                (setf (aref (,widget-visual ,widget) ,index)
                                                  (update-visual (sdet:compute ,vdescr-item-computed) (aref (,widget-visual ,widget) ,index)))
                                                nil))
                                           forms)))
                  `(progn
                    (setf (,widget-visual ,widget) (make-array ,(length vdescr) :initial-element nil))
                    ,@(reverse forms))))
           (values))))))

(export 'run-visual-prop)
(defun run-visual-prop (widget prop)
  (when prop (funcall prop widget))
  (values))

;;;; Visual elements: objects for presenting UI elements visually

(in-package #:kslgui)

(export 'visual-state)
(defstruct (visual-state (:copier nil))
  (render (error "Default VISUAL-STATE-RENDER used")
          :type (function (ui visual-state double-float double-float double-float double-float) (values &optional)))
  (destroy (error "Default VISUAL-STATE-DESTROY used")
           :type (function (visual-state) (values &optional)))
  (hitp nil :type (or null (function (ui visual-state double-float double-float double-float double-float double-float double-float) (values boolean &optional)))))

;;; User provided description of the visual that can create or update state
(export 'visual-description)
(defstruct (visual-description (:copier nil))
  (update (error "Default VISUAL-DESCRIPTION-UPDATE used")
          :type (function (visual-description (or null visual-state)) (values (or null visual-state) &optional))))

(export 'shape-visual-description)
(defstruct (shape-visual-description (:include visual-description) (:copier nil))
  (fill-style)
  (border-style)
  (border-width nil :type (or null real)))

(export 'shape-visual-state)
(defstruct (shape-visual-state (:include visual-state) (:copier nil))
  (fill-style)
  (border-style)
  (border-width nil :type (or null double-float)))

(export 'define-visual-description-update)
(defmacro define-visual-description-update (update-fn-name (vdescr-sym (vstate-sym vstate-type) &key create-fn) &body body)
  `(defun ,update-fn-name (,vdescr-sym ,vstate-sym)
     (cond
      ((null ,vstate-sym) (,create-fn ,vdescr-sym))
      ((typep ,vstate-sym ',vstate-type) ,@body ,vstate-sym)
      (t (funcall (visual-state-destroy ,vstate-sym) ,vstate-sym)
         (,create-fn ,vdescr-sym)))))

(export 'update-shape-visual-fields)
(declaim (inline update-shape-visual-fields))
(defun update-shape-visual-fields (vdescr vstate)
  (setf (shape-visual-state-fill-style vstate) (create-style (shape-visual-state-fill-style vstate) (shape-visual-description-fill-style vdescr)))
  (setf (shape-visual-state-border-style vstate) (create-style (shape-visual-state-border-style vstate) (shape-visual-description-border-style vdescr)))
  (setf (shape-visual-state-border-width vstate) (when (shape-visual-description-border-width vdescr)
                                                       (coerce (shape-visual-description-border-width vdescr) 'double-float)))
  vstate)

(export 'destroy-shape-visual-fields)
(declaim (inline destroy-shape-visual-fields))
(defun destroy-shape-visual-fields (vstate)
  (destroy-style (shape-visual-state-fill-style vstate))
  (destroy-style (shape-visual-state-border-style vstate))
  (values))

(export 'render-visual)
(declaim
  (inline render-visual)
  (ftype (function (ui (or null visual-state (vector (or null visual-state))) double-float double-float double-float double-float)
                   (values &optional))
         render-visual))
(defun render-visual (ui vstate x y w h)
  (if (arrayp vstate)
      (loop #:for item #:across vstate #:do
            (when item
                  (funcall (visual-state-render item) ui item x y w h)))
      (when vstate
            (funcall (visual-state-render vstate) ui vstate x y w h)))
  (values))

(export 'destroy-visual)
(declaim
  (inline destroy-visual)
  (ftype (function ((or null visual-state (vector (or null visual-state)))) (values &optional)) destroy-visual))
(defun destroy-visual (vstate)
  (if (arrayp vstate)
      (loop #:for item #:across vstate #:do
            (when item
                  (funcall (visual-state-destroy item) item)))
      (when vstate
            (funcall (visual-state-destroy vstate) vstate)))
  (values))

(export 'update-visual)
(declaim
  (inline update-visual)
  (ftype (function ((or null visual-description) (or null visual-state))
                   (values (or null visual-state) &optional))
         update-visual))
(defun update-visual (vdescr vstate)
  (if vdescr
      (funcall (visual-description-update vdescr) vdescr vstate)
      (progn
       (destroy-visual vstate)
       nil)))

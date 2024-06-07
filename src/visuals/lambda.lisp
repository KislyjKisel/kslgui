(in-package #:kslgui)

(defstruct (lambda-visual-description (:include visual-description)
                                      (:constructor v-lambda*
                                                    (on-render &aux (update #'lambda-visual-description-update-impl)))
                                      (:copier nil))
  (on-render (unreachable) :type (function (ui t double-float double-float double-float double-float) (values &optional))))

(export 'v-lambda)
(defmacro v-lambda (ui x y w h &body body)
  (alexandria:with-gensyms (value)
    `(v-lambda* (lambda (,ui ,value ,x ,y ,w ,h)
                  (declare (ignore ,value))
                  ,@body
                  (values)))))

(defstruct (lambda-visual-state (:include visual-state)
                                (:constructor make-lambda-visual-state
                                              (render
                                                &aux
                                                (destroy #'lambda-visual-state-destroy-impl)))
                                (:copier nil)))

(defun lambda-visual-description-create-impl (vdescr)
  (make-lambda-visual-state (lambda-visual-description-on-render vdescr)))

(define-visual-description-update lambda-visual-description-update-impl
    (vdescr (vstate lambda-visual-state) :create-fn lambda-visual-description-create-impl)
  (values))

(defun lambda-visual-state-destroy-impl (vstate)
  (declare (ignore vstate))
  (values))

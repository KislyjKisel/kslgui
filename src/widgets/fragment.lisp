(in-package #:kslgui)

(defmacro w (&body widgets)
  (setf widgets (mapcar (lambda (widget) (list widget (gensym))) widgets))
  `(let ,(mapcar (lambda (widget) `(,(second widget) ,(first widget))) widgets)
     (lambda ()
       ,@(mapcar (lambda (widget) `(funcall ,(second widget))) widgets)
       (values))))

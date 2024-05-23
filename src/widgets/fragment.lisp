(in-package #:kslgui)

(export 'w)
(defmacro w (&body widgets)
  (when (null widgets)
        (return-from w 'nil))
  (setf widgets (mapcar (lambda (widget) (list widget (gensym))) widgets))
  `(let ,(mapcar (lambda (widget) `(,(second widget) ,(first widget))) widgets)
     (lambda ()
       ,@(mapcar (lambda (widget) `(funcall ,(second widget))) widgets)
       (values))))

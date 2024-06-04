(in-package #:kslgui)

(export 'w-dynamic*)
(declaim
  (inline w-dynamic*)
  (ftype (function (ui (function () (values (or null (function () (values &optional))) &optional)))
                   (function () (values &optional)))
         w-dynamic*))
(defun w-dynamic* (ui compose)
  (let ((placeholder (insert-placeholder ui)))
    (sdet:make-effect (ui-sdet-context ui)
      (with-composition-after-placeholder ui placeholder
        (funcall compose)))
    (lambda ()
      (delete-placeholder placeholder))))

(export 'w-dynamic)
(defmacro w-dynamic (&body body)
  `(w-dynamic* ,*ui* (lambda () ,@body)))

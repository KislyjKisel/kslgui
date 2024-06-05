(in-package #:kslgui)

(export 'w-dynamic*)
(declaim
  (inline w-dynamic*)
  (ftype (function (ui (function () (values &optional)))
                   (values &optional))
         w-dynamic*))
(defun w-dynamic* (ui compose)
  (let ((placeholder (insert-placeholder ui)))
    (sdet:on-cleanup (sdet-context ui)
      (delete-placeholder placeholder)
      (values))
    (sdet:make-effect (ui-sdet-context ui)
      (with-composition-after-placeholder ui placeholder
        (funcall compose)))
    (values)))

(export 'w-dynamic)
(defmacro w-dynamic (&body body)
  `(w-dynamic* ,*ui* (lambda () ,@body)))

; todo: trivia / match variant (match (*computed*) ((a . b) (button)) (nil (label)))
(export 'w-cond)
(defmacro w-cond (&rest branches)
  "BRANCHES - list of (condition result)"
  (let* ((otherwise-clause-already nil)
         (bodies (let ((index 0))
                   (mapcar (lambda (branch)
                             (when (or (eq 'otherwise (first branch))
                                       (eq t (first branch)))
                                   (setf otherwise-clause-already t))
                             `(,(1- (incf index)) ,(second branch)))
                       branches))))
    (alexandria:with-gensyms (branch-index-computed)
      `(let ((,branch-index-computed (sdet:make-computed (sdet-context ,*ui*)
                                       (cond
                                        ,@(let ((index 0))
                                               (mapcar (lambda (branch)
                                                         (prog1
                                                             `(,(first branch) ,index)
                                                           (incf index))) branches))))))

         (w-dynamic* ,*ui*
                     (lambda ()
                       (ecase (sdet:compute ,branch-index-computed)
                         ,@bodies
                         ,@(unless otherwise-clause-already
                             `(((nil) (values)))))))))))

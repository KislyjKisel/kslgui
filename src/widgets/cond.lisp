(in-package #:kslgui)

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
                             `(((nil) nil))))))))))

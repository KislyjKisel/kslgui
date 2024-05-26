(in-package #:kslgui)

(export 'w-cond*)
(defun w-cond* (ui branch-index-computed children-variants)
  (sdet:make-effect (ui-sdet-context ui)
    (let ((placeholder (insert-placeholder ui)))
      (sdet:make-effect (ui-sdet-context ui)
        (with-composition-after-placeholder ui placeholder
          (funcall (svref children-variants (sdet:compute branch-index-computed)))))
      (lambda ()
        (delete-placeholder placeholder))))
  (values))

; todo: trivia / match variant (match (*computed*) ((a . b) (button)) (nil (label)))
(export 'w-cond)
(defmacro w-cond (&rest branches)
  "BRANCHES - list of (condition result)"
  `(w-cond* ,*ui*
            (sdet:make-computed (sdet-context ,*ui*)
              (cond
               ,@(let ((index 0))
                      (mapcar (lambda (branch)
                                (prog1
                                    `(,(first branch) ,index)
                                  (incf index))) branches))))
            (vector ,@(mapcar (lambda (branch) `(lambda () ,(second branch))) branches))))

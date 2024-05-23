(in-package #:kslgui)

(defun w-cond-impl (ui branch-index-computed children-variants)
  (sdet:make-effect (ui-sdet-context ui)
    (let ((placeholder (insert-placeholder ui)))
      (sdet:make-effect (ui-sdet-context ui)
        (let ((previous-parent (ui-temp-parent ui))
              (previous-sibling-index (ui-temp-sibling-index  ui)))
          (setf (ui-temp-sibling-index ui) (placeholder-index placeholder))
          (setf (ui-temp-parent ui) (placeholder-parent placeholder))
          (prog1
              (funcall (svref children-variants (sdet:compute branch-index-computed)))
            (setf (ui-temp-sibling-index ui) previous-sibling-index)
            (setf (ui-temp-parent ui) previous-parent)
            )))
      (lambda ()
        (delete-placeholder placeholder))))
  (values))

; todo: trivia / match variant (match (*computed*) ((a . b) (button)) (nil (label)))
(export 'w-cond)
(defmacro w-cond (&rest branches)
  "BRANCHES - list of (condition result)"
  `(w-cond-impl ,*ui*
                (lambda ()
                  (cond
                   ,@(let ((index 0))
                          (mapcar (lambda (branch)
                                    (prog1
                                        `(,(first branch) ,index)
                                      (incf index))) branches))))
                (vector ,@(mapcar (lambda (branch) `(lambda () ,(second branch))) branches))))

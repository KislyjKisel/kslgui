(in-package #:kslgui)

(defun w-cond-impl (ui branch-index-computed children-variants)
  (let (first-child)
    (sdet:make-effect (ui-sdet-context ui)
      (let ((previous-context-first-widget (ui-temp-first-widget ui))
            (rerunning (not (null first-child))))
        (setf (ui-temp-first-widget ui) nil)
        (when rerunning ; Executed outside composition context - needs to set up it itself
              (setf (ui-temp-sibling-index ui) (widget-yoga-index first-child))
              (setf (ui-temp-parent ui) (widget-parent first-child)))
        (prog1
            (funcall (svref children-variants (sdet:compute branch-index-computed)))
          (setf first-child (ui-temp-first-widget ui))
          (when previous-context-first-widget
                (setf (ui-temp-first-widget ui) previous-context-first-widget))
          (when rerunning ; Note: FIRST-CHILD may be not nil there even on the first run
                ;; Cleaning set up context just in case. (why not return previous values? just useless?)
                (setf (ui-temp-sibling-index ui) 0)
                (setf (ui-temp-parent ui) nil)))))))

; todo: trivia / match variant (match (*computed*) ((a . b) (button)) (nil (label)))
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

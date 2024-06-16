(in-package #:kslgui)

(export 'w-map-fix-index*)
(defun w-map-fix-index* (ui values-computed f &key (equal #'eql) (copy :reference))
  (let ((top-placeholder (insert-placeholder ui)))
    (sdet:on-cleanup (sdet-context ui)
      (delete-placeholder top-placeholder)
      (values))
    (sdet:map-effect-fix-index (sdet-context ui) values-computed
                               (lambda (get-val index)
                                 (let ((placeholder top-placeholder))
                                   (with-composition-after-placeholder ui placeholder
                                     (setf top-placeholder (insert-placeholder ui)))
                                   (sdet:on-cleanup (sdet-context ui)
                                     (delete-placeholder top-placeholder)
                                     (setf top-placeholder placeholder)
                                     (values))
                                   (sdet:make-effect (sdet-context ui)
                                     (with-composition-after-placeholder ui placeholder
                                       (funcall f get-val index)))))
                               :equal equal
                               :copy copy)
    (values)))

(export 'w-map-fix-index)
(defmacro w-map-fix-index (values (get-val-sym index-sym &key ui (equal '#'eql) (copy :reference)) &body body)
  (let ((actual-get-val-sym (or get-val-sym (gensym)))
        (actual-index-sym (or index-sym (gensym))))
    (macroexpand-with-ui* ui
      `(w-map-fix-index* ,*ui*
                         ,(make-computed-prop values :initialized nil)
                         (lambda (,actual-get-val-sym ,actual-index-sym)
                           (declare (ignore ,@(unless get-val-sym (list actual-get-val-sym))
                                            ,@(unless index-sym (list actual-index-sym))))
                           ,@body)
                         :equal ,equal
                         :copy ,copy))))

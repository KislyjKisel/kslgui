;;;; Styles: objects controlling fill and stroke colors when rendering
;;;; Possible style descriptions:
;;;;   null <- nil
;;;;   (unsigned-byte 32) <- #xAARRGGBB
;;;;   gradient-style-description <- (gradient &key ...)

(in-package #:kslgui)

(defstruct (gradient-style-description (:copier nil) (:constructor gradient))
  (type :linear :type (member :linear :radial))
  (extend-mode :pad :type (member :pad))
  (x0 0.0f0 :type single-float)
  (y0 0.0f0 :type single-float)
  (x1 0.0f0 :type single-float)
  (y1 0.0f0 :type single-float)
  (radius 0.0f0 :type single-float)
  (stops #() :type (vector (cons single-float (unsigned-byte 32)))))

(defstruct (style (:copier nil))
  (type :gradient :type (member :gradient))
  blend2d-object)

(defun dispose-style (style)
  (etypecase style
    (null)
    (integer)
    (style (autowrap:free (style-blend2d-object style))))
  (values))

(defun create-style (old-style description)
  (etypecase description
    (null
      (dispose-style old-style)
      nil)
    (integer
     (dispose-style old-style)
     description)
    (gradient-style-description
     (if (or (null old-style) (integerp old-style))
         (setf old-style (make-style :type :gradient
                                     :blend2d-object nil))
         ;  (unless (eq :gradient (style-type old-style))
         ;    (autowrap:free (style-blend2d-object old-style))
         ;    (setf (style-blend2d-object old-style) nil))
         )
     (if (style-blend2d-object old-style)
         (%blend2d:gradient-reset (style-blend2d-object old-style))
         (progn
          (setf (style-blend2d-object old-style) (autowrap:alloc '%blend2d:gradient-core))
          (%blend2d:gradient-init (style-blend2d-object old-style))))

     (%blend2d:gradient-set-extend-mode (style-blend2d-object old-style)
                                        (ecase (gradient-style-description-extend-mode description)
                                          (:pad %blend2d:+extend-mode-pad+)))
     (%blend2d:gradient-set-value (style-blend2d-object old-style)
                                  %blend2d:+gradient-value-common-x0+
                                  (coerce (gradient-style-description-x0 description) 'double-float))
     (%blend2d:gradient-set-value (style-blend2d-object old-style)
                                  %blend2d:+gradient-value-common-y0+
                                  (coerce (gradient-style-description-y0 description) 'double-float))
     (%blend2d:gradient-set-value (style-blend2d-object old-style)
                                  %blend2d:+gradient-value-common-x1+
                                  (coerce (gradient-style-description-x1 description) 'double-float))
     (%blend2d:gradient-set-value (style-blend2d-object old-style)
                                  %blend2d:+gradient-value-common-y1+
                                  (coerce (gradient-style-description-y1 description) 'double-float))
     (ecase (gradient-style-description-type description)
       (:linear (%blend2d:gradient-set-type (style-blend2d-object old-style)
                                            %blend2d:+gradient-type-linear+))
       (:radial (%blend2d:gradient-set-type (style-blend2d-object old-style)
                                            %blend2d:+gradient-type-radial+)
                (%blend2d:gradient-set-value (style-blend2d-object old-style)
                                             %blend2d:+gradient-value-radial-r0+
                                             (coerce (gradient-style-description-radius description) 'double-float))))
     (loop #:for stop #:across (gradient-style-description-stops description) #:do
           (%blend2d:gradient-add-stop-rgba32 (style-blend2d-object old-style)
                                              (coerce (car stop) 'double-float)
                                              (cdr stop)))
     old-style)))

(defun blend2d-context-set-stroke-style (ctx style)
  (etypecase style
    (integer (%blend2d:context-set-stroke-style-rgba32 ctx (coerce style '(unsigned-byte 32))))
    (style
     (%blend2d:context-set-stroke-style ctx (style-blend2d-object style)))))

(defun blend2d-context-set-fill-style (ctx style)
  (etypecase style
    (integer (%blend2d:context-set-fill-style-rgba32 ctx (coerce style '(unsigned-byte 32))))
    (style
     (%blend2d:context-set-fill-style ctx (style-blend2d-object style)))))

(in-package #:kslgui)

(export 'v-rectangle)
(defstruct (rectangle-visual-description (:include shape-visual-description)
                                         (:constructor v-rectangle
                                                       (&key fill-style border-style border-width
                                                             &aux (update #'rectangle-visual-description-update-impl)))
                                         (:copier nil)))

(defstruct (rectangle-visual-state (:include shape-visual-state)
                                   (:constructor make-rectangle-visual-state
                                                 (&aux
                                                  (render #'rectangle-visual-state-render-impl)
                                                  (destroy #'rectangle-visual-state-destroy-impl)
                                                  (hitp #'rectangle-visual-state-hitp-impl)))
                                   (:copier nil)))

(defun rectangle-visual-description-create-impl (vdescr)
  (update-shape-visual-fields vdescr (make-rectangle-visual-state)))

(define-visual-description-update rectangle-visual-description-update-impl
    (vdescr (vstate rectangle-visual-state) :create-fn rectangle-visual-description-create-impl)
  (update-shape-visual-fields vdescr vstate)
  (values))

(defun rectangle-visual-state-render-impl (ui vstate x y width height)
  (let* ((rect (alloc-blend2d-rect ui))
         (border-width-value (or (rectangle-visual-state-border-width vstate)
                                 (if (rectangle-visual-state-border-style vstate)
                                     1.0d0
                                     0.0d0))))
    (let ((width (- width (* 2.0d0 border-width-value)))
          (height (- height (* 2.0d0 border-width-value))))
      (when (and (rectangle-visual-state-fill-style vstate)
                 (< 0.0d0 width)
                 (< 0.0d0 height))
            (setf (%blend2d:rect.x rect) (+ x border-width-value))
            (setf (%blend2d:rect.y rect) (+ y border-width-value))
            (setf (%blend2d:rect.w rect) width)
            (setf (%blend2d:rect.h rect) height)
            (blend2d-context-set-fill-style (layer-context (ui-temp-layer ui))
                                            (rectangle-visual-state-fill-style vstate))
            (%blend2d:context-fill-geometry (layer-context (ui-temp-layer ui))
                                            %blend2d:+geometry-type-rectd+
                                            rect)))
    (let* ((width (- width border-width-value))
           (height (- height border-width-value)))
      (when (and (rectangle-visual-state-border-style vstate)
                 (< 0.0d0 width)
                 (< 0.0d0 height))
            (setf (%blend2d:rect.x rect) (+ x (* 0.5d0 border-width-value)))
            (setf (%blend2d:rect.y rect) (+ y (* 0.5d0 border-width-value)))
            (setf (%blend2d:rect.w rect) width)
            (setf (%blend2d:rect.h rect) height)
            (%blend2d:context-set-stroke-width (layer-context (ui-temp-layer ui))
                                               border-width-value)
            (blend2d-context-set-stroke-style (layer-context (ui-temp-layer ui))
                                              (rectangle-visual-state-border-style vstate))
            (%blend2d:context-stroke-geometry (layer-context (ui-temp-layer ui))
                                              %blend2d:+geometry-type-rectd+
                                              rect)))
    (free-blend2d-rect ui rect))
  (values))

(defun rectangle-visual-state-destroy-impl (vstate)
  (destroy-shape-visual-fields vstate)
  (values))

(defun rectangle-visual-state-hitp-impl (ui vstate x y w h mx my)
  (declare (ignore ui vstate))
  (and (>= mx x)
       (>= my y)
       (< mx (+ x w))
       (< my (+ y h))))

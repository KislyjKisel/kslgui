(in-package #:kslgui)

(defstruct (path-visual-description (:include shape-visual-description)
                                    (:constructor visual-path
                                                  (path-fn &key fill-rule fill-style border-style border-width
                                                           &aux (update #'path-visual-description-update-impl)))
                                    (:copier nil))
  (fill-rule %blend2d:+fill-rule-non-zero+ :type (unsigned-byte 32))
  (path-fn))

(defstruct (path-visual-state (:include shape-visual-state)
                              (:constructor make-path-visual-state
                                            (fill-rule
                                             &aux
                                             (render #'path-visual-state-render-impl)
                                             (destroy #'path-visual-state-destroy-impl)
                                             (hitp #'path-visual-state-hitp-impl)
                                             (path (autowrap:alloc '%blend2d:path-core))))
                              (:copier nil))
  (fill-rule %blend2d:+fill-rule-non-zero+ :type (unsigned-byte 32))
  (path)
  (prev-width -1.0d0 :type double-float)
  (prev-height -1.0d0 :type double-float)
  (path-fn))

(defun path-visual-description-create-impl (vdescr)
  (let ((vstate (make-path-visual-state (path-visual-description-fill-rule vdescr))))
    (update-shape-visual-fields vdescr vstate)
    (%blend2d:path-init (path-visual-state-path vstate))
    (setf (path-visual-state-path-fn vstate) (path-visual-description-path-fn vdescr))
    vstate))

(define-visual-description-update path-visual-description-update-impl
    (vdescr (vstate path-visual-state) :create-fn path-visual-description-create-impl)
  (update-shape-visual-fields vdescr vstate)
  (setf (path-visual-state-fill-rule vstate) (path-visual-description-fill-rule vdescr))
  (unless (eq (path-visual-state-path-fn vstate) (path-visual-description-path-fn vdescr))
    (setf (path-visual-state-path-fn vstate) (path-visual-description-path-fn vdescr))
    (setf (path-visual-state-prev-width vstate) -1.0d0)
    (setf (path-visual-state-prev-height vstate) -1.0d0))
  (values))

(defun path-visual-state-render-impl (ui vstate x y width height)
  (let ((origin (ui-alloc-blend2d-point ui)))
    (when (or (/= width (path-visual-state-prev-width vstate))
              (/= height (path-visual-state-prev-height vstate)))
          (%blend2d:path-clear (path-visual-state-path vstate))
          (funcall (path-visual-state-path-fn vstate) (path-visual-state-path vstate) width height)
          (setf (path-visual-state-prev-width vstate) width)
          (setf (path-visual-state-prev-height vstate) height))
    (setf (%blend2d:point.x origin) x)
    (setf (%blend2d:point.y origin) y)
    (when (path-visual-state-fill-style vstate)
          (%blend2d:context-set-fill-rule (layer-context (ui-temp-layer ui)) (path-visual-state-fill-rule vstate))
          (blend2d-context-set-fill-style (layer-context (ui-temp-layer ui)) (path-visual-state-fill-style vstate))
          (%blend2d:context-fill-path-d (layer-context (ui-temp-layer ui)) origin (path-visual-state-path vstate)))
    (when (path-visual-state-border-style vstate)
          (%blend2d:context-set-stroke-width (layer-context (ui-temp-layer ui)) (or (path-visual-state-border-width vstate) 1.0d0))
          (blend2d-context-set-stroke-style (layer-context (ui-temp-layer ui)) (path-visual-state-border-style vstate))
          (%blend2d:context-stroke-path-d (layer-context (ui-temp-layer ui)) origin (path-visual-state-path vstate)))
    (ui-free-blend2d-point ui origin))
  (values))

(defun path-visual-state-destroy-impl (vstate)
  (destroy-shape-visual-fields vstate)
  (%blend2d:path-destroy (path-visual-state-path vstate))
  (autowrap:free (path-visual-state-path vstate))
  (values))

(defun path-visual-state-hitp-impl (ui vstate x y width height mx my)
  ;; Note: updating path based on X / Y will cause constant re-pathing
  ;; because HITP uses view coordinates while rendering uses absolute coordinates.
  (let ((point (ui-alloc-blend2d-point ui)))
    (when (or (/= width (path-visual-state-prev-width vstate))
              (/= height (path-visual-state-prev-height vstate)))
          (%blend2d:path-clear (path-visual-state-path vstate))
          (funcall (path-visual-state-path-fn vstate) (path-visual-state-path vstate) width height)
          (setf (path-visual-state-prev-width vstate) width)
          (setf (path-visual-state-prev-height vstate) height))
    (setf (%blend2d:point.x point) (- mx x))
    (setf (%blend2d:point.y point) (- my y))
    (prog1
        (= %blend2d:+hit-test-in+
          (%blend2d:path-hit-test (path-visual-state-path vstate) point (path-visual-state-fill-rule vstate)))
      (ui-free-blend2d-point ui point))))


;;; Checkmark

(defun visual-checkmark (&key fill-style border-style border-width)
  (visual-path (lambda (path width height)
                 (%blend2d:path-move-to path (* 0.17d0 width) (* 0.45d0 height))
                 (%blend2d:path-line-to path (* 0.32d0 width) (* 0.67d0 height))
                 (%blend2d:path-line-to path (* 0.82d0 width) (* 0.1d0 height))
                 (%blend2d:path-line-to path (* 0.92d0 width) (* 0.2d0 height))
                 (%blend2d:path-line-to path (* 0.32d0 width) (* 0.84d0 height))
                 (%blend2d:path-line-to path (* 0.07d0 width) (* 0.55d0 height))
                 (%blend2d:path-line-to path (* 0.17d0 width) (* 0.45d0 height)))
               :fill-style fill-style
               :border-style border-style
               :border-width border-width))
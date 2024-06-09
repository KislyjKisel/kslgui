(in-package #:kslgui)

(export 'v-vector)
(defstruct (vector-visual-description (:include visual-description)
                                      (:constructor v-vector
                                                    (values &aux (update #'vector-visual-description-update-impl)))
                                      (:copier nil))
  (values (unreachable) :type (vector (or null visual-description))))

(defstruct (vector-visual-state (:include visual-state)
                                (:constructor make-vector-visual-state
                                              (length
                                                &aux
                                                (values (make-array length :adjustable t :fill-pointer length :initial-element nil))
                                                (render #'vector-visual-state-render-impl)
                                                (destroy #'vector-visual-state-destroy-impl)
                                                (hitp #'vector-visual-state-hitp-impl)))
                                (:copier nil))
  (values (unreachable) :type (vector (or null visual-state))))

(defun vector-visual-description-create-impl (vdescr)
  (let* ((vdescr-vector (vector-visual-description-values vdescr))
         (length (length vdescr-vector))
         (vstate (make-vector-visual-state length))
         (vstate-vector (vector-visual-state-values vstate)))
    (loop #:for index #:below length
          #:for nested-vdescr = (aref vdescr-vector index)
          #:do (setf (aref vstate-vector index) (update-visual (aref vdescr-vector index) nil)))
    vstate))

(define-visual-description-update vector-visual-description-update-impl
    (vdescr (vstate vector-visual-state) :create-fn vector-visual-description-create-impl)
  (let* ((vstate-vector (vector-visual-state-values vstate))
         (vdescr-vector (vector-visual-description-values vdescr))
         (vstate-vector-length (length vstate-vector))
         (vdescr-vector-length (length vdescr-vector)))
    (dotimes (index vstate-vector-length)
      (let ((nested-vstate (aref vstate-vector index)))
        (if (< index vdescr-vector-length)
            (setf (aref vstate-vector index) (update-visual (aref vdescr-vector index) nested-vstate))
            (progn
             (funcall (visual-state-destroy nested-vstate) nested-vstate)
             (setf (aref vstate-vector index) nil)))))
    (loop #:for index #:from (length vstate-vector) #:below vdescr-vector-length
          #:do (vector-push-extend (update-visual (aref vdescr-vector index) nil)
                                   vstate-vector))
    (setf (fill-pointer vstate-vector) vdescr-vector-length))
  (values))

(defun vector-visual-state-render-impl (ui vstate x y width height)
  (loop #:for nested-vstate #:across (vector-visual-state-values vstate)
        #:do
        (funcall (visual-state-render nested-vstate) ui nested-vstate x y width height))
  (values))

(defun vector-visual-state-destroy-impl (vstate)
  (loop #:for nested-vstate #:across (vector-visual-state-values vstate)
        #:do
        (funcall (visual-state-destroy nested-vstate) nested-vstate))
  (values))

(defun vector-visual-state-hitp-impl (ui vstate x y w h mx my)
  (loop #:for nested-vstate #:across (vector-visual-state-values vstate)
        #:thereis (and (visual-state-hitp nested-vstate)
                       (funcall (visual-state-hitp nested-vstate) ui nested-vstate x y w h mx my))))

(in-package #:kslgui)

(export '(layer make-layer layer-context))
(defstruct (layer (:copier nil)
                  (:constructor make-layer (image
                                            &key cci
                                            &aux (context
                                                  (let ((context (autowrap:alloc '%blend2d:context-core)))
                                                    (%blend2d:context-init-as context
                                                                              image
                                                                              (or cci (cffi:null-pointer)))
                                                    context)))))
  (image (unreachable) :type %blend2d:image-core)
  (context (unreachable) :type %blend2d:context-core :read-only t)
  (cci (unreachable) :type (or null %blend2d:context-create-info)))

(export 'dispose-layer)
(declaim (ftype (function (layer) (values &optional)) dispose-layer))
(defun dispose-layer (layer)
  "Destroys blend2d context. Doesn't affect image or cci.
  Using this layer afterwards causes UB."
  (%blend2d:context-destroy (layer-blend2d-context layer))
  (autowrap:free (layer-blend2d-context layer))
  (values))

(export 'set-layer-image)
(declaim
  (inline set-layer-image)
  (ftype (function (layer (function () (values %blend2d:image-core &optional)))
                   (values %blend2d:image-core &optional))
         set-layer-image))
(defun set-layer-image (layer f)
  (%blend2d:context-reset (layer-context layer))
  (let ((new-image (funcall f)))
    (setf (layer-image layer) new-image)
    (%blend2d:context-begin (layer-context layer)
                            new-image
                            (or (layer-cci layer) (cffi:null-pointer)))
    new-image))

(export 'set-layer-cci)
(declaim (ftype (function (layer (or null %blend2d:context-create-info)) (values &optional)) set-layer-cci))
(defun set-layer-cci (layer cci)
  (setf (layer-cci layer) cci)
  (values))

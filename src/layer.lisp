(in-package #:kslgui)

(export '(layer make-layer layer-context))
(defstruct (layer (:copier nil)
                  (:constructor make-layer (image
                                            &key (cci nil)
                                            &aux (context
                                                  (let ((context (autowrap:alloc '%blend2d:context-core)))
                                                    (%blend2d:context-init-as context
                                                                              image
                                                                              (or cci (cffi:null-pointer)))
                                                    context)))))
  (context (unreachable) :type %blend2d:context-core :read-only t))

(export 'dispose-layer)
(declaim (ftype (function (layer) (values &optional)) dispose-layer))
(defun dispose-layer (layer)
  "Destroys blend2d context. Using this layer afterwards causes UB."
  (%blend2d:context-destroy (layer-context layer))
  (autowrap:free (layer-context layer))
  (values))

(export 'update-layer)
(declaim
  (inline update-layer)
  (ftype (function (layer (function () (values %blend2d:image-core &optional)) &key (:cci (or null %blend2d:context-create-info)))
                   (values %blend2d:image-core &optional))
         update-layer))
(defun update-layer (layer f &key cci)
  (%blend2d:context-reset (layer-context layer))
  (let ((new-image (funcall f)))
    (%blend2d:context-begin (layer-context layer)
                            new-image
                            (or cci (cffi:null-pointer)))
    new-image))

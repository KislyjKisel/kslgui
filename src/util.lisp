(in-package #:kslgui)

(export 'rgb)
(declaim
  (inline rgb)
  (ftype (function ((unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 8) &optional (unsigned-byte 8))
                   (values (unsigned-byte 32) &optional))
         rgb))
(defun rgb (r g b &optional (a 255))
  "Makes a color value from integer components in range [0; 255]."
  (logior (ash a 24) (ash r 16) (ash g 8) b))

(export 'rgbf)
(declaim
  (inline rgbf)
  (ftype (function (float float float &optional float) (values (unsigned-byte 32) &optional)) rgbf))
(defun rgbf (r g b &optional (a 1.0))
  "Makes a color value from FLOAT components in range [0; 1] (unchecked)."
  (rgb (floor (* r 255)) (floor (* g 255)) (floor (* b 255)) (floor (* a 255))))

(export 'rgbfc)
(declaim
  (inline rgbfc)
  (ftype (function (float float float &optional float) (values (unsigned-byte 32) &optional)) rgbfc))
(defun rgbfc (r g b &optional (a 1.0))
  "Makes a color value from FLOAT components in range [0; 1] (clamped)."
  (rgb (max 0 (min 255 (floor (* r 255))))
       (max 0 (min 255 (floor (* g 255))))
       (max 0 (min 255 (floor (* b 255))))
       (max 0 (min 255 (floor (* a 255))))))

(export 'let-derived)
(defmacro let-derived (bindings &body body)
  "Binds expressions wrapping them in parameterless lambdas.
  Suitable for reactive expression reuse."
  (setf bindings (mapcar (lambda (binding)
                           (assert (= 2 (length binding)))
                           (vector (gensym) (first binding) (second binding)))
                     bindings))
  `(let ,(mapcar (lambda (x) `(,(svref x 0) (lambda () ,(svref x 2)))) bindings)
     (declare (ignorable ,@(mapcar (lambda (x) (svref x 0)) bindings)))
     (symbol-macrolet ,(mapcar (lambda (x) `(,(svref x 1) (funcall ,(svref x 0)))) bindings)
       ,@body)))

(declaim (inline unreachable))
(defun unreachable ()
  (error "Unreachable reached."))

(declaim
  (inline vector-delete)
  (ftype (function (t vector) (values vector &optional)) vector-delete))
(defun vector-delete (item vector)
  "Wraps DELETE and guarantees that the result has a fill-pointer and is adjustable."
  (let ((result (delete item vector)))
    #-sbcl(unless (and (adjustable-array-p result) (array-has-fill-pointer-p result))
           (setf result (make-array (length result) :adjustable t :fill-pointer (length result) :initial-contents result)))
    result))

(declaim
  (inline vector-insert)
  (ftype (function (vector t (mod #.array-dimension-limit)) (values vector &optional)) vector-insert))
(defun vector-insert (vector value index)
  (replace vector vector :start1 (1+ index) :start2 index :end2 (vector-push-extend value vector))
  (setf (aref vector index) value)
  vector)

(defun map-plist-to-list (fn plist)
  "Processes key-value pairs in PLIST using (FUNCALL FN KEY VALUE) and collects results in a list."
  (loop #:for (key value) #:on plist #:by #'cddr
        #:collect (funcall fn key value)))

(defstruct aabb
  (min-x 0.0 :type single-float)
  (min-y 0.0 :type single-float)
  (max-x -1.0 :type single-float)
  (max-y -1.0 :type single-float))

(declaim
  (inline aabb-contains)
  (ftype (function (aabb float float) (values boolean &optional)) aabb-contains))
(defun aabb-contains (aabb x y)
  (and (>= x (aabb-min-x aabb))
       (>= y (aabb-min-y aabb))
       (< x (aabb-max-x aabb))
       (< y (aabb-max-y aabb))))

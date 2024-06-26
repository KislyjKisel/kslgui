(in-package #:kslgui)

(export 'render-cursor)
(defgeneric render-cursor (renderer blend2d-context cursor x y))

(defmethod render-cursor ((renderer null) blend2d-context cursor x y)
  (declare (ignore renderer blend2d-context cursor x y))
  (values))

(export '(cursor-renderer create-cursor-renderer))
(defstruct (cursor-renderer (:copier nil)
                            (:constructor create-cursor-renderer (&key (scale 1.0d0))))
  (path (let ((path (autowrap:alloc '%blend2d:path-core))) (%blend2d:path-init path) path))
  (origin (autowrap:alloc '%blend2d:point))
  (previous-cursor nil :type (or null cursor))
  (stroke-width 0.0d0 :type double-float)
  (scale (unreachable) :type double-float))

(defmethod render-cursor ((renderer cursor-renderer) ctx cursor x y)
  (let ((path (cursor-renderer-path renderer))
        (origin (cursor-renderer-origin renderer)))
    (unless (eq cursor (cursor-renderer-previous-cursor renderer))
      (setf (cursor-renderer-previous-cursor renderer) cursor)
      (%blend2d:path-clear path)
      (ecase cursor
        (:default
         (setf (cursor-renderer-stroke-width renderer) 1.0d0)
         (%blend2d:path-move-to path 0.0d0 0.0d0)
         (%blend2d:path-line-to path 12.0d0 12.0d0)
         (%blend2d:path-line-to path 5.0d0 12.5d0)
         (%blend2d:path-line-to path 0.0d0 17.0d0)
         (%blend2d:path-line-to path 0.0d0 0.0d0))
        (:text
         (setf (cursor-renderer-stroke-width renderer) 1.0d0)
         (let* ((horz-bar-length 2.5d0)
                (half-thickness 1.1d0)
                (half-height 6.5d0)
                (x0 (- 0 half-thickness horz-bar-length))
                (x1 (- half-thickness))
                (x2 half-thickness)
                (x3 (+ half-thickness horz-bar-length))
                (y0 (- 0 half-thickness half-thickness half-height))
                (y1 (- half-height))
                (y2 half-height)
                (y3 (+ half-height half-thickness half-thickness)))
           (%blend2d:path-move-to path x1 y1)
           (%blend2d:path-line-to path x0 y1)
           (%blend2d:path-line-to path x0 y0)
           (%blend2d:path-line-to path x3 y0)
           (%blend2d:path-line-to path x3 y1)
           (%blend2d:path-line-to path x2 y1)
           (%blend2d:path-line-to path x2 y2)
           (%blend2d:path-line-to path x3 y2)
           (%blend2d:path-line-to path x3 y3)
           (%blend2d:path-line-to path x0 y3)
           (%blend2d:path-line-to path x0 y2)
           (%blend2d:path-line-to path x1 y2)
           (%blend2d:path-line-to path x1 y1)))
        (:press
         (setf (cursor-renderer-stroke-width renderer) 1.0d0)
         (%blend2d:path-move-to path 0.0d0 0.0d0)
         (%blend2d:path-line-to path 3.0d0 0.0d0)
         (%blend2d:path-line-to path 3.0d0 6.0d0)
         (%blend2d:path-line-to path 10.0d0 9.0d0)
         (%blend2d:path-line-to path 10.0d0 20.0d0)
         (%blend2d:path-line-to path -3.0d0 20.0d0)
         (%blend2d:path-line-to path -3.0d0 10.0d0)
         (%blend2d:path-line-to path 0.0d0 8.0d0)
         (%blend2d:path-line-to path 0.0d0 0.0d0))
        (:release
         (setf (cursor-renderer-stroke-width renderer) 1.0d0)
         (%blend2d:path-move-to path 0.0d0 3.0d0)
         (%blend2d:path-line-to path 3.0d0 3.0d0)
         (%blend2d:path-line-to path 3.0d0 6.0d0)
         (%blend2d:path-line-to path 10.0d0 9.0d0)
         (%blend2d:path-line-to path 10.0d0 20.0d0)
         (%blend2d:path-line-to path -3.0d0 20.0d0)
         (%blend2d:path-line-to path -3.0d0 10.0d0)
         (%blend2d:path-line-to path 0.0d0 8.0d0)
         (%blend2d:path-line-to path 0.0d0 3.0d0))
        (:grab
         (setf (cursor-renderer-stroke-width renderer) 1.0d0)
         (let* ((finger-width 3.0d0)
                (x0 (- (* 1.5d0 finger-width)))
                (x1 (- (* 0.8d0 finger-width)))
                (x2 0.0d0)
                (x3 finger-width)
                (x4 (* 2.0d0 finger-width))
                (x5 (* 3.0d0 finger-width))
                (x6 (* 4.0d0 finger-width))
                (y0 (- finger-width))
                (y1 0.0d0)
                (y2 (* 1.0d0 finger-width))
                (y3 (* 1.7d0 finger-width))
                (y4 (* 2.0d0 finger-width))
                (y5 (* 1.8d0 finger-width))
                (y6 (* 2.3d0 finger-width))
                (y7 (* 3.0d0 finger-width))
                (y8 (* 5.0d0 finger-width)))
           (%blend2d:path-move-to path x2 y1)
           (%blend2d:path-line-to path x3 y1)
           (%blend2d:path-line-to path x3 y3)
           (%blend2d:path-line-to path x3 y0)
           (%blend2d:path-line-to path x4 y0)
           (%blend2d:path-line-to path x4 y3)
           (%blend2d:path-line-to path x4 y1)
           (%blend2d:path-line-to path x5 y1)
           (%blend2d:path-line-to path x5 y4)
           (%blend2d:path-line-to path x5 y2)
           (%blend2d:path-line-to path x6 y2)
           (%blend2d:path-line-to path x6 y8)
           (%blend2d:path-line-to path x2 y8)
           (%blend2d:path-line-to path x0 y6)
           (%blend2d:path-line-to path x1 y5)
           (%blend2d:path-line-to path x2 y7)
           (%blend2d:path-line-to path x2 y1)))
        (:move-x
         (setf (cursor-renderer-stroke-width renderer) 1.0d0)
         (let* ((x-unit 3.0d0)
                (y-unit 3.0d0))
           (%blend2d:path-move-to path (- x-unit) (- y-unit))
           (%blend2d:path-line-to path x-unit (- y-unit))
           (%blend2d:path-line-to path x-unit y-unit)
           (%blend2d:path-line-to path (- x-unit) y-unit)
           (%blend2d:path-line-to path (- x-unit) (- y-unit))
           (%blend2d:path-move-to path (* 2.0d0 x-unit) (- y-unit))
           (%blend2d:path-line-to path (* 4.0d0 x-unit) 0.0d0)
           (%blend2d:path-line-to path (* 2.0d0 x-unit) y-unit)
           (%blend2d:path-line-to path (* 2.0d0 x-unit) (- y-unit))
           (%blend2d:path-move-to path (- (* 2.0d0 x-unit)) (- y-unit))
           (%blend2d:path-line-to path (- (* 4.0d0 x-unit)) 0.0d0)
           (%blend2d:path-line-to path (- (* 2.0d0 x-unit)) y-unit)
           (%blend2d:path-line-to path (- (* 2.0d0 x-unit)) (- y-unit))))
        (:move-y
         (setf (cursor-renderer-stroke-width renderer) 1.0d0)
         (let* ((x-unit 3.0d0)
                (y-unit 3.0d0))
           (%blend2d:path-move-to path (- x-unit) (- y-unit))
           (%blend2d:path-line-to path x-unit (- y-unit))
           (%blend2d:path-line-to path x-unit y-unit)
           (%blend2d:path-line-to path (- x-unit) y-unit)
           (%blend2d:path-line-to path (- x-unit) (- y-unit))
           (%blend2d:path-move-to path (- x-unit) (* 2.0d0 y-unit))
           (%blend2d:path-line-to path 0.0d0 (* 4.0d0 y-unit))
           (%blend2d:path-line-to path x-unit (* 2.0d0 y-unit))
           (%blend2d:path-line-to path (- x-unit) (* 2.0d0 y-unit))
           (%blend2d:path-move-to path (- x-unit) (- (* 2.0d0 y-unit)))
           (%blend2d:path-line-to path 0.0d0 (- (* 4.0d0 y-unit)))
           (%blend2d:path-line-to path x-unit (- (* 2.0d0 y-unit)))
           (%blend2d:path-line-to path (- x-unit) (- (* 2.0d0 y-unit)))))))
    (setf (%blend2d:point.x origin) x)
    (setf (%blend2d:point.y origin) y)
    (%blend2d:context-set-stroke-width ctx (cursor-renderer-stroke-width renderer))
    (%blend2d:context-fill-path-d-rgba32 ctx origin path #xFFFFFFFF)
    (float-features:with-float-traps-masked (:invalid)
      (%blend2d:context-stroke-path-d-rgba32 ctx origin path #xFF000000)))
  (values))

(export 'destroy-cursor-renderer)
(declaim (ftype (function (cursor-renderer) (values &optional)) destroy-cursor-renderer))
(defun destroy-cursor-renderer (renderer)
  (autowrap:free (cursor-renderer-path renderer))
  (values))

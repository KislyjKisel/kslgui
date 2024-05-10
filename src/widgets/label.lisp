(in-package #:kslgui)


;;; `font-cache`: cached sized font, metrics, space width

(defstruct (font-cache (:copier nil))
  (state :uninitialized :type (member :uninitialized :initialized :destroyed))
  (face)
  (font)
  (metrics)
  (feature-settings)
  (variation-settings))

(defun font-cache-initialized-p (cache)
  (eq (font-cache-state cache) :initialized))

(defun resize-font-cache (cache new-size)
  (%blend2d:font-reset (font-cache-font cache))
  (%blend2d:font-create-from-face-with-settings (font-cache-font cache)
                                                (font-cache-face cache)
                                                new-size
                                                (font-cache-feature-settings cache)
                                                (font-cache-variation-settings cache))
  (%blend2d:font-get-metrics (font-cache-font cache) (font-cache-metrics cache))
  (setf (font-cache-state cache) :initialized)
  (values))

(defun make-font-cache* (font-face)
  (let ((metrics (autowrap:alloc '%blend2d:font-metrics))
        (font (autowrap:alloc '%blend2d:font-core))
        (feature-settings (autowrap:alloc '%blend2d:font-feature-settings-core))
        (variation-settings (autowrap:alloc '%blend2d:font-variation-settings-core)))
    (%blend2d:font-init font)
    (%blend2d:font-feature-settings-init feature-settings)
    (%blend2d:font-variation-settings-init variation-settings)
    (make-font-cache :face font-face
                     :font font
                     :metrics metrics
                     :feature-settings feature-settings
                     :variation-settings variation-settings)))

(defun free-font-cache (cache)
  (setf (font-cache-state cache) :destroyed)
  (%blend2d:font-destroy (font-cache-font cache))
  (autowrap:free (font-cache-font cache))
  (autowrap:free (font-cache-metrics cache))
  (%blend2d:font-feature-settings-destroy (font-cache-feature-settings cache))
  (autowrap:free (font-cache-feature-settings cache))
  (%blend2d:font-variation-settings-destroy (font-cache-variation-settings cache))
  (autowrap:free (font-cache-variation-settings cache)))


;; Utility

(defun measure-text (font-cache glyph-buffer text-metrics text)
  (cffi:with-foreign-string ((text-ptr text-bsize) text)
    (%blend2d:glyph-buffer-clear glyph-buffer)
    (%blend2d:glyph-buffer-set-text glyph-buffer
                                    text-ptr
                                    (1- text-bsize)
                                    %blend2d:+text-encoding-utf8+))
  (%blend2d:font-shape (font-cache-font font-cache) glyph-buffer)
  (%blend2d:font-get-text-metrics (font-cache-font font-cache)
                                  glyph-buffer
                                  text-metrics)
  (values))


;;; Label state

(deftype text-alignment () '(member :start :center :end))

(defstruct (label (:copier nil) (:constructor make-label*))
  (font-cache)
  (text "")
  (text-style)
  (text-lines (make-array 0 :adjustable t :fill-pointer 0))
  (origin (autowrap:alloc '%blend2d:point))
  (id 0 :type (unsigned-byte 64))
  (align-horz :start :type text-alignment) ; todo: rename horz/vert to x/y
  (align-vert :start :type text-alignment)
  (measure-func-ffi-cif)
  (measure-func-ffi-args)
  (measure-func-ffi-closure)
  (measure-func-pointer)
  (requires-measure :yes :type (member :yes :no :if-size-changed))
  (node-width 0.0 :type single-float) ; todo: use common size-changed checking instead of custom for labels
  (node-height 0.0 :type single-float)
  (wrap :whitespace :type (member :no :whitespace :anywhere))
  (overflow :clip :type (member :text-last :text-any :clip))
  (overflow-text-width 0.0d0 :type double-float)
  (overflow-text-gb nil))


;;; Label line builder

(defstruct (line-builder (:copier nil))
  (glyph-buffer (let ((gb (autowrap:alloc '%blend2d:glyph-buffer-core))) (%blend2d:glyph-buffer-init gb) gb))
  (glyph-buffer-next (let ((gb (autowrap:alloc '%blend2d:glyph-buffer-core))) (%blend2d:glyph-buffer-init gb) gb))
  (text "")
  (text-metrics (autowrap:alloc '%blend2d:text-metrics))
  (text-metrics-next (autowrap:alloc '%blend2d:text-metrics))
  (extra-chars (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
  (first-word t :type boolean)
  (previous-separator nil :type (or null character))
  (previous-separator-index 0 :type fixnum)
  (first-char-idx 0 :type fixnum)
  (next-char-idx 0 :type fixnum))

(defun whitespace-character-p (c)
  (or (char= c #\Space)
      (not (graphic-char-p c))))

(defun line-builder-push (builder font max-width c on-full wrap)
  (incf (line-builder-next-char-idx builder))
  (cond
   ((eq :anywhere wrap)
     (if (char= #\Newline c)
         (progn
          (if (= (length (line-builder-extra-chars builder)) 0)
              (funcall on-full nil nil (line-builder-first-char-idx builder) 1) ; Empty lines may be required for selection / editing
              (progn
               (funcall on-full
                 (line-builder-glyph-buffer builder)
                 (line-builder-text-metrics builder)
                 (line-builder-first-char-idx builder)
                 (- (line-builder-next-char-idx builder) (line-builder-first-char-idx builder)))
               (setf (line-builder-first-char-idx builder) (line-builder-next-char-idx builder))
               (setf (line-builder-glyph-buffer builder) (autowrap:alloc '%blend2d:glyph-buffer-core))
               (%blend2d:glyph-buffer-init (line-builder-glyph-buffer builder))
               (setf (fill-pointer (line-builder-extra-chars builder)) 0)))
          (setf (line-builder-first-word builder) t))
         (progn
          (vector-push-extend c (line-builder-extra-chars builder))
          (let ((new-text-width (progn
                                 (measure-text font
                                               (line-builder-glyph-buffer-next builder)
                                               (line-builder-text-metrics-next builder)
                                               (line-builder-extra-chars builder))
                                 (coerce (%blend2d:text-metrics.advance.x (line-builder-text-metrics-next builder)) 'single-float))))
            (if (or (line-builder-first-word builder) (<= new-text-width max-width))
                (progn
                 (setf (line-builder-first-word builder) nil)
                 (rotatef (line-builder-glyph-buffer builder) (line-builder-glyph-buffer-next builder))
                 (rotatef (line-builder-text-metrics builder) (line-builder-text-metrics-next builder)))
                (progn
                 (funcall on-full
                   (line-builder-glyph-buffer builder)
                   (line-builder-text-metrics builder)
                   (line-builder-first-char-idx builder)
                   (- (line-builder-next-char-idx builder) 1 (line-builder-first-char-idx builder)))
                 (setf (line-builder-first-char-idx builder) (1- (line-builder-next-char-idx builder)))
                 (setf (fill-pointer (line-builder-extra-chars builder)) 0)
                 (vector-push-extend c (line-builder-extra-chars builder))
                 (setf (line-builder-first-word builder) nil)
                 (setf (line-builder-glyph-buffer builder) (autowrap:alloc '%blend2d:glyph-buffer-core))
                 (%blend2d:glyph-buffer-init (line-builder-glyph-buffer builder))
                 (measure-text font
                               (line-builder-glyph-buffer builder)
                               (line-builder-text-metrics builder)
                               (line-builder-extra-chars builder))))))))
   ((eq :whitespace wrap)
     (if (not (whitespace-character-p c))
         (vector-push-extend c (line-builder-extra-chars builder))
         (progn
          (let* ((new-text (concatenate 'string
                             (line-builder-text builder)
                             (if (line-builder-first-word builder)
                                 ""
                                 (list (line-builder-previous-separator builder)))
                             (line-builder-extra-chars builder)))
                 (new-text-width (progn
                                  (measure-text font
                                                (line-builder-glyph-buffer-next builder)
                                                (line-builder-text-metrics-next builder)
                                                new-text)
                                  (coerce (%blend2d:text-metrics.advance.x (line-builder-text-metrics-next builder)) 'single-float))))
            (if (or (line-builder-first-word builder) (<= new-text-width max-width))
                (progn
                 (setf (line-builder-text builder) new-text)
                 (setf (line-builder-first-word builder) nil)
                 (setf (line-builder-previous-separator builder) c)
                 (setf (line-builder-previous-separator-index builder) (1- (line-builder-next-char-idx builder)))
                 (setf (fill-pointer (line-builder-extra-chars builder)) 0)
                 (rotatef (line-builder-glyph-buffer builder) (line-builder-glyph-buffer-next builder))
                 (rotatef (line-builder-text-metrics builder) (line-builder-text-metrics-next builder)))
                (progn
                 (funcall on-full
                   (line-builder-glyph-buffer builder)
                   (line-builder-text-metrics builder)
                   (line-builder-first-char-idx builder)
                   (- (1+ (line-builder-previous-separator-index builder)) (line-builder-first-char-idx builder)))
                 (setf (line-builder-first-char-idx builder) (1+ (line-builder-previous-separator-index builder)))
                 (setf (line-builder-text builder) (line-builder-extra-chars builder))
                 (setf (line-builder-extra-chars builder) (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
                 (setf (line-builder-first-word builder) nil) ; wrapped text becomes the first word (on a new line)
                 (setf (line-builder-previous-separator builder) c)
                 (setf (line-builder-previous-separator-index builder) (1- (line-builder-next-char-idx builder)))
                 (setf (line-builder-glyph-buffer builder) (autowrap:alloc '%blend2d:glyph-buffer-core))
                 (%blend2d:glyph-buffer-init (line-builder-glyph-buffer builder))
                 ;; TODO: make "current" glyph buffer / metrics lazy
                 (measure-text font
                               (line-builder-glyph-buffer builder)
                               (line-builder-text-metrics builder)
                               (line-builder-text builder)))))
          (when (char= c #\Newline)
                (if (= (length (line-builder-text builder)) 0)
                    (funcall on-full nil nil (line-builder-first-char-idx builder) 0) ; Empty lines may be required for selection / editing
                    (progn
                     (funcall on-full
                       (line-builder-glyph-buffer builder)
                       (line-builder-text-metrics builder)
                       (line-builder-first-char-idx builder)
                       (- (line-builder-next-char-idx builder) (line-builder-first-char-idx builder)))
                     (setf (line-builder-first-char-idx builder) (line-builder-next-char-idx builder))
                     (setf (line-builder-text builder) "")
                     (setf (line-builder-glyph-buffer builder) (autowrap:alloc '%blend2d:glyph-buffer-core))
                     (%blend2d:glyph-buffer-init (line-builder-glyph-buffer builder))))
                (setf (line-builder-first-word builder) t)
                (setf (line-builder-previous-separator builder) nil)))))
   (t (error "unreachable")))
  (values))

(defun line-builder-flush (builder font text-length on-full wrap)
  (let ((moved-glyph-buffer nil))

    (ecase wrap
      (:anywhere (when (< 0 (length (line-builder-extra-chars builder)))
                       (funcall on-full
                         (line-builder-glyph-buffer builder)
                         (line-builder-text-metrics builder)
                         (line-builder-first-char-idx builder)
                         (- text-length (line-builder-first-char-idx builder)))
                       (setf moved-glyph-buffer t)))
      (:whitespace (when (or (< 0 (length (line-builder-extra-chars builder)))
                             (not (line-builder-first-word builder)))
                         (measure-text font
                                       (line-builder-glyph-buffer builder)
                                       (line-builder-text-metrics builder)
                                       (concatenate 'string
                                         (line-builder-text builder)
                                         (if (line-builder-first-word builder)
                                             ""
                                             (list (line-builder-previous-separator builder)))
                                         (line-builder-extra-chars builder)))
                         (funcall on-full
                           (line-builder-glyph-buffer builder)
                           (line-builder-text-metrics builder)
                           (line-builder-first-char-idx builder)
                           (- text-length (line-builder-first-char-idx builder)))
                         (setf moved-glyph-buffer t))))
    (unless moved-glyph-buffer
      (%blend2d:glyph-buffer-destroy (line-builder-glyph-buffer builder))
      (autowrap:free (line-builder-glyph-buffer builder))))
  (autowrap:free (line-builder-text-metrics builder))
  (%blend2d:glyph-buffer-destroy (line-builder-glyph-buffer-next builder))
  (autowrap:free (line-builder-glyph-buffer-next builder))
  (autowrap:free (line-builder-text-metrics-next builder))
  (values))

(defun build-lines (label width-bound new-line-callback wrap)
  (if (eq :no wrap)
      (let ((line-text (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character))
            (text-metrics (autowrap:alloc '%blend2d:text-metrics))
            (line-first-char-idx 0)
            (next-char-idx 0))
        (loop #:for c #:across (label-text label) #:do
              (incf next-char-idx)
              (if (char= #\Newline c)
                  (let ((glyph-buffer (autowrap:alloc '%blend2d:glyph-buffer-core)))
                    (%blend2d:glyph-buffer-init glyph-buffer)
                    (measure-text (label-font-cache label) glyph-buffer text-metrics line-text)
                    (funcall new-line-callback glyph-buffer text-metrics line-first-char-idx (- next-char-idx line-first-char-idx))
                    (setf (fill-pointer line-text) 0)
                    (setf line-first-char-idx next-char-idx))
                  (vector-push-extend c line-text)))
        (when (< 0 (length line-text))
              (let ((glyph-buffer (autowrap:alloc '%blend2d:glyph-buffer-core)))
                (%blend2d:glyph-buffer-init glyph-buffer)
                (measure-text (label-font-cache label) glyph-buffer text-metrics line-text)
                (funcall new-line-callback glyph-buffer text-metrics line-first-char-idx (- next-char-idx line-first-char-idx))))
        (autowrap:free text-metrics))
      (let ((llb (make-line-builder)))
        (loop #:for c #:across (label-text label) #:do
              (line-builder-push llb (label-font-cache label) width-bound c new-line-callback wrap))
        (line-builder-flush llb (label-font-cache label) (length (label-text label)) new-line-callback wrap))))


;;; Rest

(defstruct (label-text-line (:copier nil))
  (text-offset 0 :type fixnum)
  (text-length 0 :type fixnum)
  (glyph-buffer)
  (x-offset 0.0d0 :type double-float)
  (y-offset 0.0d0 :type double-float)
  (width 0.0d0 :type double-float))

(defun label-clear-lines (label)
  (dotimes (i (fill-pointer (label-text-lines label)))
    (let ((gb (label-text-line-glyph-buffer (aref (label-text-lines label) i))))
      (%blend2d:glyph-buffer-destroy gb)
      (autowrap:free gb))
    (setf (aref (label-text-lines label) i) nil))
  (setf (fill-pointer (label-text-lines label)) 0)
  (values))

(defvar *labels* (make-hash-table))
(defvar *label-counter* 0)
(declaim (type (unsigned-byte 64) *label-counter*))

(declaim (ftype (function (label single-float fixnum single-float fixnum) (values single-float single-float)) label-measure-func-impl))
(defun label-measure-func-impl (label width-bound width-mode height-bound height-mode)
  (label-clear-lines label)
  (when (not (font-cache-initialized-p (label-font-cache label)))
        (error "Font cache not initialized when running label measure func."))
  (setf (label-requires-measure label) :no)
  (cond
   ((or (= width-mode yogalayout:+measure-mode-undefined+))
     (let ((glyph-buffer (autowrap:alloc '%blend2d:glyph-buffer-core))
           (text-metrics (autowrap:alloc '%blend2d:text-metrics)))
       (%blend2d:glyph-buffer-init glyph-buffer)
       (measure-text (label-font-cache label) glyph-buffer text-metrics (label-text label))
       (let ((line-width (%blend2d:text-metrics.advance.x text-metrics)))
         (vector-push-extend (make-label-text-line :glyph-buffer glyph-buffer
                                                   :width line-width
                                                   :y-offset (coerce (- (%blend2d:font-metrics.y-min (font-cache-metrics (label-font-cache label)))) 'double-float))
                             (label-text-lines label))
         (multiple-value-prog1
             (values
               (coerce line-width 'single-float)
               (coerce (- (%blend2d:font-metrics.y-max (font-cache-metrics (label-font-cache label)))
                          (%blend2d:font-metrics.y-min (font-cache-metrics (label-font-cache label))))
                       'single-float))
           (autowrap:free text-metrics)))))
   (t (progn
       (let* ((y-offset 0.0d0)
              (max-line-width 0.0d0)
              (new-line-callback (lambda (gb tm t0 tn)
                                   (unless (= y-offset 0.0d0)
                                     (incf y-offset (%blend2d:font-metrics.line-gap (font-cache-metrics (label-font-cache label)))))
                                   ;; `decf` because y-min is negative (aka relative to baseline with Y-up)
                                   (decf y-offset (%blend2d:font-metrics.y-min (font-cache-metrics (label-font-cache label))))
                                   (let* ((line-width (if tm (%blend2d:text-metrics.advance.x tm) 0.0d0)))
                                     (setf max-line-width (max max-line-width line-width))
                                     (vector-push-extend (make-label-text-line :text-offset t0
                                                                               :text-length tn
                                                                               :glyph-buffer gb
                                                                               :width line-width
                                                                               :y-offset y-offset) (label-text-lines label)))
                                   (incf y-offset (%blend2d:font-metrics.y-max (font-cache-metrics (label-font-cache label)))))))
         (build-lines label width-bound new-line-callback (label-wrap label))
         (let ((width (if (= width-mode yogalayout:+measure-mode-exactly+) width-bound max-line-width))
               (height (if (= height-mode yogalayout:+measure-mode-exactly+) height-bound y-offset)))
           (unless (and (eq (label-align-horz label) :start)
                        (eq (label-align-vert label) :start))
             (let* ((remaining-height (- height y-offset))
                    (extra-y-offset (ecase (label-align-vert label)
                                      (:start 0.0d0)
                                      (:center (* 0.5d0 remaining-height))
                                      (:end remaining-height))))
               (loop for text-line across (label-text-lines label)
                     do (setf (label-text-line-x-offset text-line)
                          (let ((remaining-width (- width (label-text-line-width text-line))))
                            (ecase (label-align-horz label)
                              (:start 0.0d0)
                              (:center (* 0.5d0 remaining-width))
                              (:end remaining-width))))
                       (incf (label-text-line-y-offset text-line) extra-y-offset))))
           (values (coerce width 'single-float) (coerce height 'single-float))))))))

(autowrap:defcallback label-measure-func-cb :void ((cif :pointer) (ret :pointer) (args :pointer) (userdata :pointer))
  (declare (ignore cif))
  (multiple-value-bind (width height)
      (label-measure-func-impl (gethash (cffi:pointer-address userdata) *labels*)
                               (cffi:mem-ref (cffi:mem-aref args :pointer 1) :float)
                               (cffi:mem-ref (cffi:mem-aref args :pointer 2) :int)
                               (cffi:mem-ref (cffi:mem-aref args :pointer 3) :float)
                               (cffi:mem-ref (cffi:mem-aref args :pointer 4) :int))

    (setf ret (autowrap:wrap-pointer ret '%yogalayout:size))
    (setf (%yogalayout:size.width ret) width)
    (setf (%yogalayout:size.height ret) height)))

(defun destroy-label (label)
  (remhash (label-id label) *labels*)
  (destroy-style (label-text-style label))
  (label-clear-lines label)
  (free-font-cache (label-font-cache label))
  (autowrap.libffi:ffi-closure-free (label-measure-func-ffi-closure label))
  (autowrap:free (label-measure-func-ffi-cif label))
  (autowrap:free (label-measure-func-ffi-args label))
  (autowrap:free (label-origin label))
  (when (label-overflow-text-gb label)
        (%blend2d:glyph-buffer-destroy (label-overflow-text-gb label))
        (autowrap:free (label-overflow-text-gb label))))

(assert (= (cffi:foreign-type-size :int) 4)) ; checks int = sint32
(defun make-label (font-face)
  (let* ((bound (autowrap:calloc :pointer))
         (label (make-label* :font-cache (make-font-cache* font-face)
                             :id (prog1 *label-counter* (incf *label-counter*))
                             :measure-func-ffi-cif (autowrap:calloc 'autowrap.libffi:ffi-cif)
                             :measure-func-ffi-args (autowrap:calloc :pointer 5)
                             ;; `(autowrap:sizeof 'autowrap.libffi:ffi-closure)` returns 48 for me, causing crashes
                             :measure-func-ffi-closure (autowrap.libffi:ffi-closure-alloc 56 (autowrap:ptr bound)))))
    (setf (autowrap:c-aref (label-measure-func-ffi-args label) 0 :pointer) autowrap.libffi:ffi-type-pointer)
    (setf (autowrap:c-aref (label-measure-func-ffi-args label) 1 :pointer) autowrap.libffi:ffi-type-float)
    (setf (autowrap:c-aref (label-measure-func-ffi-args label) 2 :pointer) autowrap.libffi:ffi-type-sint32) ; assumes int = sint32
    (setf (autowrap:c-aref (label-measure-func-ffi-args label) 3 :pointer) autowrap.libffi:ffi-type-float)
    (setf (autowrap:c-aref (label-measure-func-ffi-args label) 4 :pointer) autowrap.libffi:ffi-type-sint32) ; assumes int = sint32
    (autowrap.libffi:ffi-prep-cif (label-measure-func-ffi-cif label)
                                  autowrap.libffi:+ffi-default-abi+
                                  5
                                  (autowrap::ensure-libffi-type (autowrap:find-type '%yogalayout:size))
                                  (label-measure-func-ffi-args label))
    (autowrap.libffi:ffi-prep-closure-loc (label-measure-func-ffi-closure label)
                                          (label-measure-func-ffi-cif label)
                                          (autowrap:callback 'label-measure-func-cb)
                                          (cffi:make-pointer (label-id label))
                                          (autowrap:c-aref bound 0 :pointer))
    (setf (label-measure-func-pointer label) (autowrap:c-aref bound 0 :pointer))
    (autowrap:free bound)
    (setf (gethash (label-id label) *labels*) label)
    label))

(defun label-set-font-features (label font-features)
  (%blend2d:font-feature-settings-clear (font-cache-feature-settings (label-font-cache label)))
  (dolist (font-feature font-features)
    (check-type (car font-feature) (unsigned-byte 32))
    (check-type (cdr font-feature) (unsigned-byte 32))
    (%blend2d:font-feature-settings-set-value (font-cache-feature-settings (label-font-cache label))
                                              (car font-feature)
                                              (cdr font-feature)))
  (%blend2d:font-set-feature-settings (font-cache-font (label-font-cache label))
                                      (font-cache-feature-settings (label-font-cache label))))

(defun label-set-font-variation (label font-variation)
  (%blend2d:font-variation-settings-clear (font-cache-variation-settings (label-font-cache label)))
  (dolist (font-variation-value font-variation)
    (check-type (car font-variation-value) (unsigned-byte 32))
    (check-type (cdr font-variation-value) single-float)
    (%blend2d:font-variation-settings-set-value (font-cache-variation-settings (label-font-cache label))
                                                (car font-variation-value)
                                                (cdr font-variation-value)))
  (%blend2d:font-set-variation-settings (font-cache-font (label-font-cache label))
                                        (font-cache-variation-settings (label-font-cache label))))

(defun label-set-overflow (label overflow overflow-text)
  (if (eq :clip overflow)
      (progn
       (unless (eq :clip (label-overflow label))
         (%blend2d:glyph-buffer-destroy (label-overflow-text-gb label))
         (autowrap:free (label-overflow-text-gb label)))
       (setf (label-overflow label) :clip))
      (progn
       (assert (stringp overflow-text))
       (when (eq :clip (label-overflow label))
             (setf (label-overflow-text-gb label) (autowrap:alloc '%blend2d:glyph-buffer-core))
             (%blend2d:glyph-buffer-init (label-overflow-text-gb label)))
       (let ((text-metrics (autowrap:alloc '%blend2d:text-metrics)))
         (measure-text (label-font-cache label) (label-overflow-text-gb label) text-metrics overflow-text)
         (setf (label-overflow-text-width label) (%blend2d:text-metrics.bounding-box.x1 text-metrics))
         (autowrap:free text-metrics))
       (setf (label-overflow label) overflow))))

(defun initialize-label (ui label
                            &key
                            widget (mark-dirty nil)
                            text text-style
                            font-size font-features font-variation
                            align-horz align-vert
                            wrap
                            overflow
                            overflow-text)
  (let ((font-size-computed (init-computed-prop widget font-size)))
    (sdet:make-effect (ui-sdet-context ui)
      (when (eq (font-cache-state (label-font-cache label)) :destroyed)
            (error "Font cache was destroyed before running its resizing effect."))
      (setf (label-requires-measure label) :yes)
      (resize-font-cache (label-font-cache label) (sdet:compute font-size-computed))
      nil))
  (when wrap
        (let ((wrap-computed (init-computed-prop widget wrap)))
          (sdet:make-effect (ui-sdet-context ui)
            (setf (label-wrap label) (sdet:compute wrap-computed))
            (setf (label-requires-measure label) :yes)
            (when mark-dirty (yogalayout:node-mark-dirty (widget-yoga-node widget)))
            nil)))
  (when overflow
        (let ((overflow-computed (init-computed-prop widget overflow))
              (overflow-text-computed (init-computed-prop widget overflow-text)))
          (sdet:make-effect (ui-sdet-context ui)
            (label-set-overflow label (sdet:compute overflow-computed) (sdet:compute overflow-text-computed))
            (setf (label-requires-measure label) :yes)
            (when mark-dirty (yogalayout:node-mark-dirty (widget-yoga-node widget)))
            nil)))
  (let ((text-computed (init-computed-prop widget text)))
    (sdet:make-effect (ui-sdet-context ui)
      (setf (label-text label) (sdet:compute text-computed))
      (setf (label-requires-measure label) :yes)
      (when mark-dirty (yogalayout:node-mark-dirty (widget-yoga-node widget)))
      nil))
  (when (or align-horz align-vert)
        (let ((align-horz-computed (init-computed-prop widget align-horz))
              (align-vert-computed (init-computed-prop widget align-vert)))
          (sdet:make-effect (ui-sdet-context ui)
            (setf (label-align-horz label) (sdet:compute align-horz-computed))
            (setf (label-align-vert label) (sdet:compute align-vert-computed))
            (setf (label-requires-measure label) :yes)
            (when mark-dirty (yogalayout:node-mark-dirty (widget-yoga-node widget)))
            nil)))
  (when font-features
        (let ((font-features-computed (init-computed-prop widget font-features)))
          (sdet:make-effect (ui-sdet-context ui)
            (label-set-font-features label (sdet:compute font-features-computed))
            (setf (label-requires-measure label) :yes)
            (when mark-dirty (yogalayout:node-mark-dirty (widget-yoga-node widget)))
            nil)))
  (when font-variation
        (let ((font-variation-computed (init-computed-prop widget font-variation)))
          (sdet:make-effect (ui-sdet-context ui)
            (label-set-font-variation label (sdet:compute font-variation-computed))
            (setf (label-requires-measure label) :yes)
            (when mark-dirty (yogalayout:node-mark-dirty (widget-yoga-node widget)))
            nil)))
  (let ((text-style-computed (init-computed-prop widget text-style)))
    (sdet:make-effect (ui-sdet-context ui)
      (setf (label-text-style label) (create-style (label-text-style label) (sdet:compute text-style-computed)))
      nil))
  (values))

(declaim (ftype (function (ui double-float double-float single-float single-float label) (values)) render-label))
(defun render-label (ui x y width height label)
  (when (not (font-cache-initialized-p (label-font-cache label)))
        (error "Font cache not initialized when rendering label."))
  (ecase (label-requires-measure label)
    (:no (setf (label-requires-measure label) :if-size-changed))
    (:yes
     (label-measure-func-impl label width yogalayout:+measure-mode-exactly+ height yogalayout:+measure-mode-exactly+))
    (:if-size-changed
     (when (or (/= width (label-node-width label)) (/= height (label-node-height label)))
           (label-measure-func-impl label width yogalayout:+measure-mode-exactly+ height yogalayout:+measure-mode-exactly+))))
  (setf (label-node-width label) width)
  (setf (label-node-height label) height)
  ; (when size-changed
  ;       (update-style-values* (label-text-style label) x y width height))
  (let ((clip-rect (autowrap:alloc '%blend2d:rect))
        (clip-state :full))
    (setf (%blend2d:rect.x clip-rect) x)
    (setf (%blend2d:rect.y clip-rect) y)
    (setf (%blend2d:rect.w clip-rect) (coerce width 'double-float))
    (setf (%blend2d:rect.h clip-rect) (coerce height 'double-float))
    (%blend2d:context-clip-to-rect-d (layer-context (ui-temp-layer ui)) clip-rect)
    (blend2d-context-set-fill-style (layer-context (ui-temp-layer ui)) (label-text-style label))
    (dotimes (line-index (length (label-text-lines label)))
      (let ((text-line (aref (label-text-lines label) line-index)))
        (when (and (label-text-line-glyph-buffer text-line)
                   (< (label-text-line-y-offset text-line) height))
              (if (or
                   (and (eq :text-any (label-overflow label))
                        (> (label-text-line-width text-line) width))
                   (and (eq :text-last (label-overflow label))
                        (or (and (= line-index (1- (length (label-text-lines label))))
                                 (> (label-text-line-width text-line) width))
                            (and (< (1+ line-index) (length (label-text-lines label)))
                                 (>= (label-text-line-y-offset (aref (label-text-lines label) (1+ line-index))) height)))))
                  (progn
                   (unless (eq :full clip-state)
                     (setf (%blend2d:rect.w clip-rect) (coerce width 'double-float))
                     (%blend2d:context-restore-clipping (layer-context (ui-temp-layer ui)))
                     (%blend2d:context-clip-to-rect-d (layer-context (ui-temp-layer ui)) clip-rect)
                     (setf clip-state :full))
                   (setf (%blend2d:point.x (label-origin label)) (+ x width (- (label-overflow-text-width label))))
                   (setf (%blend2d:point.y (label-origin label)) (+ y (label-text-line-y-offset text-line)))
                   (%blend2d:context-fill-glyph-run-d (layer-context (ui-temp-layer ui))
                                                      (label-origin label)
                                                      (font-cache-font (label-font-cache label))
                                                      (%blend2d:glyph-buffer-get-glyph-run (label-overflow-text-gb label)))
                   ;; always (eq :full clip-state)
                   ;; don't restore because :full clip state > :overflow clip state
                   (setf (%blend2d:rect.w clip-rect) (- (coerce width 'double-float)
                                                        (label-overflow-text-width label)))
                   (%blend2d:context-clip-to-rect-d (layer-context (ui-temp-layer ui)) clip-rect)
                   (setf clip-state :overflow))
                  (progn
                   (unless (eq :full clip-state)
                     (setf (%blend2d:rect.w clip-rect) (coerce width 'double-float))
                     (%blend2d:context-restore-clipping (layer-context (ui-temp-layer ui)))
                     (%blend2d:context-clip-to-rect-d (layer-context (ui-temp-layer ui)) clip-rect)
                     (setf clip-state :full))))
              (setf (%blend2d:point.x (label-origin label)) (+ x (label-text-line-x-offset text-line)))
              (setf (%blend2d:point.y (label-origin label)) (+ y (label-text-line-y-offset text-line)))
              (%blend2d:context-fill-glyph-run-d (layer-context (ui-temp-layer ui))
                                                 (label-origin label)
                                                 (font-cache-font (label-font-cache label))
                                                 (%blend2d:glyph-buffer-get-glyph-run (label-text-line-glyph-buffer text-line))))))

    (%blend2d:context-restore-clipping (layer-context (ui-temp-layer ui)))
    (autowrap:free clip-rect))
  (values))

(defstruct (label-widget (:include widget)
                         (:copier nil)
                         (:constructor make-label-widget (label)))
  (label))

(defun w-label-impl (ui &key
                        set-layout z-index position-type
                        text text-style
                        font font-size font-features font-variation
                        align-horz align-vert
                        wrap
                        overflow
                        overflow-text)
  (let ((widget (make-label-widget (make-label font))))
    (initialize-widget ui widget :z-index z-index :position-type position-type)
    (yogalayout:node-set-node-type (widget-yoga-node widget) yogalayout:+node-type-text+)
    (yogalayout:node-set-measure-func (widget-yoga-node widget) (label-measure-func-pointer (label-widget-label widget)))
    ; (yogalayout:node-set-baseline-func (widget-yoga-node widget) TODO)
    (setf (widget-cursor widget)
      (lambda (ui widget x y)
        (declare (ignore ui))
        (loop #:for tl #:across (label-text-lines (label-widget-label widget))
              #:do
              (when (and (>= x (+ (widget-layer-x widget) (label-text-line-x-offset tl)))
                         (<= x (+ (widget-layer-x widget) (label-text-line-x-offset tl) (label-text-line-width tl)))
                         (>= y (+ (widget-layer-y widget) (label-text-line-y-offset tl) (%blend2d:font-metrics.y-min (font-cache-metrics (label-font-cache (label-widget-label widget))))))
                         (<= y (+ (widget-layer-y widget) (label-text-line-y-offset tl) (%blend2d:font-metrics.y-max (font-cache-metrics (label-font-cache (label-widget-label widget)))))))
                    (return :text)))))
    (initialize-label ui
                      (label-widget-label widget)
                      :widget widget
                      :mark-dirty t
                      :text text
                      :text-style text-style
                      :font-size font-size
                      :font-features font-features
                      :font-variation font-variation
                      :align-horz align-horz
                      :align-vert align-vert
                      :wrap wrap
                      :overflow overflow
                      :overflow-text overflow-text)
    (when set-layout (funcall set-layout widget))
    (setf (widget-on-render-begin widget)
      (lambda (ui widget)
        (render-label ui
                      (coerce (widget-yoga-x widget) 'double-float)
                      (coerce (widget-yoga-y widget) 'double-float)
                      (widget-width widget)
                      (widget-height widget)
                      (label-widget-label widget))
        (values)))
    (lambda ()
      (destroy-label (label-widget-label widget))
      (destroy-widget widget)
      (values))))

(defmacro w-label (&key ui layout let z-index position-type
                        text (text-style #xFFFFFFFF)
                        font (font-size 12.0f0) (font-features '()) (font-variation '())
                        (align-horz :start) (align-vert :start)
                        (wrap :whitespace) (overflow :clip) (overflow-text " ... "))
  (assert (and text font))
  (macroexpand-with-ui ui
    `(w-label-impl ,*ui*
                   :set-layout ,(make-layout-setting-lambda *ui* layout)
                   :z-index ,(make-computed-prop z-index :let let)
                   :position-type ,(make-computed-prop position-type :let let)
                   :text ,(make-computed-prop text :let let)
                   :text-style ,(make-computed-prop text-style :let let)
                   :font ,font
                   :font-size ,(make-computed-prop font-size :let let)
                   :font-features ,(make-computed-prop font-features :let let)
                   :font-variation ,(make-computed-prop font-variation :let let)
                   :align-horz ,(make-computed-prop align-horz :let let)
                   :align-vert ,(make-computed-prop align-vert :let let)
                   :wrap ,(make-computed-prop wrap :let let)
                   :overflow ,(make-computed-prop overflow :let let)
                   :overflow-text ,(make-computed-prop overflow-text :let let))))

;;; Visual

(defstruct (visual-description-text (:include visual-description)
                                    (:copier nil)
                                    (:constructor visual-text (&key
                                                               font text style font-size font-features font-variation
                                                               align-horz align-vert wrap overflow overflow-text
                                                               &aux
                                                               (update #'visual-description-text-update-impl))))
  font
  (text "" :type string)
  (style #xFFFFFFFF)
  (font-size 12.0f0 :type single-float)
  (font-features '())
  (font-variation '())
  (align-horz :center)
  (align-vert :center)
  (wrap :anywhere)
  (overflow :clip)
  (overflow-text " ... " :type string))

(defstruct (visual-state-text (:include visual-state)
                              (:copier nil)
                              (:constructor make-visual-state-text (label &aux
                                                                          (render #'visual-state-text-render-impl)
                                                                          (destroy #'visual-state-text-destroy-impl))))
  (label))

(defun visual-description-text-aux (label vdescr)
  (resize-font-cache (label-font-cache label) (visual-description-text-font-size vdescr))
  (setf (label-text label) (visual-description-text-text vdescr))
  (setf (label-align-horz label) (visual-description-text-align-horz vdescr))
  (setf (label-align-vert label) (visual-description-text-align-vert vdescr))
  (setf (label-wrap label) (visual-description-text-wrap vdescr))
  (setf (label-overflow label) (visual-description-text-overflow vdescr))
  (label-set-font-features label (visual-description-text-font-features vdescr))
  (label-set-font-variation label (visual-description-text-font-variation vdescr))
  (label-set-overflow label (visual-description-text-overflow vdescr) (visual-description-text-overflow-text vdescr))
  (values))

(defun visual-description-text-create-impl (vdescr)
  (let ((label (make-label (visual-description-text-font vdescr))))
    (visual-description-text-aux label vdescr)
    (setf (label-text-style label) (create-style nil (visual-description-text-style vdescr)))
    (make-visual-state-text label)))

(define-visual-description-update visual-description-text-update-impl
    (vdescr (vstate visual-state-text) :create-fn visual-description-text-create-impl)
  (let ((label (visual-state-text-label vstate)))
    (visual-description-text-aux label vdescr)
    (setf (label-text-style label)
      (create-style (label-text-style label)
                    (visual-description-text-style vdescr)))
    (setf (label-requires-measure label) :yes))
  (values))

(defun visual-state-text-render-impl (ui vstate x y width height)
  (render-label ui x y
                (coerce width 'single-float)
                (coerce height 'single-float)
                (visual-state-text-label vstate))
  (values))

(defun visual-state-text-destroy-impl (vstate)
  (destroy-label (visual-state-text-label vstate))
  (values))
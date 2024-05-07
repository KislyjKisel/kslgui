(in-package #:kslgui)

(defconstant +base-scroll-sensitivity-x+ 0.1d0)
(defconstant +base-scroll-sensitivity-y+ -0.1d0)

(defconstant +active-mouse-button-left+ 1)
(defconstant +active-mouse-button-middle+ 2)
(defconstant +active-mouse-button-right+ 4)

(declaim (type (unsigned-byte 3) +active-mouse-button-left+))
(declaim (type (unsigned-byte 3) +active-mouse-button-middle+))
(declaim (type (unsigned-byte 3) +active-mouse-button-right+))

(deftype key-action ()
  '(member :enter :backspace :delete
           :up :down :left :right))

;; Moving focus inside this widget:
;; CONTAIN - focus can't go outside the widget;
;; CYCLE - try navigating from the other side of the widget;
;; PASSTHROUGH - focus can be passed to its siblings.
;; CONTAIN-IF-ANY - if any widget is found inside - :CONTAIN, otherwise :PASSTHROUGH
(deftype focus-behavior-as-parent ()
  '(member :contain :contain-if-any :cycle :passthrough))

;; Moving focus to this widget:
;; :SKIP - skipped,
;; :FOCUS - takes focus,
;; :TO-CHILDREN - focus may be passed to its children.
(deftype focus-behavior-as-sibling ()
  '(member :skip :focus :to-children))

(deftype cursor ()
  '(member :default :none
           :text
           :press :release
           :grab :move :move-x :move-y
           :zoom-in :zoom-out
           :progress :forbidden))

(in-package #:kslgui)

(defconstant +base-scroll-sensitivity-x+ 0.1d0)
(defconstant +base-scroll-sensitivity-y+ -0.1d0)

(defconstant +active-mouse-button-left+ 1)
(defconstant +active-mouse-button-middle+ 2)
(defconstant +active-mouse-button-right+ 4)

(declaim (type (unsigned-byte 3) +active-mouse-button-left+))
(declaim (type (unsigned-byte 3) +active-mouse-button-middle+))
(declaim (type (unsigned-byte 3) +active-mouse-button-right+))

(export 'key-action)
(deftype key-action ()
  '(member :enter :backspace :delete
           :up :down :left :right
           :copy :paste))

;; Moving focus inside this widget:
;; CONTAIN - focus can't go outside the widget;
;; CYCLE - try navigating from the other side of the widget;
;; PASSTHROUGH - focus can be passed to its siblings.
;; CONTAIN-IF-ANY - if any widget is found inside - :CONTAIN, otherwise :PASSTHROUGH
(export 'focus-behavior-as-parent)
(deftype focus-behavior-as-parent ()
  '(member :contain :contain-if-any :cycle :passthrough))

;; Moving focus to this widget:
;; :SKIP - skipped,
;; :FOCUS - takes focus,
;; :TO-CHILDREN - focus may be passed to its children.
(export 'focus-behavior-as-sibling)
(deftype focus-behavior-as-sibling ()
  '(member :skip :focus :to-children))

(export 'cursor)
(deftype cursor ()
  '(member :default :none
           :text
           :press :release
           :grab :move :move-x :move-y
           :zoom-in :zoom-out
           :progress :forbidden))

(defconstant +key-modifier-shift-left+ (ash 1 0))
(defconstant +key-modifier-shift-right+ (ash 1 1))
(defconstant +key-modifier-ctrl-left+ (ash 1 2))
(defconstant +key-modifier-ctrl-right+ (ash 1 3))
(defconstant +key-modifier-alt-left+ (ash 1 4))
(defconstant +key-modifier-alt-right+ (ash 1 5))
(defconstant +key-modifier-alt-gr+ (ash 1 6))
(defconstant +key-modifier-gui-left+ (ash 1 7))
(defconstant +key-modifier-gui-right+ (ash 1 8))
(defconstant +key-modifier-num-lock+ (ash 1 9))
(defconstant +key-modifier-caps-lock+ (ash 1 10))
(defconstant +key-modifier-scroll-lock+ (ash 1 11))
(defconstant +key-modifier-ctrl+ (logior +key-modifier-ctrl-left+ +key-modifier-ctrl-right+))
(defconstant +key-modifier-shift+ (logior +key-modifier-shift-left+ +key-modifier-shift-right+))
(defconstant +key-modifier-alt+ (logior +key-modifier-alt-left+ +key-modifier-alt-right+))
(defconstant +key-modifier-gui+ (logior +key-modifier-gui-left+ +key-modifier-gui-right+))

(export 'key-modifier)
(deftype key-modifier ()
  '(unsigned-byte 12))

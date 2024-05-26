(in-package #:kslgui)

(defun directional-navigation-criteria-init ()
  nil)

(defun directional-navigation-criteria-next (center-x center-y radius-x radius-y
                                                      key-action state widget)
  (let* ((widget-radius-x (* 0.5 (widget-width widget)))
         (widget-radius-y (* 0.5 (widget-height widget)))
         (dx (- center-x (+ (widget-layer-x widget) widget-radius-x)))
         (dy (- center-y (+ (widget-layer-y widget) widget-radius-y))))
    (multiple-value-bind (distance-main distance-side radius-side widget-radius-side)
        (ecase key-action
          (:up (values dy dx radius-x widget-radius-x))
          (:down (values (- dy) dx radius-x widget-radius-x))
          (:left (values dx dy radius-y widget-radius-y))
          (:right (values (- dx) dy radius-y widget-radius-y)))
      (setf distance-side (abs distance-side))
      (if (and (> distance-main 0)
               (< distance-side (+ radius-side widget-radius-side))
               (or (null state)
                   (< distance-main (cadr state))
                   (and (= distance-main (cadr state)) (< distance-side (cddr state)))))
          (cons widget (cons distance-main distance-side))
          state))))

(defun directional-navigation-criteria-finish (state)
  (when state (car state)))

(defun widget-focus-behavior-as-parent (widget key-action)
  (if (or (eq key-action :left) (eq key-action :right))
      (widget-focus-behavior-as-parent-x widget)
      (widget-focus-behavior-as-parent-y widget)))

(defun directional-navigation-traverse-children (ui center-x center-y radius-x radius-y
                                                    key-action criteria-state parent skipped-widget)
  (loop #:for child #:across (widget-children parent) #:do
        (unless (or (not (widgetp child))
                    (eq child skipped-widget)
                    (sdet:unobserved (ui-sdet-context ui)
                      (not (sdet:compute (widget-enabled-computed child)))))
          (ecase (if (functionp (widget-focus-behavior-as-sibling child))
                     (funcall (widget-focus-behavior-as-sibling child) ui child)
                     (widget-focus-behavior-as-sibling child))
            (:skip nil)
            (:focus (setf criteria-state
                      (directional-navigation-criteria-next center-x center-y radius-x radius-y
                                                            key-action
                                                            criteria-state
                                                            child)))
            (:to-children (setf criteria-state
                            (directional-navigation-traverse-children ui
                                                                      center-x center-y radius-x radius-y
                                                                      key-action
                                                                      criteria-state
                                                                      child
                                                                      nil))))))
  criteria-state)

(defun directional-navigation-traverse (ui focused-widget
                                           center-x center-y
                                           radius-x radius-y
                                           key-action
                                           parent
                                           go-up)
  (let ((criteria-state (directional-navigation-criteria-init)))
    (setf criteria-state
      (directional-navigation-traverse-children ui
                                                center-x center-y radius-x radius-y
                                                key-action
                                                criteria-state
                                                parent
                                                focused-widget))
    (if (not go-up)
        (directional-navigation-criteria-finish criteria-state)
        (let ((parent-focus-behavior (widget-focus-behavior-as-parent parent key-action)))
          (loop
           ; #:with unscrolled-center-x = center-x
           ; #:with unscrolled-center-y = center-y
           #:do
           (when (functionp parent-focus-behavior)
                 (setf parent-focus-behavior (funcall parent-focus-behavior ui focused-widget key-action))
                 (when (or (not (symbolp parent-focus-behavior)) (null parent-focus-behavior))
                       (return-from directional-navigation-traverse parent-focus-behavior)))
           (ecase parent-focus-behavior
             (:contain (return (directional-navigation-criteria-finish criteria-state)))
             ((:passthrough :contain-if-any)
              (when (and (eq :contain-if-any parent-focus-behavior)
                         (directional-navigation-criteria-finish criteria-state))
                    (return (directional-navigation-criteria-finish criteria-state)))
              (unless (widget-parent parent) (return (directional-navigation-criteria-finish criteria-state)))
              ;  (setf unscrolled-center-x (+ unscrolled-center-x (widget-children-scroll-x parent)))
              ;  (setf unscrolled-center-y (+ unscrolled-center-y (widget-children-scroll-y parent)))
              (setf criteria-state
                (directional-navigation-traverse-children ui
                                                          center-x center-y
                                                          radius-x radius-y
                                                          key-action
                                                          criteria-state
                                                          (widget-parent parent)
                                                          parent))
              (setf parent (widget-parent parent))
              (setf parent-focus-behavior (widget-focus-behavior-as-parent parent key-action)))
             (:cycle
              (return (or (directional-navigation-criteria-finish criteria-state)
                          (multiple-value-bind (cycle-center-x cycle-center-y)
                              (ecase key-action
                                (:up (values center-x (+ (widget-layer-y parent) (widget-height parent))))
                                (:down (values center-x (widget-layer-y parent)))
                                (:left (values (+ (widget-layer-x parent) (widget-width parent)) center-y))
                                (:right (values (widget-layer-x parent) center-y)))
                            (directional-navigation-traverse ui focused-widget
                                                             cycle-center-x cycle-center-y
                                                             radius-x radius-y
                                                             key-action
                                                             parent
                                                             nil)))))))))))

(defun directional-navigation (ui focused-widget key-action)
  (let* ((radius-x (* 0.5 (widget-width focused-widget)))
         (radius-y (* 0.5 (widget-height focused-widget)))
         (center-x (+ (widget-layer-x focused-widget) radius-x))
         (center-y (+ (widget-layer-y focused-widget) radius-y)))
    (directional-navigation-traverse ui focused-widget
                                     center-x center-y
                                     radius-x radius-y
                                     key-action
                                     (widget-parent focused-widget)
                                     t)))

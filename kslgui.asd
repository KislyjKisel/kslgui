(asdf:defsystem #:kslgui
  :version "0.0.1"
  :pathname "src/"
  :depends-on (#:alexandria
               #:trivial-macroexpand-all
               #:cl-blend2d
               #:cl-yogalayout
               #:state-dependent-effect-tree)
  :serial t
  :components ((:file "package")
               (:file "static")
               (:file "util")
               (:file "layer")
               (:file "style")
               (:file "visual")
               (:file "sensor")
               (:file "cursor-renderer")
               (:file "widget-base")
               (:file "context")
               (:file "window")
               (:file "rendering")
               (:file "keyboard")
               (:file "mouse")
               (:file "directional-navigation")
               (:file "widget")
               (:module "visuals"
                :components ((:file "utils")
                             (:file "lambda")
                             (:file "path")
                             (:file "rectangle")))
               (:module "widgets"
                :components ((:file "block")
                             (:file "dynamic")
                             (:file "map-fix-index")
                             (:file "visual")
                             (:file "label")
                             (:file "scroll")
                             (:file "button")
                             (:file "textbox")
                             (:file "two-pane")))))

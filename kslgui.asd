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
                :components ((:file "rectangle")
                             (:file "path")))
               (:module "widgets"
                :components ((:file "block")
                             (:file "cond")
                             (:file "fragment")
                             (:file "visual")
                             (:file "label")
                             (:file "button")
                             (:file "scroll")
                             (:file "textbox")))))

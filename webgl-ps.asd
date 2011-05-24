
(defsystem :webgl-ps
  :depends-on (:parenscript)
  :serial t
  :components ((:file "package")
               (:file "webgl-bindings")
               (:file "webgl-enums")
               (:module "math"
                        :serial t
                        :components ((:file "mjs-bindings")
                                     ;;fixme: decide how to load PS code
                                     ;;(:file "quat")
                                     )))
  )
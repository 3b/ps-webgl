
(defsystem :ps-webgl
  :defsystem-depends-on (:3b-ps-asdf)
  :depends-on (:parenscript)
  :serial t
  :components ((:file "package")
               (:file "webgl-bindings")
               (:file "webgl-enums")
               (:file "host-utils")
               (:3b-ps-file "webgl")
               (:module "math"
                        :serial t
                        :components ((:file "mjs-bindings")
                                     ;;fixme: decide how to load PS code
                                     (:3b-ps-file "quat")
                                     )))
  )
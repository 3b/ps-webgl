(defpackage #:mjs-bindings
  (:use :cl :ps)
  (:export :*mjs-float-array-type*
           :vec
           :matrix
           #:*mjs-version*
           #:vec+
           #:vec-
           #:vec-negate
           #:vec-direction
           #:vec-length
           #:vec-length^2
           #:normalize
           #:vec*
           #:dot
           #:cross
           #:top-left-3x3
           #:inverse-orthonormal
           #:inverse-to-3x3
           #:make-frustum
           #:make-perspective
           #:make-ortho
           #:make-ortho-2d
           #:matrix*
           #:make-rotate
           #:rotate
           #:make-scale-3
           #:make-scale-1
           #:make-scale
           #:scale-3
           #:scale-1
           #:make-translate-3
           #:make-translate-1
           #:make-translate
           #:translate-3
           #:translate-1
           #:translate
           #:make-look-at
           #:transpose))
(in-package #:mjs-bindings)

(defparameter *context* '(:v3 *v3* :m4 *m4x4))

(ps:defpsmacro with-mjs-context ((&key (v3 '*v3*) (m4 '*m4x4)) &body body)
  `(symbol-macrolet ((*context* '(:v3 ,v3 :m4 ,m4)))
     ,@body))

(defmacro define-mjs-fun (object name &body args)
  ;; TODO: probably should define a symbol-macro to access the value
  ;;   of the members too? (for passing functions to other functions, etc)
  (let ((lname (if (consp name) (second name) name))
        (jname (if (consp name) (first name) name)))
   `(ps:defpsmacro ,lname ,(loop for name in args
                              collect name)
      (let* ((local-context (ps::ps-macroexpand '*context*))
             (context (if (eq local-context '*context*)
                          *context*
                          local-context)))
        `(funcall (ps:@ ,(getf context ',object) ,',jname)
                  ,,@(loop for name in args
                        collect name))))))


;;;; attributes
;; - looks like no scoped properties to worry about
#++
(defpsmacro %mjs-global (object name)
  (let* ((local-context (ps::ps-macroexpand '*context*))
         (context (if (eq local-context '*context*)
                      *context*
                      local-context)))
    `(ps:@ ,(getf context object) ,name)))
#++
(defmacro define-mjs-global (object name type)
  (declare (ignore type))
  `(ps:define-ps-symbol-macro ,name
       (%mjs-global ',object ,name)))

;;; unscoped globals
(define-symbol-macro *mjs-version* +mjs_version+)
(define-symbol-macro *mjs-do-assert* +mjs_do_assert+)
(define-symbol-macro *mjs-float-array-type* +mjs_float_array_type+)

(defpsmacro *mjs-float-array-type* (&rest r)
  `(+mjs_float_array_type+ ,@r))

(define-mjs-fun :v3 ($ vec) x y z)

(define-mjs-fun :v3 (clone vec-clone) a)
;;; fixme: set up stuff so we can put &optional before dest on these
(define-mjs-fun :v3 (add vec+) a b dest)
(define-mjs-fun :v3 (sub vec-) a b dest)
(define-mjs-fun :v3 (neg vec-negate) a dest)
(define-mjs-fun :v3 (direction vec-direction) a b dest)
(define-mjs-fun :v3 (length vec-length) a)
(define-mjs-fun :v3 (length-squared vec-length^2) a)
(define-mjs-fun :v3 normalize a dest)
(define-mjs-fun :v3 (scale vec*) a f dest)
(define-mjs-fun :v3 (dot dot) a b dest)
(define-mjs-fun :v3 (cross cross) a b dest)

(define-mjs-fun :m4 ($ matrix) m00 m01 m02 m03 m04 m05 m06 m07 m08 m09 m10 m11 m12 m13 m14 m15)
(define-mjs-fun :m4 (clone matrix-clone) a)

(define-mjs-fun :m4 top-left-3x3 m dest3x3)
(define-mjs-fun :m4 inverse-orthonormal m dest)
(define-mjs-fun :m4 inverse-to-3x3 m dest)
(define-mjs-fun :m4 make-frustum left right bottom top znear zfar dest)
(define-mjs-fun :m4 make-perspective fovy aspect znear zfar dest)
(define-mjs-fun :m4 make-ortho left right bottom top znear zfar dest)
(define-mjs-fun :m4 (make-ortho2-d make-ortho-2d) left right bottom top dest)
(define-mjs-fun :m4 (mul matrix*) a b dest)
(define-mjs-fun :m4 make-rotate angle axis dest)
(define-mjs-fun :m4 rotate angle axis m dest)
(define-mjs-fun :m4 make-scale-3 x y z dest)
(define-mjs-fun :m4 make-scale-1 k dest)
(define-mjs-fun :m4 make-scale vec dest)
(define-mjs-fun :m4 scale-3 x y z m dest)
(define-mjs-fun :m4 scale-1 k m dest)
(define-mjs-fun :m4 make-translate-3 x y z dest)
(define-mjs-fun :m4 make-translate-1 k dest)
(define-mjs-fun :m4 make-translate vec dest)
(define-mjs-fun :m4 translate-3 x y z m dest)
(define-mjs-fun :m4 translate-1 k m dest)
(define-mjs-fun :m4 translate vec m dest)
(define-mjs-fun :m4 make-look-at eye center up dest)
(define-mjs-fun :m4 transpose m dest)

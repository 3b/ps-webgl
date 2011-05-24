;;; -*- Mode: LISP; slime-proxy-proxy-connection: t -*-
(in-package #:webgl-ps)

;;; todo: move all this into a separate object once it is stabilized

(setf #:+wps+ (create))

(eval-when (:compile-toplevel :load-toplevel)
  ;; we want to put functions inside an object to keep namespaces clean,
  ;; but we also want nice short names, and arglists for slime, so
  ;; we define a macro to expand to calls to the real function
  ;; todo: we also want to be able to define stuff inside the object,
  ;;   for example when the functions need to close over common state
  ;;   that shouldn't be visible from the object
  ;;  make sure that works with this stuff, or figure out an alternative
  ;;  ?? or maybe have a separate function to populate the object
  ;;     containing the functions with shared state?
  ;;     that way they can redefine parts of the object without
  ;;     erasing other functions that were added after the object
  ;;     was created
  (defpsmacro define-wrapped-fun ((&rest scoped-name) lambda-list &body body)
    (let ((w (gensym)))
      (print
       `(progn
          (defmacro ,(car (last scoped-name)) (&whole ,w ,@lambda-list)
            `(funcall (ps:@ ,@',scoped-name) ,@(cdr ,w)))
          (setf (@ ,@scoped-name)
                (lambda ,lambda-list
                  ,@body))

          )))))

(define-wrapped-fun (*wps* foo) (bar &optional baz)
  (+ bar baz))

(define-wrapped-fun (*wps* clog) (&rest args)
  (when (and console (@ console log))
    (when args
      #++((@ console log) (apply (@ *string concat) args))
      ((@ console log) (+ "" ((@ args join) ""))))))

;; #:canvas to avoid conflict with %gl:canvas
(define-wrapped-fun (*wps* init-webgl) (#:canvas &optional attributes messages)
  (clog "init webgl on canvas  " #:canvas)
   ;; todo: check for errors, pass to error handler of some sort
   ;; (possibly have a default to replace cavnas with error message?)
   #++
   (unless messages
     (setf messages
           (create no-webgl "WebGL not supported: <a href=\"http://get.webgl.org/\"> Get WebGL</a>"
                   other-problem "Could not initialize WebGL, <a href=\"http://get.webgl.org/troubleshooting/\"> click here ")))
   (let ((context))
     (dolist (i '("webgl" "experimental-webgl" "moz-webgl" "webkit-3d"))
       (try
        (progn
          (clog "trying " i)
          (setf context ((@ #:canvas get-context) i attributes))
          (clog "  got " context))
        (:catch (e)
          ;; todo: catch 'webglcontextcreationerror' errors and handle
          ;; properly
          ))
       (when context
         (return-from init-webgl context)))
     (unless context
       (clog "couldn't create WebGL context"))))



(defmacro request-animation-frame (&whole w callback &optional element)
  `((@ *wps* request-animation-frame apply) #:window (array ,@(cdr w))))

(setf (@ *wps* request-animation-frame)
      ((lambda ()
         (return  (or (@ window request-animation-frame)
                      (@ window moz-request-animation-frame)
                      (@ window webkit-request-animation-frame)
                      (@ window o-request-animation-frame)
                      (@ window ms-request-animation-frame)
                      (lambda (callback element)
                        (((@ window set-timeout) callback (/ 1000 60)))))))))

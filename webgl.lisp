;;; -*- Mode: LISP; slime-proxy-proxy-connection: t -*-
(in-package #:webgl-ps)

;;; todo: move all this into a separate object once it is stabilized

(setf #:+wps+ (create))


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

(define-wrapped-fun (*wps* canvas-from-image) (#:image-id &optional attributes messages)
  (clog "creating canvas to replace image  " #:image-id)
  (let ((#:canvas (chain document (create-element "canvas")))
        (context)
        (img (chain document (get-element-by-id #:image-id))))
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
        (setf (@ #:canvas width) (@ img width)
              (@ #:canvas height) (@ img height)
              (@ #:canvas style css-text) (@ img style css-text))
        (chain img parent-node (replace-child #:canvas img))
        (setf (@ #:canvas id) #:image-id)
        (return-from init-webgl context)))
    (unless context
      (clog "couldn't create WebGL context"))))

;; not really webgl, but might as well...
(define-wrapped-fun (*wps* canvas2d-from-image) (#:image-id &optional attributes messages)
  (clog "creating 2d canvas to replace image  " #:image-id)
  (let ((#:canvas (chain document (create-element "canvas")))
        (context)
        (img (chain document (get-element-by-id #:image-id))))
    (dolist (i '("2d"))
      (try
       (progn
         (clog "trying " i)
         (setf context ((@ #:canvas get-context) i attributes))
         (clog "  got " context))
       (:catch (e)
         ;; todo: handle errors?
         ))
      (when context
        (setf (@ #:canvas width) (@ img width)
              (@ #:canvas height) (@ img height)
              (@ #:canvas style css-text) (@ img style css-text))
        (chain img parent-node (replace-child #:canvas img))
        (setf (@ #:canvas id) #:image-id)
        (return-from init-webgl context)))
    (unless context
      (clog "couldn't create canvas context"))))



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

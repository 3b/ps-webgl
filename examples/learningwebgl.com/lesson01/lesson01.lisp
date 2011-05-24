;;; -*- Mode: LISP; slime-proxy-proxy-connection: t -*-
;;; translated from https://github.com/gpjt/webgl-lessons/blob/master/lesson01/index.html
;;; http://learningwebgl.com/blog/?p=28
(eval-when (:compile-toplevel :load-toplevel)
  (defpackage #:learningwebgl.com-01
    (:use :ps :cl :example-utils)
    (:local-nicknames (:m :mjs-bindings)
                      (:%gl :webgl-bindings)
                      (:gl :webgl-ps))))

(in-package #:learningwebgl.com-01)


;; these were in the html in the originally, but i code PS through
;; slime-proxy, so adding them here is more convenient
(defvar *fragment-shader* "
#ifdef GL_ES
  precision highp float;
#endif


void main(void) {
    gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
}")

(defvar *vertex-shader* "
attribute vec3 aVertexPosition;

uniform mat4 uMVMatrix;
uniform mat4 uPMatrix;

void main(void) {
    gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
}")


;;; this name must match webgl-bindings::*webgl-context*, or else
;;; we need to wrap gl calls with webgl-bindings::with-gl-context
(defvar *GL*)

(defun init-gl (canvas)
  (try
   (progn
     (setf *GL* ((@ canvas get-context) "experimental-webgl"))
     (setf (@ *GL* viewport-width) (@ canvas width))
     (setf (@ *GL* viewport-height) (@ canvas height)))
   (:catch (e)
     (clog "error " ((@ e to-string)))))
  (unless *GL*
    (clog "couldn't initialize webgl")
    #++(alert "could not initialize WebGL"))
  (clog "GL init " (if *gl* "done" "failed")))

;; (init-gl  ((@ document get-element-by-id ) "canvas"))
(defun create-shader (type source)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (unless (gl:get-shader-parameter shader gl:+compile-status+)
      (clog "  couldn't create shader"
             (gl:get-shader-info-log shader))
      (return nil))
    (clog "loaded shader:"
          (gl:get-shader-parameter shader gl:+compile-status+)
          " " (gl:get-shader-info-log shader))
    shader))

(defvar shader-program)

(defun init-shaders ()
  (let ((fragment-shader (create-shader gl:+fragment-shader+ *fragment-shader*))
        (vertex-shader (create-shader gl:+vertex-shader+ *vertex-shader*)))
    (setf shader-program (gl:create-program))
    (gl:attach-shader shader-program vertex-shader)
    (gl:attach-shader shader-program fragment-shader)
    (gl:link-program shader-program)
    (unless (gl:get-program-parameter shader-program gl:+link-status+)
      (clog (gl:get-program-info-log shader-program))
      (return nil))
    (clog "program linked" (gl:get-program-info-log shader-program))
    (gl:use-program shader-program)
    (setf (@ shader-program vertex-position-attribute)
          (gl:get-attrib-location shader-program "aVertexPosition"))
    (gl:enable-vertex-attrib-array (@ shader-program vertex-position-attribute))
    (setf (@ shader-program vertex-position-attribute)
          (gl:get-attrib-location shader-program "aVertexPosition"))
    (setf (@ shader-program p-matrix-uniform)
          (gl:get-uniform-location shader-program "uPMatrix"))
    (setf (@ shader-program mv-matrix-uniform)
          (gl:get-uniform-location shader-program "uMVMatrix"))
    (clog "init shaders done ")))

#++
(init-shaders)

(defvar mv-matrix (m:matrix))
(defvar p-matrix (m:matrix))

(defun set-matrix-uniforms ()
  (gl:uniform-matrix-4fv (@ shader-program p-matrix-uniform) false p-matrix)
  (gl:uniform-matrix-4fv (@ shader-program mv-matrix-uniform) false mv-matrix))

(defvar triangle-vertex-position-buffer)
(defvar square-vertex-position-buffer)

(defun init-buffers ()
  (flet ((buffer-from-array (array size)
           (let ((b (gl:create-buffer))
                 (size (or size 3)))
               (gl:bind-buffer gl:+array-buffer+ b)
               (gl:buffer-data gl:+array-buffer+ (new (*float-32-array array))
                               gl:+static-draw+)
               (setf (@ b item-size) size
                     (@ b num-items) (floor (length array) size))
               b)))
    (setf triangle-vertex-position-buffer
          (buffer-from-array (array 0.0 1.0 0.0
                                    -1.0 -1.0 0.0
                                    1.0 -1.0 0.0)))
    (setf square-vertex-position-buffer
          (buffer-from-array (array 1.0 1.0 0.0
                                    -1.0 1.0 0.0
                                    1.0 -1.0 0.0
                                    -1.0 -1.0 0.0)))
    (return)))

; (init-buffers)

(defun draw-scene ()
  (gl:viewport 0 0 (@ *gl* viewport-width) (@ *gl* viewport-height))
  (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))

  (m:make-perspective 45
                      (/ (@ *gl* viewport-width) (@ *gl* viewport-height))
                      0.1 100
                      p-matrix)

  (setf mv-matrix (m:matrix-clone (@ *m4x4 *i)))

  (flet ((draw-at (shape pos prim)
           (unless prim (setf prim gl:+triangles+))
           (m:translate pos mv-matrix mv-matrix)
           (gl:bind-buffer gl:+array-buffer+ shape)
           (gl:vertex-attrib-pointer (@ shader-program vertex-position-attribute)
                                     (@ shape item-size) gl:+float+ false
                                     0 0)
           (set-matrix-uniforms)
           (gl:draw-arrays prim 0 (@ shape num-items))))
    (draw-at triangle-vertex-position-buffer (m:vec -1.5 0 -7))
    (draw-at square-vertex-position-buffer (m:vec 3 0 0) gl:+triangle-strip+)))

(defun webgl-start ()
  (let ((canvas ((@ document get-element-by-id) "canvas")))
    (init-gl canvas)
    (init-shaders)
    (init-buffers)
    (gl:clear-color 0 0 0 1)
    (gl:disable gl:+depth-test+)
    (draw-scene)
    nil))

#++
(webgl-start)


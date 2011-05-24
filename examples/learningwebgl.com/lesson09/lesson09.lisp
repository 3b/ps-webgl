;;; -*- Mode: LISP; slime-proxy-proxy-connection: t -*-
;;; translated from https://github.com/gpjt/webgl-lessons/blob/master/lesson09/index.html
;;; http://learningwebgl.com/blog/?p=1008
(eval-when (:compile-toplevel :load-toplevel)
  (defpackage #:learningwebgl.com-09
    (:use :ps :cl :example-utils)
    (:local-nicknames (:m :mjs-bindings)
                      (:%gl :webgl-bindings)
                      (:gl :webgl-ps)))
  (setf (ps:ps-package-prefix '#:learningwebgl.com-09) "lesson09_"))

(in-package #:learningwebgl.com-09)


;; these were in the html in the originally, but i code PS through
;; slime-proxy, so adding them here is more convenient
(defvar *fragment-shader* "
#ifdef GL_ES
precision highp float;
#endif

varying vec2 vTextureCoord;

uniform sampler2D uSampler;

uniform vec3 uColor;

void main(void) {
    vec4 textureColor = texture2D(uSampler, vec2(vTextureCoord.s, vTextureCoord.t));
   // gl_FragColor = textureColor * vec4(uColor, 1.0);
 gl_FragColor = textureColor * vec4(uColor, 1.0);
}")

(defvar *vertex-shader* "
attribute vec3 aVertexPosition;
attribute vec2 aTextureCoord;

uniform mat4 uMVMatrix;
uniform mat4 uPMatrix;

varying vec2 vTextureCoord;

void main(void) {
    gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
    vTextureCoord = aTextureCoord;
}")


;;; this name must match webgl-bindings::*webgl-context*, or else
;;; we need to wrap gl calls with webgl-bindings::with-gl-context
(defvar #:*GL*)

(defun init-gl (canvas)
  (try
   (progn
     (setf #:*GL* ((@ canvas #:get-context) "experimental-webgl"))
     (setf (@ #:*GL* viewport-width) (@ canvas #:width))
     (setf (@ #:*GL* viewport-height) (@ canvas #:height)))
   (:catch (e)
     (gl::clog "error " ((@ e #:to-string)))))
  (unless #:*GL*
    (gl::clog "couldn't initialize webgl")
    #++(alert "could not initialize WebGL"))
  (gl::clog "GL init " (if #:*gl* "done" "failed")))

;; (init-gl "canvas")
(defun create-shader (type source)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (unless (gl:get-shader-parameter shader gl:+compile-status+)
      (gl::clog "  couldn't create shader"
             (gl:get-shader-info-log shader))
      (return nil))
    (gl::clog "loaded shader:"
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
      (gl::clog (gl:get-program-info-log shader-program))
      (return nil))
    (gl::clog "program linked" (gl:get-program-info-log shader-program))
    (gl:use-program shader-program)

    (setf (@ shader-program vertex-position-attribute)
          (gl:get-attrib-location shader-program "aVertexPosition"))
    (gl:enable-vertex-attrib-array (@ shader-program vertex-position-attribute))
    (setf (@ shader-program texture-coord-attribute)
          (gl:get-attrib-location shader-program "aTextureCoord"))
    (gl:enable-vertex-attrib-array (@ shader-program texture-coord-attribute))

    (setf (@ shader-program p-matrix-uniform)
          (gl:get-uniform-location shader-program "uPMatrix"))
    (setf (@ shader-program mv-matrix-uniform)
          (gl:get-uniform-location shader-program "uMVMatrix"))
    (setf (@ shader-program sampler-uniform)
          (gl:get-uniform-location shader-program "uSampler"))
    (setf (@ shader-program color-uniform)
          (gl:get-uniform-location shader-program "uColor"))

    (gl::clog "init shaders done ")))

#++
(init-shaders)

(defun handle-loaded-texture (texture)
  (gl:pixel-store-i gl:+unpack-flip-y-webgl+ t)
  (gl:bind-texture gl:+texture-2d+ texture)
  (gl:tex-image-2d gl:+texture-2d+ 0 gl:+rgba+ gl:+rgba+
                   gl:+unsigned-byte+ (@ texture image))
  (gl:tex-parameter-i gl:+texture-2d+ gl:+texture-mag-filter+ gl:+linear+)
  (gl:tex-parameter-i gl:+texture-2d+ gl:+texture-min-filter+ gl:+linear+)
  (gl:bind-texture gl:+texture-2d+ null))

(defvar star-texture)

(defun init-texture ()
  (setf star-texture (gl:create-texture))
  (setf (@ star-texture image) (new (#:*image)))
  (setf (@ star-texture image #:onload)
        (lambda ()
          (handle-loaded-texture star-texture)))
  (setf (@ star-texture image #:src) "../../../webgl/examples/learningwebgl.com/lesson09/star.gif"))

#++
(init-texture)

(defvar mv-matrix (m:matrix))
(defvar mv-matrix-stack (array))
(defvar p-matrix (m:matrix))

(defun mv-push-matrix ()
  ((@ mv-matrix-stack #:push) (m:matrix-clone mv-matrix)))

(defun mv-pop-matrix ()
  (when (= 0(length mv-matrix-stack))
    (throw "Invalid pop-matrix"))
  (setf mv-matrix ((@ mv-matrix-stack #:pop))))

(defun set-matrix-uniforms ()
  (gl:uniform-matrix-4fv (@ shader-program p-matrix-uniform) #:false p-matrix)
  (gl:uniform-matrix-4fv (@ shader-program mv-matrix-uniform) #:false mv-matrix))

(defun degrees-to-radians (degrees)
  (* degrees (/ pi 180)))

(defvar currently-pressed-keys (create))

(defun handle-key-down (event)
  (setf (aref currently-pressed-keys (@ event #:key-code)) t))

(defun handle-key-up (event)
  (setf (aref currently-pressed-keys (@ event #:key-code)) #:false))

(defvar zoom -15)
(defvar tilt 90)
(defvar spin 0)

(defun handle-keys ()
  (when (aref currently-pressed-keys 33) ;; page up
    (decf zoom 0.1))
  (when (aref currently-pressed-keys 34) ;; page down
    (incf zoom 0.1))
  (when (aref currently-pressed-keys 38) ;; up
    (incf tilt 2))
  (when (aref currently-pressed-keys 40) ;; down
    (decf tilt 2)))

(defvar star-vertex-position-buffer)
(defvar star-vertex-texture-coord-buffer)

(defun init-buffers ()
  (flet ((buffer-from-array (array size)
           (let ((b (gl:create-buffer))
                 (size (or size 3)))
               (gl:bind-buffer gl:+array-buffer+ b)
               (gl:buffer-data gl:+array-buffer+ (new (#:*float-32-array array))
                               gl:+static-draw+)
               (setf (@ b item-size) size
                     (@ b num-items) (floor (length array) size))
               b)))
    (setf star-vertex-position-buffer
          (buffer-from-array (array -1 -1 0
                                    1 -1 0
                                    -1 1 0
                                    1 1 0)))
    (setf star-vertex-texture-coord-buffer
          (buffer-from-array (array 0 0
                                    1 0
                                    0 1
                                    1 1) 2))
    (return)))

; (init-buffers)

(defun draw-star ()
  (gl:active-texture gl:+texture0+)
  (gl:bind-texture gl:+texture-2d+ star-texture)
  (gl:uniform-1i (@ shader-program sampler-uniform) 0)
  (gl:bind-buffer gl:+array-buffer+ star-vertex-texture-coord-buffer)
  (gl:vertex-attrib-pointer (@ shader-program texture-coord-attribute)
                            (@ star-vertex-texture-coord-buffer item-size)
                            gl:+float+ #:false 0 0)
  (gl:bind-buffer gl:+array-buffer+ star-vertex-position-buffer)
  (gl:vertex-attrib-pointer (@ shader-program vertex-position-attribute)
                            (@ star-vertex-position-buffer item-size)
                            gl:+float+ #:false 0 0)
  (set-matrix-uniforms)
  (gl:draw-arrays gl:+triangle-strip+ 0 (@ star-vertex-position-buffer num-items)))

(defun *star (starting-distance rotation-speed)
  (setf (@ this angle) 0
        (@ this dist) starting-distance
        (@ this rotation-speed) rotation-speed)
  ((@ this randomize-colors))
  #:this)

(setf (@ *star #:prototype draw)
      (lambda (tilt spin twinkle)
        (mv-push-matrix)

        ;; move to star's position
        (m:rotate (degrees-to-radians (@ this angle)) (array 0 1 0)
                  mv-matrix mv-matrix)
        (m:translate (array (@ this dist) 0 0) mv-matrix mv-matrix)

        ;; rotate back so star faces viewer
        (m:rotate (degrees-to-radians (- (@ this angle))) (array 0 1 0)
                  mv-matrix mv-matrix)
        (m:rotate (degrees-to-radians (- tilt)) (array 1 0 0)
                  mv-matrix mv-matrix)
        (when twinkle
          ;; draw a non-rotating star in the alternate 'twinkling' color
          (gl:uniform-3fv (@ shader-program color-uniform) (@ this twinkle-rgb))
          (draw-star))

        ;;all stars spin around the z axis at the same rate
        (m:rotate (degrees-to-radians spin) (array 0 0 1)
                  mv-matrix mv-matrix)

        ;; draw star in its main color
        (gl:uniform-3fv (@ shader-program color-uniform) (@ this rgb))
        (draw-star)

        (mv-pop-matrix)))

(defvar effective-fpms (/ 60 1000))

(setf (@ *star #:prototype animate)
      (lambda (elapsed-time)
        (incf (@ this angle) (* (@ this rotation-speed) effective-fpms elapsed-time))
        ;; fall towards center, reset to outside once it hits center
        (decf (@ this dist) (* 0.01 effective-fpms elapsed-time))
        (when (< (@ this dist) 0)
          (incf (@ this dist) 5.0)
          ((@ this randomize-colors)))))

(setf (@ *star #:prototype randomize-colors)
      (lambda ()
        ;; random color for normal circumstances
        (setf (@ this rgb) (array (random) (random) (random))
              ;; when star is twinkling, we draw it twice, once in
              ;; color below (and not spinning), and once as normal
              ;; in color defined above
              (@ this twinkle-rgb) (array (random) (random) (random)))))

(defvar stars (array))

(defun init-world-objects (n)
  (setf stars (array))
  (let ((num-stars (or n 50)))
    (dotimes (i num-stars)
      ((@ stars push) (new (*Star (* 5 (/ i num-stars))
                                  (/ i num-stars)))))))

(defun draw-scene ()
  (gl:viewport 0 0 (@ #:*gl* viewport-width) (@ #:*gl* viewport-height))
  (gl:clear (logior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))

  (m:make-perspective 45
                      (/ (@ #:*gl* viewport-width) (@ #:*gl* viewport-height))
                      0.1 100
                      p-matrix)

  (gl:blend-func gl:+src-alpha+ gl:+one+)
  (gl:enable gl:+blend+)

  (setf mv-matrix (m:matrix-clone (@ #:*m4x4 #:*i)))
  (m:translate (array 0 0 zoom) mv-matrix mv-matrix)
  (m:rotate (degrees-to-radians tilt) (array 1 0 0) mv-matrix mv-matrix)

  (for-in (i stars)
          ((@ (aref stars i) draw) tilt spin twinkle)
          (incf spin 0.1)))

(defvar last-time 0)

(defun animate ()
  (let ((now (funcall (@ (new (*date)) get-time))))
    (unless (= last-time 0)
        (let ((elapsed-time (- now last-time)))
          (for-in (i stars)
                  ((@ (aref stars i) animate) elapsed-time))))
    (setf last-time now)))

(defvar stop t)
(defun one-tick ()
  (handle-keys)
  (draw-scene)
  (animate)
  )
(defun tick ()
  (one-tick)
  (unless stop
    (gl:request-animation-frame tick #++ gl:canvas))
  (when stop
    (gl::clog "stopping ...")))

(defun webgl-start ()
  (let ((canvas ((@ #:document #:get-element-by-id) "canvas")))
    (init-gl canvas))
  (init-shaders)
  (init-buffers)
  (init-texture)
  (init-world-objects 50)
  (gl:disable gl:+depth-test+)
  (gl:clear-color 0 0 0 1)
  (setf (@ #:document #:onkeydown) handle-key-down)
  (setf (@ #:document #:onkeyup) handle-key-up)
  (setf stop #:false)
  (tick)
  nil)

(defvar twinkle t)
#++
(webgl-start)

(aref stars 1)
zoom
tilt
spin
(setf zoom -10)
(setf tilt 20)
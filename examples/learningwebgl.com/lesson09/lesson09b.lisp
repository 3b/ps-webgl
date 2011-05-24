;;; -*- Mode: LISP; slime-proxy-proxy-connection: t -*-
;;; like lesson09, but using more helpers...

(eval-when (:compile-toplevel :load-toplevel)
  (defpackage #:learningwebgl.com-09b
    (:use :ps :cl :example-utils)
    (:local-nicknames (:m :mjs-bindings)
                      (:%gl :webgl-bindings)
                      (:gl :webgl-ps)))
  (setf (ps:ps-package-prefix '#:learningwebgl.com-09b) "lesson09b_"))

(in-package #:learningwebgl.com-09b)

(defvar *fragment-shader* "
#ifdef GL_ES
precision highp float;
#endif

varying vec2 vTextureCoord;

uniform sampler2D uSampler;

uniform vec3 uColor;

void main(void) {
    vec4 textureColor = texture2D(uSampler, vec2(vTextureCoord.s, vTextureCoord.t));
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


(defun init-gl (canvas-id)
  (let ((canvas ((@ #:document #:get-element-by-id) canvas-id)))
    (setf #:*gl* (gl:init-webgl canvas))
    (when #:*gl*
      (setf (@ #:*GL* viewport-width) (@ canvas width))
      (setf (@ #:*GL* viewport-height) (@ canvas height)))))

;;; shader stuff, need a nicer wrapper for at least trivial case...
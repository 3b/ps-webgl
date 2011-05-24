;;; -*- Mode: LISP; slime-proxy-proxy-connection: t -*-
;;; common utillities used by examples: logging etc
(eval-when (:compile-toplevel :load-toplevel)
  (defpackage #:example-utils
    (:use :ps :cl)
    (:local-nicknames (:m :mjs-bindings)
                      (:%gl :webgl-bindings)
                      (:gl :webgl-bindings))
    (:export #:clog)))

(in-package #:example-utils)

(defun clog (&rest args)
  (when (and console (@ console log))
    ((@ console log) (apply (@ *string concat) args))))

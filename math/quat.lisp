;;; -*- Mode: LISP; slime-proxy-proxy-connection: t -*-
(in-package #:webgl-quat)


(defun quat-matrix (q dest)
  ;; todo: benchmark multiplying wxyz by #.(sqrt 2.0) instead of
  ;; multiplying all the terms by 2 in the final calc?
  ;; 4 multiplies instead of 9 might be a tiny bit faster, and probably
  ;; not much less accurate?
  ;; (also try #.(sqrt 2d0))?
  (unless dest
    (setf dest (new (mjs:*mjs-float-array-type* 16))))
  (let* ((w (aref q 0))
         (x (aref q 1))
         (y (aref q 2))
         (z (aref q 3))
         (xx (* x x))
         (xy (* x y))
         (xz (* x z))
         (xw (* x w))
         (yy (* y y))
         (yz (* y z))
         (yw (* y w))
         (zz (* z z))
         (zw (* z w)))
    (setf
     (aref dest  0) (- 1 (* 2 (+ yy zz)))
     (aref dest  1) (* 2 (+ xy zw))
     (aref dest  2) (* 2 (- xz yw))
     (aref dest  3) 0

     (aref dest  4) (* 2 (- xy zw))
     (aref dest  5) (- 1 (* 2 (+ xx zz)))
     (aref dest  6) (* 2 (+ yz xw))
     (aref dest  7) 0

     (aref dest  8) (* 2 (+ xz yw))
     (aref dest  9) (* 2 (- yz xw))
     (aref dest 10) (- 1 (* 2 (+ xx yy)))
     (aref dest 11) 0

     (aref dest 12) 0.0
     (aref dest 13) 0.0
     (aref dest 14) 0.0
     (aref dest 15) 1.0))
  dest)

(defmacro qw (q) `(aref ,q 0))
(defmacro qx (q) `(aref ,q 1))
(defmacro qy (q) `(aref ,q 2))
(defmacro qz (q) `(aref ,q 3))

(defmacro defun-dest (name (&rest args) &body body)
  (assert (not (member 'dest args)))
  ;; todo: maybe gensym 'dest' var if only using through macro?
  `(defun ,name (,@args dest)
     (unless dest
       (setf dest (new (mjs:*mjs-float-array-type* 4))))
     (macrolet ((dest (w x y z)
                  `(progn (setf (qw dest) ,w
                                (qx dest) ,x
                                (qy dest) ,y
                                (qz dest) ,z))))
       ,@body)
     dest))

(defun-dest angle-axis-to-quaternion (angle-radians axis)
  "create a quaternion from specified axis and angle in radians"
  (let* ((half-a (/ angle-radians 2.0))
         (s  (sin half-a)))
    (dest (cos half-a)
          (* (aref axis 0) s)
          (* (aref axis 1) s)
          (* (aref axis 2) s))))
#++
(angle-axis-to-quaternion 0.5 (mjs:vec 1 2 3))


(defun-dest quat-clone (q)
  (dest (qw q) (qx q) (qy q) (qz q)))

(eval-when ( :compile-toplevel :load-toplevel :execute)
  (defmacro %* (a b)
    (cond
      ((or (eql a 0) (eql a 0.0)
           (eql b 0) (eql b 0.0))
       0)
      ((or (eql a 1) (eql a 1.0)) b)
      ((or (eql b 1) (eql b 1.0)) a)
      (t `(* ,a ,b)))))

(defmacro %nq (a1 a2 a3 a4 b1 b2 b3 b4)
  `(dest  (- (%* ,a1 ,b1)
             (%* ,a2 ,b2)
             (%* ,a3 ,b3)
             (%* ,a4 ,b4))
          (+ (%* ,a1 ,b2)
             (%* ,a2 ,b1)
             (%* ,a3 ,b4)
             (- (%* ,a4 ,b3)))
          (+ (%* ,a1 ,b3)
             (- (%* ,a2 ,b4))
             (%* ,a3 ,b1)
             (%* ,a4 ,b2))
          (+ (%* ,a1 ,b4)
             (%* ,a2 ,b3)
             (- (%* ,a3 ,b2))
             (%* ,a4 ,b1))))

(defmacro %with-nq ((d1 d2 d3 d4) (a1 a2 a3 a4 b1 b2 b3 b4) &body body)
  `(let (,@ (when d1
              `((,d1 (- (%* ,a1 ,b1)
                        (%* ,a2 ,b2)
                        (%* ,a3 ,b3)
                        (%* ,a4 ,b4)))))
         ,@ (when d2
              `((,d2 (+ (%* ,a1 ,b2)
                       (%* ,a2 ,b1)
                       (%* ,a3 ,b4)
                       (- (%* ,a4 ,b3))))))
            ,@ (when d3
                 `((,d3 (+ (%* ,a1 ,b3)
                           (- (%* ,a2 ,b4))
                           (%* ,a3 ,b1)
                           (%* ,a4 ,b2)))))
            ,@ (when d4
                 `((,d4 (+ (%* ,a1 ,b4)
                           (%* ,a2 ,b3)
                           (- (%* ,a3 ,b2))
                           (%* ,a4 ,b1))))))
     ,@body))

(defmacro with-quat ((w x y z) q &body body)
  `(let ((,w (qw ,q))
         (,x (qx ,q))
         (,y (qy ,q))
         (,z (qz ,q)))
     ,@body))

(defun-dest quat* (a b)
  (with-quat (aw ax ay az) a
    (with-quat (bw bx by bz) b
      (%nq aw ax ay az bw bx by bz))))

#++
(quat* (angle-axis-to-quaternion 0.5 (mjs:vec 1 2 3))
       (angle-axis-to-quaternion 1.0 (mjs:vec 3 4 5)))


(defmacro defun-qrot (name index &optional post)
  (flet ((mul (i1 i2 &optional extra)
           (let ((term2 nil))
             (when post (rotatef i1 i2))
             (cond
               ((eq 'qw i2)
                (setf term2 'cos-a2))
               ((eq (elt '(qw qx qy qz) index) i2)
                (setf term2 'sin-a2)))
             (when term2
               `((* ,i1 ,term2 ,@(when extra (list extra))))))))
    (let ((body `((- ,@(mul 'qw 'qw)
                     ,@(mul 'qx 'qx)
                     ,@(mul 'qy 'qy)
                     ,@(mul 'qz 'qz))
                  (+ ,@(mul 'qw 'qx)
                     ,@(mul 'qx 'qw)
                     ,@(mul 'qy 'qz)
                     ,@(mul 'qz 'qy -1))
                  (+ ,@(mul 'qw 'qy)
                     ,@(mul 'qx 'qz -1)
                     ,@(mul 'qy 'qw)
                     ,@(mul 'qz 'qx))
                  (+ ,@(mul 'qw 'qz)
                     ,@(mul 'qx 'qy)
                     ,@(mul 'qy 'qx -1)
                     ,@(mul 'qz 'qw))))
          (bindings '((a2 (/ angle 2.0))
                      (cos-a2 (cos a2))
                      (sin-a2 (sin a2))
                      (qw (aref quat 0))
                      (qx (aref quat 1))
                      (qy (aref quat 2))
                      (qz (aref quat 3)))))
      `(defun-dest ,name (quat angle)
         (let* ,bindings
           (dest ,@body))))))


;;; macros to rotate a quat by specified angle:
;;;  local-* rotates around local axes of quat
;;;  world-* rotates around world space axes
;;;  rotate-* returns rotated quat without modifying args
;;;  nrotate* modifies (and returns) quat arg
;;; !note that these don't preserve -0.0 or NaNs properly when multiplying by 0

(defun-qrot q-rotate-local-x 1)
(defun-qrot q-rotate-local-y 2)
(defun-qrot q-rotate-local-z 3)

(defun-qrot q-rotate-world-x 1 t)
(defun-qrot q-rotate-world-y 2 t)
(defun-qrot q-rotate-world-z 3 t)

;(q-rotate-local-x (angle-axis-to-quaternion 2 (mjs:vec 1 1 0)) 1)
;(q-rotate-local-y (angle-axis-to-quaternion 2 (mjs:vec 1 1 0)) 1)
;(q-rotate-local-z (angle-axis-to-quaternion 2 (mjs:vec 1 1 0)) 1)

;(q-rotate-world-x (angle-axis-to-quaternion 2 (mjs:vec 1 1 0)) 1)
;(q-rotate-world-y (angle-axis-to-quaternion 2 (mjs:vec 1 1 0)) 1)
;(q-rotate-world-z (angle-axis-to-quaternion 2 (mjs:vec 1 1 0)) 1)

#++
(q-rotate-local-x
 (q-rotate-local-y
  (q-rotate-local-z
   (q-rotate-world-x
    (q-rotate-world-y
     (q-rotate-world-z (angle-axis-to-quaternion 2 (mjs:vec 1 1 0)) 1) 2) 3) 4) 5) 6)


(defun quat-rotate-vector (quat vec dest)
  #++"rotate a vector VEC using specifed rotation quaternion Q, returning
result as a new single-float vector."
  (unless dest
    (setf dest (new (mjs:*mjs-float-array-type* 3))))
  (with-quat (qw qx qy qz ) quat
             (%with-nq (t1 t2 t3 t4)
                       (qw qx qy qz
                           0 (aref vec 0) (aref vec 1) (aref vec 2))
                       (%with-nq (nil v0 v1 v2)
                                 (t1 t2 t3 t4
                                     qw (- qx) (- qy) (- qz))
                                 (setf (aref dest 0) v0
                                       (aref dest 1) v1
                                       (aref dest 2) v2))))
  dest)
(quat-rotate-vector
 (q-rotate-local-x
  (q-rotate-local-y
   (q-rotate-local-z
    (q-rotate-world-x
     (q-rotate-world-y
      (q-rotate-world-z (angle-axis-to-quaternion 2 (mjs:vec 1 1 0)) 1) 2) 3) 4) 5) 6)
 (mjs:vec 1 2 3))

(defun-dest quat+ (a b)
  (dest (+ (aref a 0) (aref b 0))
        (+ (aref a 1) (aref b 1))
        (+ (aref a 2) (aref b 2))
        (+ (aref a 3) (aref b 3))))
#++
(quat+ (angle-axis-to-quaternion 0.5 (mjs:vec 1 2 3))
       (angle-axis-to-quaternion 1.0 (mjs:vec 3 4 5)))

(defun-dest quat- (a b)
  (dest (- (aref a 0) (aref b 0))
        (- (aref a 1) (aref b 1))
        (- (aref a 2) (aref b 2))
        (- (aref a 3) (aref b 3))))

#++
(quat- (angle-axis-to-quaternion 0.5 (mjs:vec 1 2 3))
       (angle-axis-to-quaternion 1.0 (mjs:vec 3 4 5)))

(defun-dest quat-inverse (quat)
  (dest (aref quat 0)
        (- (aref quat 1))
        (- (aref quat 2))
        (- (aref quat 3))))
#++
(quat-inverse (angle-axis-to-quaternion 0.5 (mjs:vec 1 2 3)))

(macrolet ((%mat* (a b d)
           ;; build an inline matrix multiply fron lists of elements
           ;; in a,b,d
           (flet ((make-mul (a b)
                    (cond
                      ((or (equalp a 0) (equalp b 0)) 0)
                      ((equalp a 1) b)
                      ((equalp b 1) a)
                      ((equalp a -1) `(- ,b))
                      ((equalp b -1) `(- ,a))
                      (t `(* ,a ,b))))
                  (make-add (els)
                    (cond
                      (t (let ((s (remove-if (lambda (a) (equalp a 0)) els)))
                           (if s (cons '+ s) 0))))))
             `(progn
                ,@(loop with de = d
                     for i below 4
                     append (loop for j below 4
                               collect
                               `(setf ,(elt de (+ i (* j 4)))
                                      ,(make-add
                                        (loop for k below 4
                                           collect
                                           (make-mul
                                            (elt a (+ i (* k 4)))
                                            (elt b (+ k (* j 4)))))))))))))
  (defun _translationxquat-into-x (tx ty tz qw qx qy qz d)
    (%mat* (1 0 0 tx
              0 1 0 ty
              0 0 1 tz
              0 0 0 1)
           #++(1 0 0 0
                 0 1 0 0
                 0 0 1 0
                 tx ty tz 1)
           ((- 1.0 (* 2.0 (+ (* qy qy) (* qz qz))))
            (* 2.0 (+ (* qx qy) (* qz qw)))
            (* 2.0 (- (* qx qz) (* qy qw)))
            0
            (* 2.0 (- (* qx qy) (* qz qw)))
            (- 1.0 (* 2.0 (+ (* qx qx) (* qz qz))))
            (* 2.0 (+ (* qy qz) (* qx qw)))
            0
            (* 2.0 (+ (* qx qz) (* qy qw)))
            (* 2.0 (- (* qy qz) (* qx qw)))
            (- 1.0 (* 2.0 (+ (* qx qx) (* qy qy ))))
            0
            0 0 0 1)
           #++(1 0 0 tx
                 0 1 0 ty
                 0 0 1 tz
                 0 0 0 1)
           #++(1 0 0 0
                 0 1 0 0
                 0 0 1 0
                 tx ty tz 1)

           ((AREF D 0) (AREF D 1) (AREF D 2) (AREF D 3)
            (AREF D 4) (AREF D 5) (AREF D 6) (AREF D 7)
            (AREF D 8) (AREF D 9) (AREF D 10) (AREF D 11)
            (AREF D 12) (AREF D 13) (AREF D 14) (AREF D 15))))

  (defun _translationxquat-into (tx ty tz qw qx qy qz d)
    (%mat* (1 0 0 0
              0 1 0 0
              0 0 1 0
              tx ty tz 1)
           ((- 1.0 (* 2.0 (+ (* qy qy) (* qz qz))))
            (* 2.0 (- (* qx qy) (* qz qw)))
            (* 2.0 (+ (* qx qz) (* qy qw)))
            0

            (* 2.0 (+ (* qx qy) (* qz qw)))
            (- 1.0 (* 2.0 (+ (* qx qx) (* qz qz))))
            (* 2.0 (- (* qy qz) (* qx qw)))
            0

            (* 2.0 (- (* qx qz) (* qy qw)))
            (* 2.0 (+ (* qy qz) (* qx qw)))
            (- 1.0 (* 2.0 (+ (* qx qx) (* qy qy ))))
            0
            0 0 0 1)

           ((AREF D 0) (AREF D 1) (AREF D 2) (AREF D 3)
            (AREF D 4) (AREF D 5) (AREF D 6) (AREF D 7)
            (AREF D 8) (AREF D 9) (AREF D 10) (AREF D 11)
            (AREF D 12) (AREF D 13) (AREF D 14) (AREF D 15))))

  (defun translation-quat-matrix (v q d)
    (unless d
      (setf d (new (mjs:*mjs-float-array-type* 16))))
    ;; should this be transposed?
    (let ((qw (aref q 0))
          (qx (aref q 1))
          (qy (aref q 2))
          (qz (aref q 3)))
      (%mat* (1 0 0 0
                0 1 0 0
                0 0 1 0
                (aref v 0) (aref v 1) (aref v 2) 1)
             ((- 1.0 (* 2.0 (+ (* qy qy) (* qz qz))))
              (* 2.0 (- (* qx qy) (* qz qw)))
              (* 2.0 (+ (* qx qz) (* qy qw)))
              0

              (* 2.0 (+ (* qx qy) (* qz qw)))
              (- 1.0 (* 2.0 (+ (* qx qx) (* qz qz))))
              (* 2.0 (- (* qy qz) (* qx qw)))
              0

              (* 2.0 (- (* qx qz) (* qy qw)))
              (* 2.0 (+ (* qy qz) (* qx qw)))
              (- 1.0 (* 2.0 (+ (* qx qx) (* qy qy ))))
              0
              0 0 0 1)

             ((AREF D 0) (AREF D 1) (AREF D 2) (AREF D 3)
              (AREF D 4) (AREF D 5) (AREF D 6) (AREF D 7)
              (AREF D 8) (AREF D 9) (AREF D 10) (AREF D 11)
              (AREF D 12) (AREF D 13) (AREF D 14) (AREF D 15)))
      d))
  (macrolet ((mat** (a b dest)
               (let* ((avars '(m11 m21 m31 m41 m12 m22 m32 m42
                               m13 m23 m33 m43 m14 m24 m34 m44))
                      (drefs (loop for i below 16
                                collect `(aref ,dest ,i))))
                 `(let (,@(loop for i below 16
                             for var in avars
                             collect `(,var (aref ,a ,i))))
                    (%mat* ,avars ,b ,drefs)))))
   (defun quat-rotate-matrix (quat matrix dest)
     (unless dest
       (setf dest (new (mjs:*mjs-float-array-type* 16))))
     (let* ((a (qw quat))
            (b (qx quat))
            (c (qy quat))
            (d (qz quat))
            (aa (* a a))
            (bb (* b b))
            (cc (* c c))
            (dd (* d d))
            (ab2 (* 2 a b))
            (ac2 (* 2 a c))
            (ad2 (* 2 a d))
            (bc2 (* 2 b c))
            (bd2 (* 2 b d))
            (cd2 (* 2 c d)))
       (mat**
        matrix
        ((- (+ aa bb) cc dd) (- bc2 ad2)         (+ ac2 bd2)         0.0
         (+ ad2 bc2)         (- (+ aa cc) bb dd) (- cd2 ab2)         0.0
         (- bd2 ac2)         (+ ab2 cd2)         (- (+ aa dd) bb cc) 0.0
         0.0 0.0 0.0 1.0)
        dest))
     dest)))

#++
(quat-rotate-matrix (angle-axis-to-quaternion pi (mjs:vec 1 0 0))
                    (mjs:matrix 1 0 0 0
                              0 1 0 0
                              0 0 1 0
                              0 0 0 1))

;; todo: lerp, nlerp, slerp

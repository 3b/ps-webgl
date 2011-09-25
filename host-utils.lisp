(in-package #:webgl-ps)

;;; we could define macros in parenscript code with eval-when, but then
;;; we have to load the ps code before we can load libraries that depend
;;; on this one...
;;; eventually, probably should set up ASDF to compile ps code to js 'fasl'
;;; files, but for now just define the macros and such here...


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
                ,@body))))))

(load "~/Development/widget-dev/setup.lisp")

(in-package :cl-jupyter-widgets)

(defparameter *int* (make-instance 'int-text))

(fdefinition 'cl-jupyter-widgets::open)

(get-keys *int*)
(print (get-state *int*))

(defclass foo ()
  ((bar :initarg :bar :accessor bar :metadata (:sync t)))
  (:metaclass traitlets:traitlet-class))

(loop for slot-def in (clos:class-slots (find-class 'int))
   when (eq (clos:slot-definition-name slot-def) '%view-name)
     return slot-def)



(load "~/quicklisp/setup.lisp")
(progn
  (asdf:load-asd "/Users/meister/Development/widget-dev/cl-jupyter-widgets/src/cl-jupyter-widgets.asd")
  (asdf:load-system :cl-jupyter-widgets :verbose t))





(in-package :cl-jupyter-widgets)

(defparameter *w* (make-instance 'int))

(keys *w*)

(instance nil)

(tuple 

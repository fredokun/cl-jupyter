(in-package :nglv)

(defclass shape ()
  ((view :initarg :view :accessor view :initform nil)
   (names :accessor names :type list :initform (list "mesh" "sphere" "ellipsoid" "cylinder" "cone" "arrow"))))

;;I would like to imitate shape.py::__init__
(defmethod initialize-instance :after ((self shape) &key)
  (_make_func self (names self)))



(defmethod _make_func ((self shape) names)
  (flet ((make_func (name)
	   (lambda (this &rest args)
	     "check `add` method"
	     (apply #'add this name args)))
	 )
    (dolist (name names)
      (let* ((func_name (concatenate 'string "add_" name))
	     (func (make_func name)))))))
	

(defmethod add ((self shape) &rest args)
  (_add_shape (view self) args))

(in-package :nglv)

(cljw:widget-log "in shape.lisp~%")

(defclass shape ()
  ((view :initarg :view :accessor view :initform nil)
   (names :accessor names :type list :initform (list "mesh" "sphere" "ellipsoid" "cylinder" "cone" "arrow" "label" "text"))))

;;I would like to imitate shape.py::%-init--
(defmethod initialize-instance :after ((self shape) &key)
  (%make-func self (names self)))



(defmethod %make-func ((self shape) names)
  (flet ((make-func (name)
	   (lambda (this &rest args)
	     "check `add` method"
	     (apply #'add this name args)))
	 )
    (dolist (name names)
      (let* ((func-name (concatenate 'string "add_" name))
	     (func (make-func name)))))))
	

(defmethod add ((self shape) &rest args)
  (%add-shape (view self) args))

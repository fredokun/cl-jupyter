(in-package :cl-jupyter-widgets)

(defclass value-widget (widget)
  ((value :initarg :value :accessor value
	 :initform nil)))

(defmethod get-interact-value ((self value-widget))
  (value self))



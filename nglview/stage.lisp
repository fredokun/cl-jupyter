(in-package :nglv)

(defclass stage ()
  ((%view :initarg :view
	  :accessor view)))

(defmethod set-parameters ((stage stage) kwargs)
  (%remote-call (view stage) "setParameters"
		:target "Stage"
		:kwargs (camelize-dict kwargs)))


  

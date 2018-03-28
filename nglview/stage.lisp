(in-package :nglv)

(cljw:widget-log "stage.lisp~%")

(defclass stage ()
  ((%view :initarg :view
	  :accessor view)))

(defmethod set-parameters ((stage stage) kwargs)
  (%remote-call (view stage) "setParameters"
		:target "Stage"
		:kwargs (camelize-dict kwargs)))


  
(cljw:widget-log "end of stage.lisp~%")

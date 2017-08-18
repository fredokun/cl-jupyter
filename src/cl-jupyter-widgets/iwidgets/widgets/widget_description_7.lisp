(in-package :cl-jupyter-widgets)

(defclass-widget-register description-style (style core-widget widget)
  ((%description-width :initarg :description-width :accessor description-width
		       :initform (unicode)
		       :metadata (:sync t :json-name "description_width"
					:help "Width of the description to the side of the control")))
  (:default-initargs
   :model-name (unicode "DescriptionStyleModel"))
  (:metaclass traitlets:traitlet-class))


(defclass description-widget (domwidget core-widget)
  ((%description :initarg :description :accessor description
		 :initform (unicode "")
		 :metadata (:sync t :json-name "description" :help "Description of the control."))
   #|(%style :initarg :style :accessor style
	   :initform (make-instance 'instance-dict :instance (make-instance 'description-style))
	   :metadata #.`(:sync t :json-name "style" :help "Styling customizations"
                            ,@*widget-serialization*))|#)
  (:metaclass traitlets:traitlet-class))



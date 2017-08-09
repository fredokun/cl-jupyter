(in-package :cl-jupyter-widgets)

(defclass domwidget (widget)
  ((%dom-classes :initarg :dom-classes :accessor dom-classes
		 :type vector
		 :initform #()
		 :metadata
		 (:sync t
			:json-name "_dom_classes"))
   (%layout :initarg :layout :accessor layout
	    :type (or instance null)
	    :initform (make-instance 'layout)
	    :metadata
	    #.`(:sync t
		      :json-name "layout"
		      ,@*widget-serialization*))
   )
  (:default-initargs
   :model-name (unicode "DOMWidgetModel")
    :layout (make-instance 'layout))
  (:metaclass traitlets:traitlet-class))



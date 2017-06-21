(in-package :cl-jupyter-widgets)

(defclass color-picker (dom-widget)
  ((value :initarg :value :accessor value
	   :type unicode 
	   :initform "black"
	   :metadata (:sync t
			    :json-name "value"
			    :help "Color"))
   (concise :initarg :concise :accessor concise
	     :type boolean
	     :initform :false
	     :metadata (:sync t
			      :json-name "concise"
			      ))
   (description :initarg :description :accessor description
		 :type unicode
		 :initform (unicode "")
		 :metadata (:sync t
				  :json-name "description"
				  )))
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "ColorPickerView")
    :model-name (unicode "ColorPickerModel"))
  (:metaclass traitlets:traitlet-class))

(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))

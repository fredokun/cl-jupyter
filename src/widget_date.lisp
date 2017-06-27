(in-package :cl-jupyter-widgets)

(defclass date-picker (labeled-widget dom-widget core-widget)
  ((value :initarg value :accessor value
	  :type unicode
	  :initform (unicode "")
	  :metadata (:sync t
			   :json-name "value"
			   ;;what to put for **datetime_serialization?
			   )))
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "DatePickerView")
    :modelname (unicode "DatePickerModel"))
  (:metaclass traitlets:traitlet-class))
	  
	  

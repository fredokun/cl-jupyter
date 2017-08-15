(in-package :cl-jupyter-widgets)

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_date.py#L18
(defclass-widget-register date-picker (description-widget domwidget core-widget)
  ((value :initarg value :accessor value
	  :type unicode
	  :initform (unicode "")
	  :metadata (:sync t
			   :json-name "value"
			   ;;what to put for **date_serialization?
			  ;; https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_date.py#L19
			   )))
  (:default-initargs
   ;:model-module (unicode "jupyter-js-widgets")
    ;:view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "DatePickerView")
    :modelname (unicode "DatePickerModel"))
  (:metaclass traitlets:traitlet-class))
	  
	  

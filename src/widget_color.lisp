(in-package :cl-jupyter-widgets)
;;;Python code: https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets6/widgets/widget_color.py#L18
(defclass color-picker (labeled-widget dom-widget core-widget)
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
			      )))
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "ColorPickerView")
    :model-name (unicode "ColorPickerModel"))
  (:metaclass traitlets:traitlet-class))

(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))

(in-package :cl-jupyter-widgets)
;;;Python code: https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets6/widgets/widget_color.py#L18
(defclass-widget-register color-picker (description-widget value-widget core-widget)
  ((value :initarg :value :accessor value
	   :type unicode 
	   :initform "black"
	   :metadata (:sync t
			    :json-name "value"
			    :help "The color value."))
   (concise :initarg :concise :accessor concise
	     :type bool
	     :initform :false
	     :metadata (:sync t
			      :json-name "concise"
			      :help "Display short version with justa  color selector."
			      ))
   (disabled :initarg :disabled :accessor disabled
	     :type bool
	     :initform :false
	     :metadata (:sync t
			      :json-name "disabled"
			      :help "Enable or disable user changes.")))
  (:default-initargs
    :view-name (unicode "ColorPickerView")
    :model-name (unicode "ColorPickerModel"))
  (:metaclass traitlets:traitlet-class))

(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))

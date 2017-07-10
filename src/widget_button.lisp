(in-package :cl-jupyter-widgets)
;;;Python code: drmeister/spy-ipykernel/blob/master/ipywidgets6/widgets/widget_button.py#L29
(defclass button-style (style core-widget)
  ((button_color :initarg button_color :accessor button_color
		 :type unicode
		 :initform (unicode "")
		 :metadata (:sync t
				  :json-name "button_color"))
   (font_weight :initarg :font_weight :accessor font_weight
		:type unicode
		:initform (unicode "")
		:metadata (:sync t
				 :json-name "font_weight")))
  (:default-initargs
   :model-name (unicode "ButtonStyleModel"))
  (:metaclass traitlets:traitlet-class))



(defclass button (dom-widget core-widget)
 ((description :initarg :description :accessor description
		:type unicode
		:initform (unicode "")
		:metadata (:sync t
				 :json-name "description"
				 :help "Button label."))
  (tooltip :initarg :tooltip :accessor tooltip
	    :type unicode
	    :initform (unicode "")
	    :metadata (:sync t
			     :json-name "tooltip"
			     :help "Tooltip caption of the button."))
  (disabled :initarg :disabled :accessor disabled
	     :type bool
	     :initform :false
	     :metadata (:sync t
			      :json-name "disabled"
			      :help "Enable or disabled user changes."))
  (icon :initarg :icon :accessor icon
	 :type unicode
	 :initform (unicode "")
	 :metadata (:sync t
			  :json-name "icon"
			  :help "Font-awesome icon."))
  (button_style :initarg :button_style :accessor button_style
		 :type unicode
		 :initform (unicode "")
		 :metadata (:sync t
				  :json-name "button_style"
				  :help "Use a predefined styling for the button. Options include: \"primary\", \"success\", \"info\", \"warning\", \"danger\", and \"""\"."))
  (style :accessor style
	 :initform (make-instance 'button-style)
	 :metadata (:sync t
			  :json-name "style"
			;;  :to-json json-to-widget
			  :from-json widget-to-json))
  )
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "ButtonView")
    :model-name (unicode "ButtonModel"))
 (:metaclass traitlets:traitlet-class))
	    
;;Still need an 'on_click' method (to register a callback to execute when the button is clicked) and a '_handle_button_msg' method (to handle a message from the front-end).



;;Start Meister code copy
(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))


(in-package :cl-jupyter-widgets)

(defclass button (dom-widget)
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
	     :type boolean
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
  (background_color :initarg :background_color :accessor background_color
		    :type boolean
		    :initform :null
		    :metadata (:sync t
				     :json-name "background_color"))
  (color :initarg :color :accessor color
	 :type boolean
	 :initform :null
	 :metadata (:sync t
			  :json-name "color"))
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


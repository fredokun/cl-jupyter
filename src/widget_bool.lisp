(in-package :cl-jupyter-widgets)

(defclass %bool (labeled-widget value-widget core-widget)
  ((%value :initarg :value :accessor value
	   :type boolean
	   :initform :false
	   :metadata (:sync t
			    :json-name "value"
			    :help "Bool value"))
   (%disabled :initarg :disabled :accessor disabled
	      :type boolean
	      :initform :false
	      :metadata (:sync t
			       :json-name "disabled"
			       :help "Enable or disable user changes.")))
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :model-name  (unicode "BoolModel")
    )
  (:metaclass traitlets:traitlet-class))

(defclass checkbox (%bool)
  ()
  (:default-initargs
   :view-name (unicode "CheckboxView")
    :model-name (unicode "CheckboxModel")
    )
  (:metaclass traitlets:traitlet-class))

(defclass toggle-button(%bool)
  ((tooltip :initarg :tooltip :accessor tooltip
	     :type unicode
	     :initform (unicode "")
	     :metadata (:sync t
			      :json-name "tooltip"
			      :help "Tooltip Caption of the toggle button."))
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
				   :help "Use a predefined styling for the button.Options include: \"primary\", \"success\", \"info\", \"warning\", \"danger\", \"""\".")))
  (:default-initargs
   :view-name (unicode "ToggleButtonView")
   :model-name (unicode "ToggleButtonModel"))
  (:metaclass traitlets:traitlet-class))


(defclass valid (%bool)
  ((readout :accessor readout
	    :type unicode
	    :initform (unicode "Invalid")
	    :metadata (:sync t
			     :json-name "readout"
			     :help "Message displayed when the value is False.")))
  (:default-initargs
    :view-name (unicode "ValidView")
    :model-name (unicode "ValidModel"))
  (:metaclass traitlets:traitlet-class))

(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))
#|
(defclass toggle-button (%bool)
  ()
  (:default-initargs
   :view-name (unicode "ToggleButtonView")
    :model-name (unicode "ToggleButtonModel")
    :tool-tip (unicode)
    :icon (unicode ""))
  (:metaclass traitlets:traitlet-class))
   ;;;HELP ME :button-style
    ;; button_style = CaselessStrEnum(
   ; ;   values=['primary', 'success', 'info', 'warning', 'danger', ''], default_value='',
      ;;  help="""Use a predefined styling for the button.""").tag(sync=True)

|#

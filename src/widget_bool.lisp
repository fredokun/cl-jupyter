(in-package :cl-jupyter-widgets)

(defclass %bool (dom-widget)
  ((%value :initarg :value :accessor value
	   :type boolean
	   :initform :false
	   :metadata (:sync t
			    :json-name "value"
			    :help "Bool value"))
   (%description :initarg :description :accessor description
		 :type unicode
		 :initform (unicode "")
		 :metadata (:sync t
				  :json-name "description"
				  :help "Description of the boolean (label)."))
   (%disabled :initarg disabled :accessor disabled
	      :type boolean
	      :initform :false
	      :metadata (:sync t
			       :json-name "disabled"
			       :help "Enable or disable user changes.")))
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :model-name  (unicode "jupyter-js-widgets")
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
  ((%tooltip :initarg :tooltip :accessor tooltip
	     :type unicode
	     :initform (unicode "")
	     :metadata (:sync t
			      :json-name "tooltip"
			      :help "Tooltip Caption of the toggle button."))
   (%icon :initarg :icon :accessor icon
	  :type unicode
	  :initform (unicode "")
	  :metadata (:sync t
			   :json-name "icon"
			   :help "Font-awesome icon."))
   (%button-style :initarg :button-style :accessor button-style
		  :type string ;;I should be a caseless String Enum
		  :initform ""
		  :metadata (:sync t
				   :json-name "button-style"
				   :help "Use a predefined styling for the button."))))
(defclass valid (%bool)
  ()
  (:default-initargs
   :readout (unicode "Invalid")
    :view-name (unicode "ValidView")
    :model-name (unicode "ValidModel")))

(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))

(defclass toggle-button (%bool)
  ()
  (:default-initargs
   :view-name (unicode "ToggleButtonView")
    :model-name (unicode "ToggleButtonModel")
    :tool-tip (unicode)
    :icon (unicode "")))
   ;;;HELP ME :button-style
    ;; button_style = CaselessStrEnum(
   ; ;   values=['primary', 'success', 'info', 'warning', 'danger', ''], default_value='',
      ;;  help="""Use a predefined styling for the button.""").tag(sync=True)

(defclass valid (%bool)
  ()
  (:default-initargs
   :readout (unicode "Invalid")
    :view-name (unicode "ValidView")
    :model-name (unicode "ValidModel")))

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



(defclass button (domwidget core-widget)
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
			  :help "Font-awesome icon. Styles we know about include: check, bullseye, camera, refresh, eye-slash, trash, eraser."))
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
  (%click-handlers :initform (make-instance 'callback-dispatcher)
		   :accessor click-handlers)
  )
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "ButtonView")
    :model-name (unicode "ButtonModel"))
 (:metaclass traitlets:traitlet-class))
	    

(defmethod initialize-instance :after ((self button) &key)
  (on-msg self #'%handle-button-msg))


(defmethod on-click ((self button) callback &key (remove nil))
  (register-callback (click-handlers self) callback :remove remove)
  (values))

(defmethod %handle-button-msg ((self button) content buffers)
  "Handle a msg from the front-end.

        Parameters
        ----------
        content: dict
            Content of the msg."
  (when (string= (cdr (assoc "event" content)) "click")
    (click-handlers self))
  (values))


;;Start Meister code copy
(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))


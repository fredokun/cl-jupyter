(in-package :cl-jupyter-widgets)

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_controller.py#L17
(defclass-widget-register button (domwidget value-widget core-widget)
  ((value :initarg :value :reader value
	  :type (float 0.0 1.0)
	  :metadata (:sync t
			   :json-name "value"
			   :help "Min: 0.0 Max: 1.0"))
   (pressed :initarg :pressed :reader pressed
	    :type bool
	    :initform :false ;added this just in case pressed is not supplied. It'll always be not pressed to start, right?
	    :metadata (:sync t
			     :json-name "pressed"))
   )
  (:default-initargs
   ;:model-module (unicode "jupyter-js-widgets")
   ;:view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "ControllerButtonView")
    :model-name (unicode "ControllerButtonModel")
    )
  (:metaclass traitlets:traitlet-class))

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_controller.py#L29
(defclass-widget-register axis (domwidget value-widget core-widget)
  ((value :initarg :value :reader value
	  :type (float -1.0 1.0)
	  :metadata (:sync t
			   :json-name "value")))
  
  (:default-initargs
   ;:model-module (unicode "jupyter-js-widgets")
    ;:view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "ControllerAxisView")
    :model-name (unicode "ControllerAxisModel")
    )
  (:metaclass traitlets:traitlet-class))

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_controller.py#L40
(defclass-widget-register controller (domwidget core-widget)
  ((index :initarg :index :accessor index
	  :type integer
	  :metadata (:sync t
			   :json-name "value"
			   :help "The id number of the controller."))
   (name :initarg :name :reader name
	 :type unicode
	 :metadata (:sync t
			  :json-name "name"
			  :help "The name of the controller."))
   (mapping :initarg :mapping :reader mapping
	    :type unicode
	    :metadata (:sync t
			     :json-name "mapping"
			     :help "The name of the control mapping."))
   (connected :initargs :connected :reader connected
	      :type bool
	      :metadata (:sync t
			       :json-name "connected"
			       :help "Whether the gamepad is connected."))
   (timestamp :initarg :timestamp :reader timestamp
	      :type float
	      :metadata (:sync t
			       :json-name "timestamp"
			       :help "The last time the data form this gamepad was updated."))

   ;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_controller.py#L52
   (buttons :initarg :buttons :reader buttons
	    :type list
	    :initform (make-instance 'button) ;;trait=Instance(Button)
	    :metadata (:sync t
			     :json-name "buttons"
			     :from-json json-to-widget
			     :to-json widget-to-json
			     ))
   (axes :initarg :axes :reader axes
	 :type list
	 :initform (make-instance 'axis) ;;trait=Instance(Axis)
	 :metadata (:sync t
			  :json-name "axes"
			  :from-json json-to-widget
			  :to-json widget-to-json
			  )))
  (:default-initargs
   ;:model-module (unicode "jupyter-js-widgets")
   ; :view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "ControllerView")
    :model-name (unicode "ControllerModel"))
  (:metaclass traitlets:traitlet-class))
   

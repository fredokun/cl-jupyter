(in-package :cl-jupyter-widgets)

(defclass button(value-widget core-widget)
  ((value :initarg :value :reader value
	  :type (float 0.0 1.0)
	  :metadata (:sync t
			   :json-name "value"
			   :help "Min: 0.0 Max: 1.0"))
   (pressed :initarg :pressed :reader pressed
	    :type bool
	    :metadata (:sync t
			     :json-name "pressed"))
   )
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "ControllerButtonView")
    :model-name (unicode "ControllerButtonModel")
    )
  (:metaclass traitlets:traitlet-class))

(defclass axis(value-widget core-widget)
  ((value :initargs :value :reader value
	  :type (float -1.0 1.0)
	  :metadata (:sync t
			   :json-name "value")))
  
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "ControllerAxisView")
    :model-name (unicode "ControllerAxisModel")
    )
  (:metaclass traitlets:traitlet-class))

(defclass controller (dom-widget core-widget)
  ((index :initargs :index :accessor index
	  :type integer
	  :metadata (:sync t
			   :json-name "value"))
   (name :initargs :name :reader name
	 :type unicode
	 :metadata (:sync t
			  :json-name "name"))
   (mapping :initargs :mapping :reader mapping
	    :type unicode
	    :metadata (:sync t
			     :json-name "mapping"))
   (connected :initargs :connected :reader connected
	      :type bool
	      :metadata (:sync t
			       :json-name "connected"))
   (timestamp :initargs :timestamp :reader timestamp
	      :type float
	      :metadata (:sync t
			       :json-name "timestamp"))

   ;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_controller.py#L52
   (buttons :initargs :buttons :reader buttons
	    :type list
	    :initform (make-instance 'button) ;;trait=Instance(Button)
	    :metadata (:sync t
			     :json-name "buttons"
			     ;;what to put for **widget_serialization?
			     ))
   (axes :initargs :axes :reader axes
	 :type list
	 :initform (make-instance 'axis) ;;trait=Instance(Axis)
	 :metadata (:sync t
			  :json-name "axes"
			  ;;what to put for **widget_serialiation?
			  )))
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "ControllerView")
    :model-name (unicode "ControllerModel"))
  (:metaclass traitlets:traitlet-class))
   

(in-package :cl-jupyter-widgets)

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_string.py#L17
(defclass %string (labeled-widget value-widget core-widget)
  ((value :initarg :value :accessor value
	   :type unicode
	   :initform (unicode "")
	   :metadata (:sync t
			    :json-name "value"
			    :help "String value"))
   (disabled :initarg :disabled :accessor disabled
	      :type bool
	      :initform :false
	      :metadata (:sync t
			       :json-name "disabled"
			       :help "enable or disable user changes"))
   (description :initarg :description :accessor description
		 :type unicode
		 :initform (unicode "")
		 :metadata (:sync t
				  :json-name "description"
				  :help "Description of the value this widget represents"))
   (placeholder :initarg :placeholder :accessor placeholder
		 :type unicode
		 :initform (unicode "")
		 :metadata (:sync t
				  :json-name "placeholder"
				  :help "Placeholder text to display when nothing has been typed")))

   (:default-initargs
       :model-module (unicode "jupyter-js-widgets")
     :model-name (unicode "StringModel")
     :view-module (unicode "jupyter-js-widgets")
   )
   (:metaclass traitlets:traitlet-class))


;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_string.py#L41
(defclass html(%string)
  ()
  (:default-initargs
   :view-name (unicode "HTMLView")
    :model-name (unicode "HTMLModel")
    )
  (:metaclass traitlets:traitlet-class))


;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_string.py#L47
(defclass html-math(%string)
  ()
  (:default-initargs
   :view-name (unicode "HTMLMathView")
    :model-name (unicode "HTMLMathModel")
    )
  (:metaclass traitlets:traitlet-class))


;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_string.py#L54
(defclass label(%string)
  ()
  (:default-initargs
   :view-name (unicode "LabelView")
    :model-name (unicode "LabelModel")
    )
  (:metaclass traitlets:traitlet-class))

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_string.py#L65
(defclass textarea(%string)
  ()
  (:default-initargs
   :view-name (unicode "TextareaView")
    :model-name (unicode "TextareaModel")
    )
  (:metaclass traitlets:traitlet-class))


;;are these methods for text class needed?
;;How would we go about translating these?
;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_string.py#L81

(defclass text(%string)
  ()
  (:default-initargs
   :view-name (unicode "TextView")
    :model-name (unicode "TextModel")
    )
  (:metaclass traitlets:traitlet-class))

 

(in-package :cl-jupyter-widgets)

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_string.py#L17
(defclass %string (description-widget value-widget core-widget)
  ((value :initarg :value :accessor value
	   :type unicode
	   :initform (unicode "")
	   :metadata (:sync t
			    :json-name "value"
			    :help "String value"))
   (placeholder :initarg :placeholder :accessor placeholder
		 :type unicode
		 :initform (unicode "")
		 :metadata (:sync t
				  :json-name "placeholder"
				  :help "Placeholder text to display when nothing has been typed")))
   (:default-initargs
     :model-name (unicode "StringModel")
   )
   (:metaclass traitlets:traitlet-class))




;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_string.py#L41
(defclass-widget-register html (%string)
  ()
  (:default-initargs
   :view-name (unicode "HTMLView")
    :model-name (unicode "HTMLModel")
    )
  (:metaclass traitlets:traitlet-class))


;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_string.py#L47
(defclass-widget-register html-math (%string)
  ()
  (:default-initargs
   :view-name (unicode "HTMLMathView")
    :model-name (unicode "HTMLMathModel")
    )
  (:metaclass traitlets:traitlet-class))


;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_string.py#L54
(defclass-widget-register label(%string)
  ()
  (:default-initargs
   :view-name (unicode "LabelView")
    :model-name (unicode "LabelModel")
    )
  (:metaclass traitlets:traitlet-class))

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_string.py#L65
(defclass-widget-register textarea(%string)
  ((rows :initarg :rows :accessor rows
	 :type integer
	 :initform 0
	 :metadata (:sync t
			  :json-name "rows"
			  :help "The number of rows to display."))
   (disabled :initarg :disabled :accessor disabled
	     :type bool
	     :initform :false
	     :metadata (:sync t
			      :json-name "disabled"
			      :help "Enable or disable user changes."))
   (continuous-update :initarg :continuous-update :accessor continuous-update
		      :type bool
		      :initform :true
		      :metadata (:sync t
				       :json-name "continuous_update"
				       :help "Update the value as the user types. If False, update on submission, e.g., pressing Enter or navigating away.")))
  (:default-initargs
   :view-name (unicode "TextareaView")
    :model-name (unicode "TextareaModel")
    )
  (:metaclass traitlets:traitlet-class))


;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_string.py#L81

(defclass-widget-register text (%string)
  ((disabled :initarg :disabled :accessor disabled
	     :type bool
	     :initform :false
	     :metadata (:sync t
			      :json-name "disabled"
			      :help "Enable or disable the user changes"))
   (continuous-update :initarg :continuous-update :accessor continuous-update
		      :type bool
		      :initform :true
		      :metadata (:sync t
				       :json-name "continuous_update"
				       :help "Update the value as the user types. If False, update on submission, e.g., pressing Enter or navigating away."))
   (%submission-callbacks :accessor submission-callbacks
			  :initform (make-instance 'callback-dispatcher)))
  (:default-initargs
   :view-name (unicode "TextView")
    :model-name (unicode "TextModel")
    )
  (:metaclass traitlets:traitlet-class))

(defmethod on-submit ((self text) callback &key (remove nil))
  (register-callback (submission-callbacks self) callback :remove remove)
  (values))

(defmethod %handle-string-msg ((self text) content buffers)
  ;; I took out the _ argument in ((self text) _ content buffers)
  (when (string= (cdr (assoc "event" content :test #'string=)) "submit")
    (do-call (submission-callbacks self) self))
  (values))


 
(defclass-widget-register password (text)
  ((disabled :initarg :disabled :accessor disabled
	     :type bool
	     :initform :false
	     :metadata (:sync t
			      :json-name "disabled"
			      :help "Enable or disable user changes")))
  (:default-initargs
      :view-name (unicode "PasswordView")
    :model-name (unicode "PasswordModel"))
  (:metaclass traitlets:traitlet-class))

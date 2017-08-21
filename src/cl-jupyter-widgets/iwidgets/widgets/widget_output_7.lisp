(in-package :cl-jupyter-widgets)

(defclass-widget-register output (domwidget core-widget)
  ((msg-id :initarg :msg-id :accessor msg-id
	   :type unicode
	   :initarg (unicode "")
	   :metadata (:sync t
			    :json-name "msg_id"
			    :help "Parent message id of messages to capture."))
   (outputs :initarg :outputs :accessor outputs
	    :type tuple
	    :initform nil
	    :metadata (:sync t
			     :json-name "outputs"
			     :help "The output messages synced from the frontend.")))
  (:default-initargs
      :view-name (unicode "OutputView")
    :model-name (unicode "OutputModel")
    :view-module (unicode "@jupyter-widgets/output")
    :model-module (unicode "@jupyter-widgets/output"))
  (:metaclass traitlets:traitlet-class))


(in-package :cl-jupyter-widgets)

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_output.py#L18
(defclass-widget-register output (domwidget core-widget)
  ((msg-id :initarg :msg-id :accessor msg-id
	   :type unicode
	   :initform (unicode "")
	   :metadata (:sync t
			    :json-name "msg_id"))
   (outputs :initarg :outputs :accessor outputs
	    :type tuple
	    :initform nil
	    :metadata (:sync t
			     :json-name "outputs"
			     :help "The output messages synced from the frontend.")
  (:default-initargs
   :view-name (unicode "OutputView")
    :model-name (unicode "OutputModel")
    :model-module (unicode "@jupyter-js/output")
    :view-module (unicode "@jupyter-js/output")
    )
  (:metaclass traitlets:traitlet-class))

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_output.py#L43
;;are these functions needed?
;;how to go about translating them?

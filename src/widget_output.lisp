(in-package :cl-jupyter-widgets)

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_output.py#L18
(defclass output (dom-widget core-widget)
  ((msg-id :initarg :msg-id :accessor msg-id
	   :type unicode
	   :initform (unicode "")
	   :metadata (:sync t
			    :json-name "msg_id")))
  (:default-initargs
   :view-name (unicode "OutputView")
    :model-name (unicode "OutputModel")
    :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    )
  (:metaclass traitlets:traitlet-class))

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_output.py#L43
;;are these functions needed?
;;how to go about translating them?

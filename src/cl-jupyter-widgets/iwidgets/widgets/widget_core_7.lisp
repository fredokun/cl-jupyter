(in-package :cl-jupyter-widgets)

(defclass core-widget (widget)
  ()
  (:default-initargs
   :model-module (unicode "@jupyter-widgets/controls")
    :model-module-version (unicode *jupyter-widgets-controls-version*)
    :view-module (unicode "@jupyter-widgets/controls")
    :view-module-version (unicode *jupyter-widgets-controls-version*))
  (:metaclass traitlets:traitlet-class))

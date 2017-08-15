(in-package :cl-jupyter-widgets)

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_style.py#L10
(defclass style (widget)
  ()
  (:default-initargs
   :model-name (unicode "StyleModel")
   :view-name (unicode "StyleView")
   :view-module (unicode "@jupyter-widgets/base")
   :view-module-version (unicode *jupyter-widgets-base-version*))
  (:metaclass traitlets:traitlet-class))

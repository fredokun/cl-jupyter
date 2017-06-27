(in-package :cl-jupyter-widgets)

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_style.py#L10
(defclass style (widget)
  ()
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
   :view-module (unicode "jupyter-js-widgets")
   :view-name (unicode "StyleView")
   :model-name (unicode "StyleModel"))
   (:metaclass traitlets:traitlet-class))

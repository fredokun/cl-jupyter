(in-package :cl-jupyter-widgets)

(defclass style (widget)
  ()
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
   :view-module (unicode "jupyter-js-widgets")
   :view-name (unicode "StyleView")
   :model-name (unicode "StyleModel"))
   (:metaclass traitlets:traitlet-class))

(in-package :cl-jupyter-widgets)
;;;Python code: https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets6/widgets/widget_box.py#L18

(defclass-widget-register Box (domwidget core-widget)
  ((children :initarg :children :accessor children
             :type vector
             :initform (vector)
             :metadata (:sync t
                        :json-name "children"
                        :help "Child widgets in the container. Using tuple to force
                                     reassignment to update the list. When a proper 
                                     notifying-list trait exists, that is what should be used."
                        :to-json json-to-widget
                        :from-json widget-to-json
                              ))
   (box_style :initarg :box_style :accessor box_style
              :type unicode
              :initform (unicode "")
              :metadata (:sync t
                         :json-name "box_style"
                         :help "Use a predefined styling for the box. Options include: \"success\", \"info\", \"warning\", \"danger\", and \"\".")))
  (:default-initargs
   :model-name (unicode "BoxModel")
   :view-name (unicode "BoxView"))
  (:metaclass traitlets:traitlet-class))

(defclass-widget-register vbox (Box)
  ()
  (:default-initargs
   :model-name (unicode "VBoxModel")
   :view-name (unicode "VBoxView"))
  (:metaclass traitlets:traitlet-class))

(defclass-widget-register hbox (Box)
  ()
  (:default-initargs
   :model-name (unicode "HBoxModel")
   :view-name (unicode "HBoxView"))
  (:metaclass traitlets:traitlet-class))

(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))

(defmethod %fire-children-displayed ((self Box))
  (loop for child across (children self)
     do
       (%handle-displayed child))
  (values))

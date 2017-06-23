(in-package :cl-jupyter-widgets)

(defclass Box (dom-widget core-widget)
  ((children :initarg :children :accessor children
	      :type vector
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
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :model-name (unicode "BoxModel")
    :view-name (unicode "BoxView"))
  (:metaclass traitlets:traitlet-class))

(defclass proxy (dom-widget)
 ((child :initarg :child :accessor child
	  :initform (make-instance dom-widget)
	  :metadata (:sync t
			   :json-name "child"
			   :help "Child widget of the Proxy"
			   :to-json json-to-widget
			   :from-json widget-to-json
			   )))
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :model-name (unicode "ProxyModel")
    :view-name (unicode "ProxyView"))
  (:metaclass traitlets:traitlet-class))

(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))

(defclass vbox (Box)
  ()
  (:default-initargs
   :model-name (unicode "VBoxModel")
   :view-name (unicode "VBoxView"))
  (:metaclass traitlets:traitlet-class))

(defclass hbox (Box)
  ()
  (:default-initargs
   :model-name (unicode "HBoxModel")
   :view-name (unicode "HBoxView"))
  (:metaclass traitlets:traitlet-class))

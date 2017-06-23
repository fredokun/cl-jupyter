(in-package :cl-jupyter-widgets)

(defclass Box (dom-widget)
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
   (overflow_x :initarg :overflow_x :accessor overflow_x
		:type unicode
		:initform (unicode "")
		:metadata (:sync t
				 :json-name "overflow_x"
				 :help "Specifies what happens to content that is too large for the rendered region. Options include: \"visible\", \"hidden\", \"scroll\", \"auto\", \"initial\", \"inherit\", and \"\"."))
   (overflow_y :initarg :overflow_y :accessor overflow_y
		:type unicode
		:initform (unicode "")
		:metadata (:sync t
				 :json-name "overflow_y"
				 :help "Specifies what happens to content that is too large for the rendered region. Options include: \"visible\", \"hidden\", \"scroll\", \"auto\", \"initial\", \"inherit\", and \"\"."))
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

(defclass place-proxy (proxy)
  ((selector :initarg :selector :accessor selector
	     :type unicode
	     :initform (unicode "")
	     :metadata (:sync t
			      :json-name "selector")))
  (:default-initargs
   :view-name (unicode "PlaceProxyView")
    :model-name (unicode "PlaceProxyModel"))
  (:metaclass traitlets:traitlet-class))

#+deprecated-widgets
(defclass %flex-box (box)
  ((orientation :initarg :orientation :accessor orientation
		 :type unicode
		 :initform (unicode "vertical")
		 :metadata (:sync t
				  :json-name "orientation"
				  :help "Options: \"vertical\" and \"horizontal\"."))
   (flex :initarg :flex :accessor flex
	 :type integer
	 :initform 0
	 :metadata (:sync t
			  :json-name "flex"
			  :help "Specify the flexible-ness of the model."))
   (pack :initarg :pack :accessor pack
	 :type unicode
	 :initform (unicode "start")
	 :metadata (:sync t
			  :json-name "pack"
			  :help "Options include: \"start\", \"center\", \"end\", \"baseline\", \"stretch\"."))
   (align :initarg :align :accessor align
	  :type unicode
	  :initform (unicode "align")
	  :metadata (:sync t
			   :json-name "align"
			   :help "Options include: \"start\", \"center\", \"end\", \"baseline\", \"stretch\".")))
  (:default-initargs
   :view-name (unicode "FlexBoxView")
    :model-name (unicode "FlexBoxModel"))
  (:metaclass traitlets:traitlet-class))
(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))
#|
def VBox(*pargs, **kwargs):
    """Displays multiple widgets vertically using the flexible box model."""
    box = Box(*pargs, **kwargs)
    box.layout.display = 'flex'
    box.layout.flex_flow = 'column'
    box.layout.align_items = 'stretch'
    return box

def HBox(*pargs, **kwargs):
    """Displays multiple widgets horizontally using the flexible box model."""
    box = Box(*pargs, **kwargs)
    box.layout.display = 'flex'
    box.layout.align_items = 'stretch'
    return box
|#


;;;Thought process:box is an instance of box. go to the layout class and change fields display, align-items, and for VBox, change flex-flow. Then return the box object. NOTE THE FIELDS ARE HYPTHENATED NOT UNDERSCORED IN LISP!!! vector + kw args
(defmethod VBox (idk &rest kwargs)
  (let ((box (make-instance 'Box idk kwargs)))
    (with-slots (display flex-flow align-items) (layout box)
      (setf display "flex" flex-flow "column" align-items "stretch")
      box)))

(defmethod HBox (idk &rest kwargs)
  (let ((box (make-instance 'Box idk kwargs)))
    (with-slots (display align-items) (layout box)
      (setf display "flex" align-items "stretch")
      box)))

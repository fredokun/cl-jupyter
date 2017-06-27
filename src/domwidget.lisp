(in-package :cl-jupyter-widgets)

(defclass dom-widget (widget)
  ((%dom-classes :initarg :dom-classes :accessor dom-classes
		 :type list
		 :initform nil
		 :metadata
		 (:sync t
			:json-name "_dom_classes")
		 )
   #+deprecated-widgets
   (%background-color :initarg :background-color :accessor background-color
		      :type (or color (eql :null))
		      :initform :null
		      :metadata
		      (:sync t
			     :json-name "background_color"))
   #+deprecated-widgets
   (%color :initarg :color :accessor color
	   :type (or color (eql :null))
	   :initform :null
	   :metadata
	   (:sync t
		  :json-name "color"))  ; deprecated in ipywidgets 5.0
   (%font-family :initarg :font-family :accessor font-family
		 :type cunicode
		 :initform (unicode "")
		 :metadata
		 (:sync t
			:json-name "font_family"))
   (%font-size :initarg :font-size :accessor font-size
	       :type cunicode
	       :initform (unicode "")
	       :metadata
	       (:sync t
		      :json-name "font_size"))
   (%font-style :initarg :font-style :accessor font-style
		:type string
		:initform (unicode "")
		:metadata
		(:sync t
		       :json-name "font_style"
		       :allowed-caseless-values '("normal" "italic" "oblique" "initial" "inherit" "")
		       ))
   (%font-weight :initarg :font-weight :accessor font-weight
		 :type cunicode
		 :initform (unicode "")
		 :metadata
		 (:sync t
			:json-name "font_weight"
			:allowed-caseless-values '("normal" "bold" "bolder" "lighter" "initial" "inherit" ""
						   "100" "200" "300" "400" "500" "600" "700" "800" "900" )))
   (%visible :initarg :visible :accessor visible
	     :type boolean
	     :initform :true
	     :metadata
	     (:sync t
		    :json-name "visible"
		    :help "Whether the widget is visible. :false collapses the empty space, while NIL preserves the empty space."))
   (%layout :initarg :layout :accessor layout
	    :type (or instance null)
	    :initform (make-instance 'layout)
	    :metadata
	    #.`(:sync t
		      :json-name "layout"
		      ,@*widget-serialization*))
   )
  (:default-initargs
   :model-name (unicode "DOMWidgetModel")
    :visible :true
    :dom-classes nil
    :layout (make-instance 'layout))
  (:metaclass traitlets:traitlet-class))





(defclass labeled-widget (dom-widget)
  ((description :initarg :description :accessor description
	       :type unicode
	       :initform (unicode "")
	       :metadata (:sync t
				:json-name "description"
				:help "Description of the control.")))
  (:default-initargs
   :model-name (unicode "LabeledWidgetModel"))
  (:metaclass traitlets:traitlet-class))

(defclass value-widget (widget)
  ()
  (:metaclass traitlets:traitlet-class))

(defclass core-widget (widget)
  ()
 ; (:default-initargs
 ;  :model-module-version (unicode "")
 ;  :view-module-version (unicode ""))
  (:metaclass traitlets:traitlet-class))
;;;HELP ME! model and view need to be (unicode __frontend_version__)!!!

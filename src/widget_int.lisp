(in-package :cl-jupyter-widgets)


(defclass %int (dom-widget)
  ((%value :initarg :value :accessor value
	   :type integer
	   :initform 0
	   :metadata (:sync t
			    :json-name "value"))
   (%disabled :initarg :disabled :accessor disabled
	      :type boolean
	      :initform :false
	      :metadata (:sync t
			       :json-name "disabled"
			       :help "enable or disable user changes"))
   (%description :initarg :description :accessor description
		 :type unicode
		 :initform (unicode "")
		 :metadata (:sync t
				  :json-name "description"
				  :help "Description of the value this widget represents"))
   )
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
   :view-module (unicode "jupyter-js-widgets")
   )
  (:metaclass traitlets:traitlet-class))


(defclass int-text (%int)
  ()
  (:default-initargs
   :view-name (unicode "IntTextView")
    :model-name (unicode "IntTextModel")
   )
  (:metaclass traitlets:traitlet-class))





(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))


(defun get-keys (object)
  (loop for slot-def in (clos:class-slots (class-of object))
     when (eq (clos:slot-definition-allocation slot-def) :instance)
     when (getf (traitlets::metadata slot-def) :sync)
     collect (clos:slot-definition-name slot-def)))

(defun get-state (object &key key)
  "Gets the widget state, or a piece of it.

        Parameters
        ----------
        key : unicode or iterable (optional)
            A single property's name or iterable of property names to get.

        Returns
        -------
        state : dict of states
        metadata : dict
            metadata for each field: {key: metadata}
        "
  (let ((keys (cond
		((null key) (get-keys object))
		((atom key) (list key))
		((listp key) key)
		(t (error "key must be a slot name, a list or NIL, key -> ~a" key))))
	state)
    (loop for slot-name in keys
       for slot-def = (or (find slot-name (clos:class-slots (class-of object))
				:key #'clos:slot-definition-name)
			  (error "Could not find slot-definition with name ~a" slot-name))
       for to-json = (or (traitlets:traitlet-metadata (class-of object) slot-name :to-json)
			 'widget-to-json)
       collect (cons (or (traitlets:traitlet-metadata (class-of object) slot-name :json-name)
			 (string (clos:slot-definition-name slot-def)))
		     (funcall to-json (widget-slot-value object slot-name) object)))))
    
(defclass layout (widget)
  ((%align-content :initarg :align-content
		   :accessor align-content
		   :type 'cunicode
		   :initform (unicode "")
		   :metadata (:sync t
				    :json-name "align_content"))
   (%align-items :initarg :align-items :accessor align-items
		 :type 'cunicode
		 :initform (unicode "")
		 :metadata (:sync t
				  :json-name "align_items"))
   (%align-self :initarg :align-self :accessor align-self
		:type 'cunicode
		:initform (unicode "")
		:metadata (:sync t
				 :json-name "align_self"))
   (%bottom :initarg :bottom :accessor bottom
	    :type 'cunicode
	    :initform (unicode "")
	    :metadata (:sync t :json-name "bottom"))
   (%border :initarg :border :accessor border
	    :type 'cunicode
	    :initform (unicode "")
	    :metadata (:sync t :json-name "border"))
   (%display :initarg :display :accessor display
	     :type 'cunicode
	     :initform (unicode "")
	     :metadata (:sync t :json-name "display"))
   (%flex :initarg :flex :accessor flex
	  :type 'cunicode
	  :initform (unicode "")
	  :metadata (:sync t :json-name "flex"))
   (%flex-flow :initarg :flex-flow :accessor flex-flow
	       :type 'cunicode
	       :initform (unicode "")
	       :metadata (:sync t :json-name "flex_flow"))
   (%height :initarg :height :accessor height
	    :type 'cunicode
	    :initform (unicode "")
	    :metadata (:sync t :json-name "height"))
   (%justify-content :initarg :justify-content :accessor justify-content
		     :type 'cunicode
		     :initform (unicode "")
		     :metadata (:sync t :json-name "justify_content"))
   (%left :initarg :left :accessor left
	  :type 'cunicode
	  :initform (unicode "")
	  :metadata (:sync t :json-name "left"))
   (%margin :initarg :margin :accessor margin
	    :type 'cunicode
	    :initform (unicode "")
	    :metadata (:sync t :json-name "margin"))
   (%max-height :initarg :max-height :accessor max-height
		:type 'cunicode
		:initform (unicode "")
		:metadata (:sync t :json-name "max_height"))
   (%max-width :initarg :max-width :accessor max-width
	       :type 'cunicode
	       :initform (unicode "")
	       :metadata (:sync t :json-name "max_width"))
   (%min-height :initarg :min-height :accessor min-height
		:type 'cunicode
		:initform (unicode "")
		:metadata (:sync t :json-name "min_height"))
   (%min-width :initarg :min-width :accessor min-width
	       :type 'cunicode
	       :initform (unicode "")
	       :metadata (:sync t :json-name "min_width"))
   (%overflow :initarg :overflow :accessor overflow
	      :type 'cunicode
	      :initform (unicode "")
	      :metadata (:sync t :json-name "overflow"))
   (%overflow-x :initarg :overflow-x :accessor overflow-x
		:type 'cunicode
		:initform (unicode "")
		:metadata (:sync t :json-name "overflow_x"))
   (%overflow-y :initarg :overflow-y :accessor overflow-y
		:type 'cunicode
		:initform (unicode "")
		:metadata (:sync t :json-name "overflow_y"))
   (%padding :initarg :padding :accessor padding
	     :type 'cunicode
	     :initform (unicode "")
	     :metadata (:sync t :json-name "padding"))
   (%right :initarg :right :accessor right
	   :type 'cunicode
	   :initform (unicode "")
	   :metadata (:sync t :json-name "right"))
   (%top :initarg :top :accessor top
	 :type 'cunicode
	 :initform (unicode "")
	 :metadata (:sync t :json-name "top"))
   (%visibility :initarg :visibility :accessor visibility
		:type 'cunicode
		:initform (unicode "")
		:metadata (:sync t :json-name "visibility"))
   (%width :initarg :width :accessor width
	   :type 'cunicode
	   :initform (unicode "")
	   :metadata (:sync t :json-name "width"))
   )
  (:metaclass traitlets:traitlet-class)
  (:default-initargs
      :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets")
    :view-name (unicode "LayoutView")
    :model-name (unicode "LayoutModel"))
  (:documentation
   "From ipywidgets/widgets/widget_layout.py
Layout specification

    Defines a layout that can be expressed using CSS.  Supports a subset of
    https://developer.mozilla.org/en-US/docs/Web/CSS/Reference

    When a property is also accessible via a shorthand property, we only
    expose the shorthand.

    For example:
    - ``flex-grow``, ``flex-shrink`` and ``flex-basis`` are bound to ``flex``.
    - ``flex-wrap`` and ``flex-direction`` are bound to ``flex-flow``.
    - ``margin-[top/bottom/left/right]`` values are bound to ``margin``, etc.
    "
   ))

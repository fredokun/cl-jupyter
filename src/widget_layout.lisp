(in-package :cl-jupyter-widgets)

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

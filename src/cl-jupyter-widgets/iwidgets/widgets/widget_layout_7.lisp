(in-package :cl-jupyter-widgets)

(defparameter *css-properties* (list "inherit" "initial" "unset"))

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_layout.py#L11
;;should I put unicode for CaselessStrEnum?
(defclass-widget-register layout (widget)
  ((%align-content :initarg :align-content
		   :accessor align-content
		   :type '(or nil cunicode)
		   :initform :null
		   :metadata (:sync t
				    :json-name "align_content"
				    :caseless-str-enum (list* "flex-start" "flex-end" "center" "space-between" "space-around" "space-evenly" "stretch" *css-properties*)
				    :help "The align-content css attribute"))
   (%align-items :initarg :align-items :accessor align-items
		 :type '(or nil cunicode)
		 :initform :null
		 :metadata (:sync t
				  :json-name "align_items"
				  :caseless-str-enum (list* "flex-start" "flex-end" "center" "baseline" "stretch" *css-properties*)
				  :help "The align-items CSS attribute."))
   (%align-self :initarg :align-self :accessor align-self
		:type 'cunicode
		:initform :null
		:metadata (:sync t
				 :json-name "align_self"
				 :caseless-str-enum (list* "auto" "flex-start" "flex-end" "center" "baselines" "stretch" *css-properties*)
				 :help "The align-self CSS attribute."))
   (%bottom :initarg :bottom :accessor bottom
	    :type unicode
	    :initform :null
	    :metadata (:sync t :json-name "bottom"
			     :help "The bottom CSS attribute."))
   (%border :initarg :border :accessor border
	    :type unicode
	    :initform :null
	    :metadata (:sync t :json-name "border"
			     :help "The border CSS attribute."))
   (%display :initarg :display :accessor display
	     :type unicode
	     :initform :null
	     :metadata (:sync t :json-name "display"
			      :help "The display CSS attribute."))
   (%flex :initarg :flex :accessor flex
	  :type unicode
	  :initform :null
	  :metadata (:sync t :json-name "flex"
			   :help "The flex CSS attribute."))
   (%flex-flow :initarg :flex-flow :accessor flex-flow
	       :type unicode
	       :initform :null
	       :metadata (:sync t :json-name "flex_flow"
				:help "The flex-flow CSS attribute."))
   (%height :initarg :height :accessor height
	    :type unicode
	    :initform :null
	    :metadata (:sync t :json-name "height"
			     :help "The heigh CSS attribute."))
   (%justify-content :initarg :justify-content :accessor justify-content
		     :type 'cunicode
		     :initform :null
		     :metadata (:sync t :json-name "justify_content"
				      :caseless-str-enum (list* "flex-start" "flex-end" "center" "space-between" "space-around" *css-properties*)
				      :help "The justify-content CSS attribute."))
   (%left :initarg :left :accessor left
	  :type unicode
	  :initform :null
	  :metadata (:sync t :json-name "left"
			   :help "The left CSS attribute."))
   (%margin :initarg :margin :accessor margin
	    :type unicode
	    :initform :null
	    :metadata (:sync t :json-name "margin"
			     :help "The margin CSS attribute."))
   (%max-height :initarg :max-height :accessor max-height
		:type unicode
		:initform :null
		:metadata (:sync t :json-name "max_height"
				 :help "The max-height CSS attribute."))
   (%max-width :initarg :max-width :accessor max-width
	       :type unicode
	       :initform :null
	       :metadata (:sync t :json-name "max_width"
				:help "The max-width CSS attribute."))
   (%min-height :initarg :min-height :accessor min-height
		:type unicode
		:initform :null
		:metadata (:sync t :json-name "min_height"
				 :help "The min-height CSS attribute."))
   (%min-width :initarg :min-width :accessor min-width
	       :type unicode
	       :initform :null
	       :metadata (:sync t :json-name "min_width"
				:help "The min-width CSS attribute."))
   (%overflow :initarg :overflow :accessor overflow
	      :type 'cunicode
	      :initform :null
	      :metadata (:sync t :json-name "overflow"
			       :caseless-str-enum (list* "visible" "hidden" "scroll" "auto" *css-properties*)
			       :help "The overflow CSS attribute."))
   (%overflow-x :initarg :overflow-x :accessor overflow-x
		:type 'cunicode
		:initform :null
		:metadata (:sync t :json-name "overflow_x"
				 :caseless-str-enum (list* "visible" "hidden" "scroll" "auto" *css-properties*)
				 :help "The overflow-x CSS attribute."))
   (%overflow-y :initarg :overflow-y :accessor overflow-y
		:type 'cunicode
		:initform :null
		:metadata (:sync t :json-name "overflow_y"
				 :caseless-str-enum (list* "visible" "hidden" "scroll" "auto" *css-properties*)
				 :help "The overflow-y CSS attribute."))
   (%order :initarg :order :accessor order
		:type 'cunicode
		:initform :null
		:metadata (:sync t :json-name "order"
				 :help "The order CSS attribute."))
   (%padding :initarg :padding :accessor padding
	     :type unicode
	     :initform :null
	     :metadata (:sync t :json-name "padding"
			      :help "The padding CSS attribute."))
   (%right :initarg :right :accessor right
	   :type unicode
	   :initform :null
	   :metadata (:sync t :json-name "right"
			    :help "The right CSS attribute."))
   (%top :initarg :top :accessor top
	 :type unicode
	 :initform :null
	 :metadata (:sync t :json-name "top"
			  :help "The top CSS attribute."))
   (%visibility :initarg :visibility :accessor visibility
		:type 'cunicode
		:initform :null
		:metadata (:sync t :json-name "visibility"
				 :help "The visibility CSS attribute."))
   (%width :initarg :width :accessor width
	   :type unicode
	   :initform :null
	   :metadata (:sync t :json-name "width"
			    :help "The width CSS attribute."))
   )
  (:metaclass traitlets:traitlet-class)
  (:default-initargs
    :view-name (unicode "LayoutView")
    :view-module (unicode "@jupyter-widgets/base")
    :view-module-version (unicode *jupyter-widgets-base-version*)
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

(defclass layout-trait-type ()
  ())

;;;FIXME
#|
class LayoutTraitType(Instance):

    klass = Layout

    def validate(self, obj, value):
        if isinstance(value, dict):
            return super(LayoutTraitType, self).validate(obj, self.klass(**value))
        else:
            return super(LayoutTraitType, self).validate(obj, value)
|#

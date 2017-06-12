(in-package :cl-jupyter-widgets)

(defun unicode (string)
  (make-array (length string) :element-type 'character :initial-contents string))

(defun widget-to-json (x obj)
  (cond ((hash-table-p x)
	 (loop for key being the hash-keys of x
	    using (hash-value value)
	    collect (cons key (widget-to-json value obj))))
	((listp x)
	 (loop for (k . v) in x
	    collect (cons k (widget-to-json v obj))))
	((typep x 'widget)
	 (format nil "IPY_MODEL_~a" (model-id x)))
	(t x)))

(defun json-to-widget (x obj)
  (error "Implement json-to-widget as in https://github.com/drmeister/widget-dev/blob/master/ipywidgets/widgets/widget.py#L33"))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *widget-serialization*
    (list :from-json 'json-to-widget
	  :to-json 'widget-to-json)))
(deftype bool () '(T NULL))
(deftype unicode () '(simple-array character *))
(deftype cunicode () '(simple-array character *))
(deftype color () T)
(deftype instance () T)


(defclass widget ()
  ((%widget-construction-callback :initform nil :accessor widget-construction-callback)
   (%widgets :allocation :class :initform (make-hash-table) :accessor widgets)
   (%widget-types :allocation :class :initform (make-hash-table) :accessor widget-types)
   ;; Traits
   (%model-module :initarg :model-module :accessor model-module
		  :type unicode
		  :initform (unicode "jupyter-js-widgets")
		  :metadata (:json-name "_model_module"
					:sync t
					:help "A requirejs module name in which to find _model_name. If empty, look in the global registry."))
   (%model-name :initarg :model-name :accessor model-name
		:type unicode
		:initform (unicode "WidgetModel")
		:metadata (:sync t
				 :json-name "_model_name"))
   (%view-module :initarg :view-module :accessor view-module
		 :type (or null unicode)
		 :initform nil
		 :metadata (:sync t
				  :json-name "_view_module"))
   (%view-name :initarg :view-name :accessor view-name
	       :type (or null unicode)
	       :initform nil
	       :metadata
	       (:sync t
		      :json-name "_view_name"))
   (%msg-throttle :initarg :msg-throttle :accessor msg-throttle
		  :type integer
		  :initform 3
		  :metadata (:sync t
				   :json-name "msg_throttle") )
   (%comm :initarg :comm :accessor comm :initform nil)
   (%keys :initarg :keys :accessor keys)
   (%property-lock :initarg :property-lock :accessor property-lock)
   (%holding-sync :initarg :holding-sync :accessor holding-sync)
   (%states-to-send :initarg :states-to-send :accessor states-to-send)
   (%display-callbacks :initarg :display-callbacks :accessor display-callbacks)
   (%msg-callbacks :initarg :msg-callbacks :accessor msg-callbacks)
   (%model-id :initarg :model-id :accessor model-id :initform nil)
   )
  (:metaclass traitlets:traitlet-class))

(defun call-widget-constructed (w)
  (when (widget-construction-callback w)
    (funcall widget-construction-callback w)))

(defmethod initialize-instance :around ((w widget) &rest initargs)
  (let ((w (call-next-method)))
    (call-widget-constructed w)
    (widget-open w)))

(defun widget-open (self)
  (multiple-value-bind (state buffer-keys buffers)
      (split-state-buffers self (get-state self))
    (format t "In widget-open~%")
    (format t "state -> ~s~%" state)
    (format t "buffer-keys -> ~s~%" buffer-keys)
    (format t "buffers -> ~s~%" buffers)
    (let ((kwargs (list :target-name "jupyter.widget"
			:data state)))
      (when (model-id self)
	(setf (getf kwargs :comm-id) (model-id self)))
      (setf (comm self) (apply #'comm.__init__ kwargs))
      (when buffers
	;; See comment about buffers at
	;; https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget.py#L205
	(send-state self)))))

(defun binary-types-p (obj)
  ;;; In python this test is
  ;; _binary_types = (memoryview, buffer)
  ;; isinstance(obj, _binary_types)
  nil)

(defun split-state-buffers (self state)
  (let (buffer-keys buffers new-state)
    (loop for (key . value) in state
       do (if (binary-types-p value)
	      (progn
		(push value buffers)
		(push key buffer-keys))
	      (push (cons key value) new-state)))
    (values new-state buffer-keys buffers)))

       
(defun send-state (self &key key)
  "From https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget.py#L252
   Sends the widget state, or a part of the widget state to the front-end.
*Arguments
key : a key or a list of keys (optional)
      A property's name or a list of property names to sync with the front-end"
  (let ((state (get-state self :key key)))
    (multiple-value-bind (state buffer-keys buffers)
	(split-state-buffers self state)
      (let ((msg (list (cons "method" "update")
		       (cons "state" state)
		       (cons "buffers" buffer-keys))))
	(widget-send* self msg :buffers buffers)))))

(defmethod widget-send (self content &key buffers)
  "Send a custom msg to the widget model in the front-end.
*Arguments
content : alist - Content of the message to send
buffers : list  - A list of binary buffers "
  (widget-send* self (list (cons "method" "custom")
		    (cons "content" content))
	 :buffers buffers))

(defun widget-send* (self msg &key buffers)
  "Sends a message to the widget model in the front-end.
See: https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget.py#L485
Sends a message to the model in the front-end."
  (send (comm self) :data msg :buffers buffers))


(defclass dom-widget (widget)
  ((%dom-classes :initarg :dom-classes :accessor dom-classes
		 :type list
		 :initform nil
		 :metadata
		 (:sync t
			:json-name "dom_classes")
		 )
   (%background-color :initarg :background-color :accessor background-color
		      :type (or color null)
		      :initform nil
		      :metadata
		      (:sync t
			     :json-name "background_color"))
   (%color :initarg :color :accessor color
	   :type (or color null)
	   :initform nil
	   :metadata
	   (:sync t
		  :json-name "color"))
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
		:type (member :normal :italic :oblique :initial :inherit nil)
		:initform nil
		:metadata
		(:sync t
		       :json-name "font_style"))
   (%font-weight :initarg :font-weight :accessor font-weight
		 :type (or (member :normal :bold :bolder :lighter :initial :inherit nil)
			   #.(list* 'member (loop for weight from 100 below 1000 by 100
					       collect weight)))
		 :initform nil
		 :metadata
		 (:sync t
			:json-name "font_weight"))
   (%visible :initarg :visible :accessor visible
	     :type (or bool null)
	     :initform nil
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
    :visible t
    :dom-classes nil
    :layout (make-instance 'layout))
  (:metaclass traitlets:traitlet-class))



(defclass int (dom-widget)
  ((%value :initarg :value :accessor value
	   :type integer
	   :initform 0
	   :metadata (:sync t
			    :json-name "value"))
   (%disabled :initarg :disabled :accessor disabled
	      :type bool
	      :initform nil
	      :metadata (:sync t
			       :json-name "disabled"
			       :help "enable or disable user changes")))
  (:metaclass traitlets:traitlet-class))


(defclass int-text (int)
  ()
  (:default-initargs
;;   :view-name (unicode "IntTextView")
   ;;    :model-name (unicode "IntTextModel")
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

(in-package :cl-jupyter-widgets)

(defclass %int (dom-widget)
  ((value :initarg :value :accessor value
	   :type integer
	   :initform 0
	   :metadata (:sync t
			    :json-name "value"))
   (disabled :initarg :disabled :accessor disabled
	      :type boolean
	      :initform :false
	      :metadata (:sync t
			       :json-name "disabled"
			       :help "enable or disable user changes"))
   (description :initarg :description :accessor description
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

(defclass %bounded-int (%int)
  ((step :initarg :step :accessor step
	 :type integer
	 :initform 1
	 :metadata (:sync t
			  :json-name "step"
			  :help "Minimum step to increment the value (ignored by some views)."))
  (max :initarg :max :accessor max
	:type integer
	:initform 100
	:metadata (:sync t
			 :json-name "max"
			 :help "Max value."))
  (min :initarg :min :accessor min
	:type integer
	:initform 0
	:metadata (:sync t
			 :json-name "min"
			 :help "Min value."))
   )
  (:metaclass traitlets:traitlet-class))

(defclass bounded-int-text (%bounded-int)
  ()
  (:default-initargs
   :view-name (unicode "IntTextView")
    :model-name (unicode "IntTextModel"))
  (:metaclass traitlets:traitlet-class))

(defclass int-text (%int)
  ()
  (:default-initargs
   :view-name (unicode "IntTextView")
    :model-name (unicode "IntTextModel")
   )
  (:metaclass traitlets:traitlet-class))

(defclass int-slider (%bounded-int)
  ((orientation :initarg :orientation :accessor orientation
		 :type unicode
		 :initform "horizontal"
		 :metadata (:sync t
				  :json-name "orientation"
				  :help "vertical or horizontal"))
   (_range :initarg :range :accessor range
	   :type boolean
	   :initform :false
	   :metadata (:sync t
			    :json-name "_range"
			    :help "Display a range selector"))
   (readout :initarg :readout :accessor readout
	     :type boolean
	     :initform :true
	     :metadata (:sync t
			      :json-name "readout"
			      :help "Dispaly the current value of the slider next to it."))
   (readout-format :initarg :readout-format :accessor readout-format
		    :type unicode
		    :initform (unicode "d")
		    :metadata (:sync t
				     :json-name "readout_format"
				     :help "Format for the readout."))
   (slider-color :initarg :slider-color :accessor slider-color
		  :type unicode
		  :initform (unicode "None")
		  :metadata (:sync t
				   :json-name "slider_color"
				   :help "Color of the slider"))
   (continuous-update :initarg :continuous-update :accessor continuous-update
		       :type boolean
		       :initform :true
		       :metadata (:sync t
					:json-name "continuous_update"
					:help "Update the value of the widget as the user is holding the slider.")))
  (:default-initargs
   :view-name (unicode "IntSliderView")
    :model-name (unicode "IntSliderModel"))
  (:metaclass traitlets:traitlet-class))


(defclass int-progress (%bounded-int)
  ((orientation :initarg :orientation :accessor orientation
		 :type unicode
		 :initform (unicode "horizontal")
		 :metadata (:sync t
				  :json-name "orientation"
				  :help "vertical or horizontal."))
   (bar-style :initarg :bar-style :accessor bar-style
	       :type unicode
	       :initform (unicode "")
	       :metadata (:sync t
				:json-name "bar_style"
				:help "Use a predefined styling for the progress bar. Options: \"success\", \"info\", \"warning\", and \"danger\". Default: \"\".")))
  (:default-initargs
   :view-name (unicode "ProgressView")
    :model-name (unicode "ProgressModel"))
  (:metaclass traitlets:traitlet-class))


(defclass %int-range(%int)
  ((value :initarg :value :accessor value
	  :type tuple
	  :initform (tuple 0 1)
	  :metadata (:sync t
			   :json-name "value"
			   :help "Tuple of (lower, upper) bounds"))
   )
  (:metaclass traitlets:traitlet-class))

(defclass %bounded-int-range(%int-range)
  ((step :initarg :step :accessor step
	 :type integer
	 :initform 1
	 :metadata (:sync t
			  :json-name "step"
			  :help "Minimum step that the value can take (ignored by some views)"))
   (max :initarg :max :accessor max
	:type integer
	:initform 100
	:metadata (:sync t
			 :json-name "max"
			 :help "Max value"))
   (min :initarg :min :accessor min
	:type integer
	:initform 0
	:metadata (:sync t
			 :json-name "min"
			 :help "Min value"))
   )
  (:metaclass traitlets:traitlet-class))


(defclass int-range-slider(%bounded-int-range)
  ((orientation :initarg :orientation :accessor orientation
		:type unicode
		:initform (unicode "horizontal")
		:metadata (:sync t
				 :json-name "orientation"
				 :help "Vertical or horizontal"))
   (_range :initarg :range :accessor range
	   :type boolean
	   :initform :true
	   :metadata (:sync t
			    :json-name "_range"
			    :help "Display a range selector"))
   (readout :initarg :readout :accessor readout
	    :type boolean
	    :initform :true
	    :metadata (:sync t
			     :json-name "readout"
			     :help "Display the current value of the slider next to it"))
   (slider-color :initarg :slider-color :accessor slider-color
		 :type unicode
		 :initform (unicode "None")
		 :metadata (:sync t
				  :json-name "slider_color"))
   (continuous-update :initarg :continuous-update :accessor continuous-update
		      :type boolean
		      :initform :true
		      :metadata (:sync t
				       :json-name "continuous_update"
				       :help "Update the value of the widget as the user is sliding the slider"))
   )
  (:default-initargs
   :view-name (unicode "IntSliderView")
    :model-name (unicode "IntSliderModel"))
  (:metaclass traitlets:traitlet-class))


(defclass play (%bounded-int)
  ((interval :initarg :interval :accessor interval
	      :type integer
	      :initform 100
	      :metadata (:sync t
			       :json-name "interval"
			       :help "Interval"))
   (_playing :initarg :playing :accessor playing
	     :type boolean
	     :initform :true
	     :metadata (:sync t
			      :json-name "_playing"
			      :help "I don't know.")))
  (:default-initargs
   :view-name (unicode "PlayView")
    :model-name (unicode "PlayModel")
    :view-module (unicode "jupyter-js-widgets")
    :model-module (unicode "jupyter-js-widgets"))
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
    


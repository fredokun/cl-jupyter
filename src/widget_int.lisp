(in-package :cl-jupyter-widgets)
;;;Python code: https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets6/widgets/widget_int.py#L69
(defclass %int (labeled-widget value-widget core-widget)
  ((value :initarg :value :accessor value
	   :type integer
	   :initform 0
	   :metadata (:sync t
			    :json-name "value"))
   (disabled :initarg :disabled :accessor disabled
	      :type bool
	      :initform :false
	      :metadata (:sync t
			       :json-name "disabled"
			       :help "enable or disable user changes"))
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

(defclass int-text (%int)
  ()
  (:default-initargs
   :view-name (unicode "IntTextView")
    :model-name (unicode "IntTextModel")
   )
  (:metaclass traitlets:traitlet-class))

(defclass bounded-int-text (%bounded-int)
  ()
  (:default-initargs
   :view-name (unicode "IntTextView")
    :model-name (unicode "IntTextModel"))
  (:metaclass traitlets:traitlet-class))

(defclass slider-style (style core-widget)
  ((handle_color :initarg :handle_color :accessor handle_color
		:type unicode
		:initform (unicode "")
		:metadata (:sync t
				 :json-name "handle_color")))
  (:default-initargs
   :model-name (unicode "SliderStyleModel"))
  (:metaclass traitlets:traitlet-class))

(defclass int-slider (%bounded-int)
  ((orientation :initarg :orientation :accessor orientation
		 :type unicode
		 :initform "horizontal"
		 :metadata (:sync t
				  :json-name "orientation"
				  :help "vertical or horizontal"))
   (_range :initarg :range :accessor range
	   :type bool
	   :initform :false
	   :metadata (:sync t
			    :json-name "_range"
			    :help "Display a range selector"))
   (readout :initarg :readout :accessor readout
	    :type bool
	     :initform :true
	     :metadata (:sync t
			      :json-name "readout"
			      :help "Dispaly the current value of the slider next to it."))
   (readout_format :initarg :readout_format :accessor readout_format
		    :type unicode
		    :initform (unicode "d")
		    :metadata (:sync t
				     :json-name "readout_format"
				     :help "Format for the readout.")) 
   (continuous_update :initarg :continuous_update :accessor continuous_update
		       :type bool
		       :initform :true
		       :metadata (:sync t
					:json-name "continuous_update"
					:help "Update the value of the widget as the user is holding the slider."))
   (style :accessor style
	  :initform (make-instance 'slider-style)
	  :metadata (:sync t
			   :json-name "style"
			   :from-json json-to-widget
			   :to-json widget-to-json
   )))
  (:default-initargs
   :view-name (unicode "IntSliderView")
    :model-name (unicode "IntSliderModel"))
  (:metaclass traitlets:traitlet-class))

(defclass progress-style (style core-widget)
  ((bar_color :initarg :bar_color :accessor bar_color
	     :type unicode
	     :initform (unicode "")
	     :metadata (:sync t
			      :json-name "bar_color")))
  (:default-initargs
   :model-name (unicode "ProgressStyleModel"))
  (:metaclass traitlets:traitlet-class))

(defclass int-progress (%bounded-int)
  ((orientation :initarg :orientation :accessor orientation
		 :type unicode
		 :initform (unicode "horizontal")
		 :metadata (:sync t
				  :json-name "orientation"
				  :help "vertical or horizontal."))
   (bar_style :initarg :bar_style :accessor bar_style
	       :type unicode
	       :initform (unicode "")
	       :metadata (:sync t
				:json-name "bar_style"
				:help "Use a predefined styling for the progress bar. Options: \"success\", \"info\", \"warning\", and \"danger\". Default: \"\"."))
    (style :accessor style
	  :initform (make-instance 'slider-style)
	  :metadata (:sync t
			   :json-name "style"
			   :from-json json-to-widget
			   :to-json widget-to-json
   )))
  (:default-initargs
   :view-name (unicode "ProgressView")
    :model-name (unicode "ProgressModel"))
  (:metaclass traitlets:traitlet-class))


(defclass int-range(%int)
  ((value :initarg :value :accessor value
	  :type tuple
	  :initform (tuple 0 1)
	  :metadata (:sync t
			   :json-name "value"
			   :help "Tuple of (lower, upper) bounds"))
   )
  (:metaclass traitlets:traitlet-class))

(defclass bounded-int-range(int-range)
  ((step :initarg :step :accessor step
	 :type integer
	 :initform 1
	 :metadata (:sync t
			  :json-name "step"
			  :help "Minimum step that the value can take (ignored by some views)"))
   (value :validator validate-range)
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

(defun validate-range (object val)
  (let ((min (min object)) (max (max object)))
    (cond ((< val min) min)
	  ((> val max) max)
	  (t val))))


(defclass int-range-slider(bounded-int-range)
  ((orientation :initarg :orientation :accessor orientation
		:type unicode
		:initform (unicode "horizontal")
		:metadata (:sync t
				 :json-name "orientation"
				 :help "Vertical or horizontal"))
   (_range :initarg :range :accessor range
	   :type bool
	   :initform :true
	   :metadata (:sync t
			    :json-name "_range"
			    :help "Display a range selector"))
   (readout :initarg :readout :accessor readout
	    :type bool
	    :initform :true
	    :metadata (:sync t
			     :json-name "readout"
			     :help "Display the current value of the slider next to it"))
   (readout_format :initarg :readout_format :accessor readout_format
		   :type unicode
		   :initform (unicode "i")
		   :metadata (:sync t
				    :json-name "readout_format"))
   (continuous_update :initarg :continuous_update :accessor continuous_update
		      :type bool
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
	     :type bool
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
    


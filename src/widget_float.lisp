(in-package :cl-jupyter-widgets)

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_float.py#L19
(defclass %float (labeled-widget value-widget core-widget)
  ((value :initarg :value :accessor value
	  :validator validate-range
	   :type float
	   :initform 0.0
	   :metadata (:sync t
			    :json-name "value"
			    :help "Float value"))
   (disabled :initarg :disabled :accessor disabled
	      :type bool
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


;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_float.py#L32
(defclass %bounded-float(%float)
  ((value :validator validate-range)
   (max :initarg :max :accessor max
	 :type float
	 :initform 100.0
	 :metadata (:sync t
			  :json-name "max"
			  :help "Max value"))
   (min :initarg :min :accessor min
	 :type float
	 :initform 0.0
	 :metadata (:sync t
			  :json-name "min"
			  :help "Min value"))
   (step :initarg :step :accessor step
	  :type float
	  :initform 0.1
	  :metadata (:sync t
			   :json-name "step"
			   :help "Minimum step to increment the value (ignored by some views)"))
   )
  (:metaclass traitlets:traitlet-class))

(defun validate-range (object val)
  (if (and (slot-boundp object 'min) (slot-boundp object 'max))
      (let ((min (min object)) (max (max object)))
	(cond ((< val min) min)
	      ((> val max) max)
	      (t val)))
      val))

;(defun validate-range-vector (object val)
  

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_float.py#L67
(defclass float-text(%float)
  ()
  (:default-initargs
   :view-name (unicode "FloatTextView")
    :model-name (unicode "FloatTextModel")
    )
  (:metaclass traitlets:traitlet-class))


;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_float.py#L85
(defclass bounded-float-text(%bounded-float)
  ()
  (:default-initargs
   :view-name (unicode "FloatTextView")
    :model-name (unicode "FloatTextModel"))
  (:metaclass traitlets:traitlet-class))


;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_float.py#L108
(defclass float-slider(%bounded-float)
  ((orientation :initarg :orientation :accessor orientation
		:type unicode
		:initform (unicode "horizontal")
		:metadata (:sync t
				 :json-name "orientation"
				 :help "Vertical or horizontal"))
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
			      :help "Display the current value of the slider next to it"))
   (readout-format :initarg :readout-format :accessor readout-format
		   :type unicode
		   :initform (unicode ".2f")
		   :metadata (:sync t
				    :json-name "readout_format"
				    :help "Format for the readout"))
   (slider-color :initarg :slider-color :accessor slider-color
		 :type unicode
		 :initform (unicode "None")
		 :metadata (:sync t
				  :json-name "slider_color"))
   (continuous-update :initarg :continuous-update :accessor continuous-update
		      :type bool
		       :initform :true
		       :metadata (:sync t
					:json-name "continuous_update"
					:help "Update the value of the widget as the user is holding the slider."))
   )
  (:default-initargs
   :view-name (unicode "FloatSliderView")
    :model-name (unicode "FloatSliderModel"))
  (:metaclass traitlets:traitlet-class))


;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_float.py#L148
(defclass float-progress (%bounded-float)
  ((orientation :initarg :orientation :accessor orientation
		 :type unicode
		 :initform (unicode "horizontal")
		 :metadata (:sync t
				  :json-name "orientation"
				  :help "Vertical or horizontal."))
   (bar-style :initarg :bar-style :accessor bar-style
	       :type unicode
	       :initform (unicode "")
	       :metadata (:sync t
				:json-name "bar_style"
				:help "Use a predefined styling for the progress bar. Options: 'success', 'info', 'warning', 'danger'"))
   )
  (:default-initargs
   :view-name (unicode "ProgressView")
    :model-name (unicode "ProgressModel"))
  (:metaclass traitlets:traitlet-class))


;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_float.py#L180
(defclass %float-range (%float)
  ((value :initarg :value :accessor value
	 ;; :validator validate-range
	   :type vector
	   :initform (vector 0.0 1.0)
	   :metadata (:sync t
			    :json-name "value"
			    :help "Tuple of (lower, upper) bounds"))
   )
  (:metaclass traitlets:traitlet-class))


;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_float.py#L208
(defclass %bounded-float-range(%float-range)
  ((step :initarg :step :accessor step
	  :type float
	  :initform 1.0
	  :metadata (:sync t
			   :json-name "step"
			   :help "Minimum step that the value can take (ignored by some views)"))
   (value :validator validate-range)
   (max :initarg :max :accessor max
	 :type float
	 :initform 100.0
	 :metadata (:sync t
			  :json-name "max"
			  :help "Max value"))
   (min :initarg :min :accessor min
	 :type float
	 :initform 0.0
	 :metadata (:sync t
			  :json-name "min"
			  :help "Min value"))
   )
  (:metaclass traitlets:traitlet-class))

#||(defun validate-range (object val)
  (if (and (slot-boundp object 'min) (slot-boundp object 'max))
      (let ((min (min object)) (max (max object)))
	(cond ((< val min) min)
	      ((> val max) max)
	      (t val)))
      val))||#

;;https://github.com/drmeister/widget-dev/blob/master/ipywidgets6/widgets/widget_float.py#L243
(defclass float-range-slider(%bounded-float-range)
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
   (readout-format :initarg :readout-format :accessor readout-format
		    :type unicode
		    :initform (unicode ".2f")
		    :metadata (:sync t
				     :json-name "readout_format"
				     :help "Format for the readout"))
   (slider-color :initarg :slider-color :accessor slider-color
		  :type unicode
		  :initform (unicode "None")
		  :metadata (:sync t
				   :json-name "slider_color"))
   (continuous-update :initarg :continuous-update :accessor continuous-update
		       :type bool
		       :initform :true
		       :metadata (:sync t
					:json-name "continuous_update"
				        :help "Update the value of the widget as the user is sliding the slider"))
   )
  (:default-initargs
   :view-name (unicode "FloatSliderView")
    :model-name (unicode "FloatSliderModel"))
  (:metaclass traitlets:traitlet-class))


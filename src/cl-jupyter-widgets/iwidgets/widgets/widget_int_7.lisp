(in-package :cl-jupyter-widgets)
;;;Python code: https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets6/widgets/widget_int.py#L69
(defclass %int (description-widget value-widget core-widget)
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
			      :help "Enable or disable user changes")))
  (:metaclass traitlets:traitlet-class))

(defclass %bounded-int (%int)
  ((value :validator validate-bounded-int)
   (max :initarg :max :accessor max
	:type integer
	:initform 100
	:validator validate-int-max
	:metadata (:sync t
			 :json-name "max"
			 :help "Max value."))
   (min :initarg :min :accessor min
	:type integer
	:initform 0
	:validator validate-int-min
	:metadata (:sync t
			 :json-name "min"
			 :help "Min value."))
   )
  (:metaclass traitlets:traitlet-class))

(defclass-widget-register int-text (%int)
  ((%continuous-update :accessor continuous-update
		       :type bool
		       :initform :false
		       :metadata (:sync t
                                        :json-name "continuous_update"
                                  :help "Update the value as the user types. If False, update on submission, e.g., pressing Enter or navigating away."))
   (%step :accessor step
	  :type int
	  :initform 1
	  :metadata (:sync t
                     :json-name "step"
                     :help "Minimum step to increment the value")))
  (:default-initargs
   :view-name (unicode "IntTextView")
    :model-name (unicode "IntTextModel")
   )
  (:metaclass traitlets:traitlet-class))
       
(defclass-widget-register bounded-int-text (%bounded-int)
  ((%continuous-update :accessor continuous-update
		       :initarg :continuous-update
		       :type bool
		       :initform :false
		       :metadata (:sync t
                                  :json-name "continuous_update"
                                  :help "Update the value as the user types. If False, update on submission, e.g., pressing Enter or navigating away."))
   (%step :accessor step
	  :type int
	  :initform 1
	  :metadata (:sync t
                     :json-name "step"
                     :help "Minimum step to increment the value")))
  (:default-initargs
      :view-name (unicode "IntTextView")
    :model-name (unicode "BoundedIntTextModel"))
  (:metaclass traitlets:traitlet-class))

(defclass-widget-register slider-style (style core-widget)
  ((handle_color :initarg :handle_color :accessor handle_color
		:type unicode
		:initform (unicode "")
		:metadata (:sync t
				 :json-name "handle_color"
				 :help "Color of the slider handle.")))
  (:default-initargs
   :model-name (unicode "SliderStyleModel"))
  (:metaclass traitlets:traitlet-class))

(defclass-widget-register int-slider (%bounded-int)
  ((%step :initarg :step :accessor step
          :type integer
          :initform 1
          :metadata (:sync t
                     :json-name "step"
                     :help "Minimum step to increment the value (ignored by some views)."))
   (orientation :initarg :orientation :accessor orientation
                :type unicode
                :initform "horizontal"
                :metadata (:sync t
                           :json-name "orientation"
                           :help "vertical or horizontal"))
   (readout :initarg :readout :accessor readout
	    :type bool
            :initform :true
            :metadata (:sync t
                       :json-name "readout"
                       :help "Display the current value of the slider next to it."))
   (readout_format :initarg :readout_format :accessor readout_format
                   :type unicode
                   :initform (number-format :format "d")
                   :metadata (:sync t
                              :json-name "readout_format"
                              :help "Format for the readout.")) 
   (continuous_update :initarg :continuous_update :accessor continuous_update
                      :type bool
                      :initform :true
                      :metadata (:sync t
                                 :json-name "continuous_update"
                                 :help "Update the value of the widget as the user is holding the slider."))
   #|(style :accessor style :initform (make-instance 'slider-style)
	  :metadata (:sync t
			   :json-name "style"
			   ,@*widget-serialization*))|#
   )
  (:default-initargs
   :view-name (unicode "IntSliderView")
   :model-name (unicode "IntSliderModel"))
  (:metaclass traitlets:traitlet-class))

(defclass-widget-register progress-style (description-style core-widget)
  ((bar_color :initarg :bar_color :accessor bar_color
	     :type unicode
	     :initform (unicode "")
	     :metadata (:sync t
			      :json-name "bar_color")))
  (:default-initargs
   :model-name (unicode "ProgressStyleModel"))
  (:metaclass traitlets:traitlet-class))

(defclass-widget-register int-progress (%bounded-int)
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
   )
  (:default-initargs
   :view-name (unicode "ProgressView")
   :model-name (unicode "IntProgressModel"))
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


(defclass-widget-register play (%bounded-int)
  ((interval :accessor interval
	     :type int
	     :initform 100
	     :metadata (:sync t
			      :json-name "interval"
			      :help "The maximum value for the play control."))
   (%step :initarg :step :accessor step
          :type integer
          :initform 1
          :metadata (:sync t
                     :json-name "step"
                     :help "Minimum step to increment the value (ignored by some views)."))
   (%playing :accessor playing
	     :type bool
	     :initform :true
	     :metadata (:sync t
			      :json-name "_playing"
			      :help "Whether the control is currently playing."))
   (%repeat :accessor repeat
	    :type bool
	    :initform :true
	    :metadata (:sync t
			     :json-name "_repeat"
			     :help "Whether the control will repeat in a continuous loop."))
   (%show-repeat :accessor show-repeat
		 :type bool
		 :initform :true
		 :metadata (:sync t
				  :json-name "show_repeat"
				  :help "Show the repeat toggle button in the widget.")))
  (:default-initargs
      :view-name (unicode "PlayView")
    :model-name (unicode "PlayModel"))
  (:metaclass traitlets:traitlet-class))

(defclass %bounded-int-range(int-range)
  ((value :validator validate-bounded-int-range)
   (max :initarg :max :accessor max
	:type integer
	:initform 100
	:validator validate-int-max
	:metadata (:sync t
			 :json-name "max"
			 :help "Max value"))
   (min :initarg :min :accessor min
	:type integer
	:initform 0
	:validator validate-int-min
	:metadata (:sync t
			 :json-name "min"
			 :help "Min value"))
   )
  (:metaclass traitlets:traitlet-class))


(defclass-widget-register int-range-slider(%bounded-int-range)
  ((%step :initarg :step :accessor step
          :type integer
          :initform 1
          :metadata (:sync t
                     :json-name "step"
                     :help "Minimum step to increment the value (ignored by some views)."))
   (orientation :initarg :orientation :accessor orientation
		:type unicode
		:initform (unicode "horizontal")
		:metadata (:sync t
				 :json-name "orientation"
				 :help "Vertical or horizontal"))
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
   :view-name (unicode "IntRangeSliderView")
    :model-name (unicode "IntRangeSliderModel"))
  (:metaclass traitlets:traitlet-class))

(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))


(defun get-keys (object)
  (loop for slot-def in (clos:class-slots (class-of object))
     when (eq (clos:slot-definition-allocation slot-def) :instance)
     when (getf (traitlets::metadata slot-def) :sync)
     collect (clos:slot-definition-name slot-def)))
    
;;;Validator for changing value in a bounded int (make sure it's > min and < max.
;;;@validate('value')
(defun validate-bounded-int (object val)
  (if (and (slot-boundp object 'min) (slot-boundp object 'max))
      (let ((min (min object)) (max (max object)))
	(cond ((< val min) min)
	      ((> val max) max)
	      (t val)))
      val))

;;;Validator for changing a min in a bounded int (make sure it's not above max)
;;;If new min is above current value, set value to min.
;;;@validate('int-min')
(defun validate-int-min (object val)
  (if (slot-boundp object 'min)
      (with-slots ((max max) (value value)) object
	(cond ((> val  max) max)
	      ((> val value) (setf value val))
	      (t val)))
      val))

;;;Validator for changing a max in a bounded int (make sure it's not below min)
;;;If new max is below current val, set value to max.
;;;@validate('int-max')
(defun validate-int-max (object val)
  (if (slot-boundp object 'max)
      (with-slots ((min min) (value value)) object
	(cond ((> value  val) (setf value val))
	      ((< val min) min)
	      (t val)))
      val))

;;;Validator for the range of a bounded int
;;;Makes sure the first value is lower than the second value
;;;@validate('value')
(defun validate-bounded-int-range (object val)
  (flet ((enforce-min (val min) (if (< val min) min val))
	 (enforce-max (val max) (if (> val max) max val)))
	  (let ((low-val (cl:min (elt val 0) (elt val 1)))
		(high-val (cl:max (elt val 0) (elt val 1))))
	    ;; Now low-val <= high-val
	    (if (and (slot-boundp object 'min) (slot-boundp object 'max))
		(let ((min (min object))
		      (max (max object)))
		  (let ((low-val-min (enforce-min low-val min))
			(high-val-min (enforce-min high-val min)))
		    (let ((low-val-min-max (enforce-max low-val-min max))
			  (high-val-min-max (enforce-max high-val-min max)))
		      (vector low-val-min-max high-val-min-max))))
		(vector low-val high-val)))))
  

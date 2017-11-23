
(in-package :cl-jupyter-widgets)
;;;Python code: https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets6/widgets/widget_selection.py#L49



(defclass %selection (description-widget value-widget core-widget)
  ((value :initarg :value :accessor value
	  :initform nil
	  :validator validate-selection
	  :observers (update-value))
   (label :initarg :label :accessor label
	  :type unicode
	  :initform (unicode "")
	  :validator validate-label)
   (index :initarg :index :accessor index
	  :type integer
	  :initform 0
	  :observers (update-index)
	 ; :validator validate-index
	  :metadata (:sync t
			   :json-name "index"
			   :help "Selected index"))
   (options :initarg :options :accessor options
	     :type list
	     :initform ()
	     :observers (update-options)
	     :documentation "Iterable of values, (label, value) pairs, or a mapping of {label: value} pairs that the user can select. The labels are the strings that will be displayed in the UI, representing the actual Python choices, and should be unique.")
   (disabled :initarg :disabled :accessor disabled
	      :type bool
	      :initform :false
	      :metadata (:sync t
			       :json-name "disabled"
			       :help "Enable or disable user changes."))
   (options-full :initarg :options-full :accessor options-full
		  :initform nil)
   (options-labels :initarg :options-labels :accessor options-labels
		    :initform (make-array 0 :adjustable t :fill-pointer 0)
		    :type vector
		    :metadata (:sync t
				     :json-name "_options_labels"
				     :help "The labels for the options."))
   (options-values :accessor options-values
		    :initform  (make-array 0 :adjustable t :fill-pointer 0)
		    :type vector))
  (:metaclass traitlets:traitlet-class))

(defclass multiple-selection (description-widget value-widget core-widget)
  ((value :initarg :value :accessor value
	  :type vector
	  :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (label :initarg :label :accessor label
	  :type vector
	  :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (index :initarg :index :accessor label
	  :type vector
	  :initform (make-array 0 :fill-pointer 0 :adjustable t)
	  :validator validate-multiple-indexes
	  :metadata (:sync t
                     :json-name "index"))
   (options :initarg :options :accessor options
	    :initform nil
	    :documentation "Iterable of values, (label, value) pairs, or a mpaaing of {label: value} pairs that the user can select. The labels are the strings that will be displayed in the UI, representing the actual Python choices, and should be unique.")
   (%options-full :initarg :options-full :accessor options-full
		  :initform nil)
   (%options-labels :initarg :options-labels :accessor options-labels
		    :initform nil
		    :metadata (:sync t
                               :json-name "options_labels"))
   (disabled :initarg :disabled :accessor disabled
	     :type bool
	     :initform :false
	     :metadata (:sync t
                        :json-name "disabled"
                        :help "Enable or disable user changes.")))
  (:metaclass traitlets:traitlet-class))

(defclass-widget-register toggle-buttons-style (style core-widget)
  ((button-width :initarg :button-width :accessor button-width
		:type unicode
		:initform (unicode "")
		:metadata (:sync t
				 :json-name "button_width"
				 :help "The width of each button.")))
  (:default-initargs
   :model-name (unicode "ToggleButtonsStyleModel"))
  (:metaclass traitlets:traitlet-class))

(defclass-widget-register toggle-buttons (%selection)
  ((tooltips :initarg :tooltips :accessor tooltips
	     :type unicode
	     :initform (unicode)
	     :metadata (:sync t
			      :json-name "tooltips"
			      ))
   (icons :initarg :icons :accessor icons
	  :type unicode
	  :initform (unicode)
	  :metadata (:sync t
			   :json-name "icons"
			   :help "Font-awesome icon. Styles we know about include: check, bullseye, camera, refresh, eye-slash, trash, eraser. Syntax: :icons (vector \"refresh\" \"check\" \"camera\"). This would give you three icons if you have three buttons."))
  #| (style :initarg :style :accessor style
	  :initform (make-instance 'instance-dict :instance (make-instance 'toggle-buttons-style))
	  :metadata (:sync t
			   :to-json json-to-widget
			   :from-json widget-to-json))|#
   (button_style :initarg :button_style :accessor button_style
		 :type unicode
		 :initform (unicode)
		 :metadata (:sync t
				  :json-name "button_style"
				  :help "Use a predefined styling for the buttons. Options include: \"primary\", \"success\", \"info\", \"warning\", \"danger\", \"\"\"\".")))
  (:default-initargs
   :view-name (unicode "ToggleButtonsView")
    :model-name (unicode "ToggleButtonsModel"))
  (:metaclass traitlets:traitlet-class))

(defclass-widget-register dropdown (%selection)
  ()
  (:default-initargs
   :view-name (unicode "DropdownView")
    :model-name (unicode "DropdownModel"))
  (:metaclass traitlets::traitlet-class))

(defclass-widget-register radio-buttons (%selection)
  ()
  (:default-initargs
   :view-name (unicode "RadioButtonsView")
    :model-name (unicode "RadioButtonsModel"))
  (:metaclass traitlets:traitlet-class))

(defclass-widget-register select (%selection)
  ()
  (:default-initargs
   :view-name (unicode "SelectView")
    :model-name (unicode "SelectModel"))
  (:metaclass traitlets:traitlet-class))



(defclass-widget-register select-multiple (multiple-selection)
  ((rows :initarg :rows :accessor rows
	 :type integer
	 :initform 5
	 :metadata (:sync t
                    :json-name "rows")))
  (:default-initargs
   :view-name (unicode "SelectMultipleView")
   :model-name (unicode "SelectMultipleModel"))
  (:metaclass traitlets:traitlet-class))

(defclass %selection-nonempty (%selection)
  ())

(defclass %multiple-selection-nonempty (multiple-selection)
  ())

;;;FIXME: Do we need those? They seem meaningless.

(defclass-widget-register selection-slider (%selection)
  ((orientation :initarg :orientation :accessor orientation
		 :type unicode
		 :initform (unicode "horizontal")
		 :metadata (:sync t
				  :json-name "orientation"
				  :help "vertical or horizontal"))
   (readout :initarg :readout :accessor readout
	     :type bool
	     :initform :true
	     :metadata (:sync t
			      :json-name "readout"
			      :help "Display the current selected label next to the slider."))
   (continuous_update :initarg :continuous_update :accessor continuous_update
		       :type bool
		       :initform :true
		       :metadata (:sync t
					:json-name "continuous_update"
					:help "Update the value of the widget as the user is holding the slider.")))
  (:default-initargs
   :view-name (unicode "SelectionSliderView")
    :model-name (unicode "SelectionSliderModel"))
  (:metaclass traitlets:traitlet-class))

(defclass-widget-register selection-range-slider (%multiple-selection-nonempty)
  ((value :initarg :value :accessor value
	  :type vector                  ;?
	  :initform nil
	  :documentation "Min and max selected values")
   (label :initarg :label :accessor label
	  :type vector                  ;also ?
	  :initform nil
	  :documentation "Min and max selected labels")
   (index :initarg :index :accessor index
	  :type vector                  ;?
	  :initform nil
	  :validator validate-range-index
	  :metadata (:sync t
                     :json-name "index"
                     :help "Min and max selected indices."))
   (orientation :initarg :orientation :accessor orientation
		:type unicode
		:initform (unicode "horizontal")
		:metadata (:sync t
                           :json-name "orientation"
                           :help "Options are \"horizontal\" or \"vertical\"."))
   (readout :initarg :readout :accessor readout
	    :type bool
	    :initform :true
	    :metadata (:sync t
                       :json-name "readout"
                       :help "Display the current selected label next to the slider."))
   (continuous-update :initarg :continuous-update :accessor continuous-update
		      :type bool
		      :initform :true
		      :metadata (:sync t
				      :json-name "continuous_update"
				      :help "Update the value of the widget as the user is holding the slider."))
   )
  (:default-initargs
      :view-name (unicode "SelectionRangeSliderView")
    :model-name (unicode "SelectionRangeSliderModel"))
    (:metaclass traitlets:traitlet-class))

;FIXME: Get some validators in this jawn!

#|
def _value_to_label(value, obj):
    """Convert a value to a label, given a _Selection object.

    Raises a KeyError if the value is not found."""
    # We can't rely on _options_labels and _options_values since we
    # might be called before the options are validated and those are filled.
    # TODO: make a separate validation function so this doesn't have
    # to redo the work of parsing the options object.
    options = obj._make_options(obj.options)
    if len(obj.options) == 0 and value is None:
        return ''
    else:
        try:
            # return the first label whose value is equal to the desired value
            return next(l for (l, v) in options if obj.equals(v, value))
        except StopIteration:
            raise KeyError(value)

def _label_to_value(label, obj):
    """Convert a label to a value, given a _Selection object."""
    if len(obj._options_dict) == 0 and label == '':
        return None
    else:
        return obj._options_dict[label]

|#
#|
(defun %label-to-value (k object)
      (gethash 'k (slot-value object '_options_dict)))


#|
def _values_to_labels(values, obj):
    "Convert values to labels from a _MultipleSelection object"
    return tuple(_value_to_label(v, obj) for v in values)

def _labels_to_values(k, obj):
    "Convert labels to values from a _MultipleSelection object"
    return tuple(_label_to_value(l, obj) for l in k)
|#

 (defun %labels-to-values (k obj)
   (loop for o across k collect (%label-to-value o obj)))

(defun %value-to-label (value obj)
	(car (rassoc value (options obj) :test #'equal)))

;;; This implements
;;; https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget_selection.py#L106
(defun %values-to-labels (values obj)
  (map 'vector (lambda (v) (%value-to-label v obj)) values))

(defmethod initialize-instance :after ((%selection %selection) &key)
  (with-slots (value _options_labels _options_values _options_dict options) %selection
    (loop for (k . v) in options
	 do (setf (gethash k _options_dict) v))
    (setf _options_labels (map 'vector #'car options))
    (setf _options_values (map 'vector #'cdr options))
    (if (not value)
	(setf value (aref _options_values 0))))) 
(defmethod initialize-instance :after ((%selection select-multiple) &key)
  (with-slots (value _options_values) %selection
    (if (zerop (length value))
	(vector-push-extend (aref _options_values 0) value))))

|#
(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))

(defmethod initialize-instance :after ((self %selection) &key)
  (with-slots (options-labels options-full options options-values value index) self
    (setf options-full options)
    (loop for (k . v) in options
       do
	  (vector-push-extend k options-labels) 
	  (vector-push-extend v options-values))
    (when (zerop (length value))
      (setf value (aref options-values 0)))
    (setf index (position value options :key #'cdr :test #'equal))))



;;;FIXME: Make validators -_- ....and propagators?

(defun validate-selection (object val)
  (if (slot-boundp object 'options) 
      (let ((valid (member val (options object) :test #'string= :key #'car)))
	(if valid
	    val
	    (error "New value for ~a is invalid: ~a" object val)))
	val))
#|
(defun validate-index (object val)
  (if (slot-boundp object 'value)
      (let ((valid (length (options object))))
	(if (<= val valid)
	    val
	    (error "New value for ~a is invalid: ~a" object val))
	val)))
|#
(defun validate-label (object val)
  (if (slot-boundp object 'label)
      (let ((valid (assoc val (label object) :test #'string=)))
	(if valid
	    val
	    (error "New value for ~a is invalid: ~a" object val)))
	val))

(defun validate-range-index (object val)
  (if (slot-boundp object 'value)
      (let ((valid (length (value object))))
	(unless (= (length val) (valid))
	    (error "Invalid Selection: index must have two values, but has ~a" (length val)))
	(loop for i in val
	   do
	     (when (or (> i valid) (< i 0))
	       (error "Invalid Selection: index is out of range. Please select an index in range of options."))
	))
      val))

(defun validate-multiple-indexes (object val)
  (if (slot-boundp object 'options)
      (let ((valid (length (options object))))
	(loop for i in val
	   do
	     (when (or (> i (length (options-labels object)) (< i 0)))
	       (error "Invalid Selection: index is out of range. Please select an index in range of options."))))
      val))


(defun update-index (object name new old)
  (when (slot-boundp (index object))
    (unless (equal new old)
      (setf (label object) (aref (options-labels object) new)
	    (value object) (aref (options-values object) new))))
      new)

(defun update-options (object name new old)
  (values))

(defun update-value (object name new old)
  (setf (index object) (position (value object) (options object) :key #'cdr :test #'equal)
	(label object) (aref (options-values object) (index object)))
  new)

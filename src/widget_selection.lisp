
(in-package :cl-jupyter-widgets)


(defclass %selection (labeled-widget value-widget core-widget)
    ((value :initarg :value :accessor value
	  :initform nil
	  :metadata (:sync t
			   :json-name "value"
			   :help "Selected value"
			   :to-json %value-to-label
			   :from-json %label-to-value
			   ))
   (options :initarg :options :accessor options
	     :type list
	     :initform ())
   (disabled :initarg :disabled :accessor disabled
	      :type boolean
	      :initform :false
	      :metadata (:sync t
			       :json-name "disabled"
			       :help "Enable or disable user changes."))
   (_options_dict :accessor _options_dict
		  :type hash-table
		  :initform (make-hash-table))
   (_options_labels :accessor options_labels
		    :type vector
		    :metadata (:sync t
				     :json-name "_options_labels"
				     :help "Who knows."))
   (_options_values  :accessor options_values
		    :type vector))
  (:default-initargs
    :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets"))
  (:metaclass traitlets:traitlet-class))

(defclass multiple-selection (%selection)
    ((value :type vector :initform (make-array 0 :fill-pointer 0 :adjustable t)
	    :metadata (:help "Selected values"
		     :to-json %values-to-labels
		     :from-json %labels-to-values)))
  (:metaclass traitlets:traitlet-class))

(defclass toggle-buttons (%selection)
  ((tooltips :initarg :tooltips :accessor tooltips
	     :type unicode
	     :initform (unicode "")
	     :metadata (:sync t
			      :json-name "tooltips"
			      ))
   (icons :initarg :icons :accessor icons
	  :type unicode
	  :initform (unicode "")
	  :metadata (:sync t
			   :json-name "icons"))
   (button_style :initarg :button_style :accessor button_style
		 :type unicode
		 :initform (unicode "")
		 :metadata (:sync t
				  :json-name "button_style"
				  :help "Use a predefined styling for the buttons. Options include: \"primary\", \"success\", \"info\", \"warning\", \"danger\", \"""\".")))
  (:default-initargs
   :view-name (unicode "ToggleButtonsView")
    :model-name (unicode "ToggleButtonsModel"))
  (:metaclass traitlets:traitlet-class))

(defclass dropdown (%selection)
  ()
  (:default-initargs
   :view-name (unicode "DropdownView")
    :model-name (unicode "DropdownModel"))
  (:metaclass traitlets::traitlet-class))

(defclass radio-buttons (%selection)
  ()
  (:default-initargs
   :view-name (unicode "RadioButtonsView")
    :model-name (unicode "RadioButtonsModel"))
  (:metaclass traitlets:traitlet-class))

(defclass select (%selection)
  ()
  (:default-initargs
   :view-name (unicode "SelectView")
    :model-name (unicode "SelectModel"))
  (:metaclass traitlets:traitlet-class))

(defclass selection-slider (%selection)
  ((orientation :initarg :orientation :accessor orientation
		 :type unicode
		 :initform (unicode "horizontal")
		 :metadata (:sync t
				  :json-name "orientation"
				  :help "vertical or horizontal"))
   (readout :initarg :readout :accessor readout
	     :type boolean
	     :initform :true
	     :metadata (:sync t
			      :json-name "readout"
			      :help "Display the current selected label next to the slider."))
   (continuous_update :initarg :continuous_update :accessor continuous_update
		       :type boolean
		       :initform :true
		       :metadata (:sync t
					:json-name "continuous_update"
					:help "Update the value of the widget as the user is holding the slider.")))
  (:default-initargs
   :view-name (unicode "SelectionSliderView")
    :model-name (unicode "SelectionSliderModel"))
  (:metaclass traitlets:traitlet-class))

(defclass select-multiple (multiple-selection)
  ()
  (:default-initargs
   :view-name (unicode "SelectMultipleView")
    :model-name (unicode "SelectMultipleModel"))
  (:metaclass traitlets:traitlet-class))

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


(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))

#|
(in-package :cl-jupyter-widgets)


(defclass %selection (dom-widget)
    ((value :initarg :value :accessor value
	  :initform nil
	  :metadata (:sync t
			   :json-name "value"
			   :help "Selected value"
			   :to-json %value-to-label
			   :from-json %label-to-value
			   ))
   (options :initarg :options :accessor options
	     :type list
	     :initform ())
   (disabled :initarg :disabled :accessor disabled
	      :type boolean
	      :initform :false
	      :metadata (:sync t
			       :json-name "disabled"
			       :help "Enable or disable user changes."))
   (description :initarg :description :accessor description
		 :type unicode
		 :initform (unicode "")
		 :metadata (:sync t
				  :json-name "description"
				  :help "Description of the value this widget represents."))
   (_options_dict :accessor _options_dict
		  :type hash-table
		  :initform (make-hash-table))
   (_options_labels :accessor options_labels
		    :type vector
		    :metadata (:sync t
				     :json-name "_options_labels"
				     :help "Who knows."))
   (_options_values  :accessor options_values
		    :type vector))
  (:default-initargs
    :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets"))
  (:metaclass traitlets:traitlet-class))


(defclass toggle-buttons (%selection)
  ((tooltips :initarg :tooltips :accessor tooltips
	     :type unicode
	     :initform (unicode "")
	     :metadata (:sync t
			      :json-name "tooltips"
			      ))
   (icons :initarg :icons :accessor icons
	  :type unicode
	  :initform (unicode "")
	  :metadata (:sync t
			   :json-name "icons"))
   (button_style :initarg :button_style :accessor button_style
		 :type unicode
		 :initform (unicode "")
		 :metadata (:sync t
				  :json-name "button_style"
				  :help "Use a predefined styling for the buttons. Options include: \"primary\", \"success\", \"info\", \"warning\", \"danger\", \"""\".")))
  (:default-initargs
   :view-name (unicode "ToggleButtonsView")
    :model-name (unicode "ToggleButtonsModel"))
  (:metaclass traitlets:traitlet-class))

(defclass multiple-selection (%selection)
    ((value :type vector :initform (make-array 0 :fill-pointer 0 :adjustable t)
	    :metadata (:help "Selected values"
		     :to-json %values-to-labels
		     :from-json %labels-to-values)))
  (:metaclass traitlets:traitlet-class))

(defclass dropdown (%selection)
  ((button_style :initarg :button_style :accessor button_style
		  :type unicode
		  :initform (unicode "")
		  :metadata (:sync t
				   :json-name "button_style"
				   :help "Use a predefined styling for the buttons.Options include:\"primary\", \"success\", \"info\", \"warning\", and \"danger\".")))
  (:default-initargs
   :view-name (unicode "DropdownView")
    :model-name (unicode "DropdownModel"))
  (:metaclass traitlets::traitlet-class))

(defclass radio-buttons (%selection)
  ()
  (:default-initargs
   :view-name (unicode "RadioButtonsView")
    :model-name (unicode "RadioButtonsModel"))
  (:metaclass traitlets:traitlet-class))

(defclass select (%selection)
  ()
  (:default-initargs
   :view-name (unicode "SelectView")
    :model-name (unicode "SelectModel"))
  (:metaclass traitlets:traitlet-class))

(defclass selection-slider (%selection)
  ((orientation :initarg :orientation :accessor orientation
		 :type unicode
		 :initform (unicode "horizontal")
		 :metadata (:sync t
				  :json-name "orientation"
				  :help "vertical or horizontal"))
   (readout :initarg :readout :accessor readout
	     :type boolean
	     :initform :true
	     :metadata (:sync t
			      :json-name "readout"
			      :help "Display the current selected label next to the slider."))
   (continuous_update :initarg :continuous_update :accessor continuous_update
		       :type boolean
		       :initform :true
		       :metadata (:sync t
					:json-name "continuous_update"
					:help "Update the value of the widget as the user is holding the slider.")))
  (:default-initargs
   :view-name (unicode "SelectionSliderView")
    :model-name (unicode "SelectionSliderModel"))
  (:metaclass traitlets:traitlet-class))

(defclass select-multiple (multiple-selection)
  ()
  (:default-initargs
   :view-name (unicode "SelectMultipleView")
    :model-name (unicode "SelectMultipleModel"))
  (:metaclass traitlets:traitlet-class))

(defun %label-to-value (k object)
      (gethash 'k (slot-value object '_options_dict)))

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


(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))
|#

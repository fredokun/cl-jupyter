(in-package :cl-jupyter-widgets)


(defclass %selection (dom-widget)
  ((value :initarg :value :accessor value
	  :initform nil
	  :metadata (:sync t
			   :json-name "value"
			   :help "Selected value"
			   :to_json %value-to-label
			   :from_json %label-to-value
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
  ((value :initarg :value :accessor value
	  :type vector
	  :initform ()
	  :metadata (:sync t
			   :json-name "value"
			   :help "Selected values"
			   :to_json %values-to-labels
			   :from_json %labels-to-values
			   )))
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
   (loop for v in k collect (%label-to-value v obj)))

(defun %value-to-label (value obj)
    (car (rassoc value (options obj) :test #'equal)))

;;; This implements
;;; https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget_selection.py#L106
(defun %values-to-labels (values obj)
  (loop for v in values collect (%value-to-label v obj)))

(defmethod initialize-instance :after ((%selection %selection) &key)
  (with-slots (value _options_labels _options_values _options_dict options) %selection
    (loop for (k . v) in options
	 do (setf (gethash k _options_dict) v))
    (setf _options_labels (map 'vector #'car options))
    (setf _options_values (map 'vector #'cdr options))
    (setf value (car (rassoc value options :test #'equal)))))

(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))

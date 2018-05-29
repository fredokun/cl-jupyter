
(in-package :cl-jupyter-widgets)
;;;Python code: https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets6/widgets/widget_selection.py#L49



(defclass %selection (description-widget value-widget core-widget)
  ((value :initarg :value :accessor value
	  :initform nil
	  :validator validate-value
	  :observers (update-value))
   (label :initarg :label :accessor label
	  :type unicode
	  :initform (unicode "")
	  :validator validate-label
          :observers (update-label))
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
            :validator validate-options
            :observers (update-options)
            :documentation "Iterable of values, (label, value) pairs, or a mapping of {label: value} pairs that the user can select. The labels are the strings that will be displayed in the UI, representing the actual Python choices, and should be unique.")
   (disabled :initarg :disabled :accessor disabled
             :type bool
             :initform :false
             :metadata (:sync t
                        :json-name "disabled"
                        :help "Enable or disable user changes."))
   (options-full  :accessor options-full
                 :initform nil)
   ;; This being read-only means that it cannot be changed by the user
   (options-labels  :accessor options-labels
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

(defclass-widget-register toggle-buttons-style (description-style core-widget)
  ((button-width :initarg :button-width :accessor button-width
                 :type unicode
                 :initform (unicode "")
                 :metadata (:sync t
                            :json-name "button_width"
                            :help "The width of each button."))
   (font-weight :initarg :button-width :accessor font-weight
		:type unicode
		:initform (unicode "")
		:metadata (:sync t
                           :json-name "button_width"
                           :help "Text font weight of each button.")))
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
	  :type vector                 
	  :initform nil
	  :documentation "Min and max selected values")
   (label :initarg :label :accessor label
	  :type vector                  
	  :initform nil
	  :documentation "Min and max selected labels")
   (index :initarg :index :accessor index
	  :type vector                 
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


(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))

(defmethod initialize-instance :after ((self %selection) &key)
  (with-slots (options-labels options-full options options-values value index) self
    (setf options-full options)
    (loop for (k . v) in options
       do
	  (vector-push-extend k options-labels) 
	 (vector-push-extend v options-values))
    ;;;If no initial value is supplied, choose the first option by default
    (when (zerop (length value))
      (setf value (aref options-values 0)))
    (setf index (position value options :key #'cdr :test #'equal))))


;;;TODO: Finish validate-options
;;;@validate('options')
(defun validate-options (object val)
  (if (slot-boundp object 'options)
      val
      val))



;;;TODO: Finish options updater.
;;;@observe('options')
(defun update-options (object name new old)
  new)


;;;@validate('index')
#+(or)
(defmethod validate-index (object val)
  (if (slot-boundp object 'value)
      (let ((valid (length (options object))))
	(if (<= val valid)
	    val
	    (error "New value for ~a is invalid: ~a" object val))
	val)))

;;;@observe('index')
(defun update-index (object name new old)
  (when (slot-boundp object 'index)
    (unless (equal new old)
      (let ((new-label (aref (options-labels object) new)))
	(unless (string= (label object) new-label)
	  (setf (label object) new-label)))
      (let ((new-value (aref (options-values object) new)))
	(unless (string= (value object) new-value)
	  (setf (value object) new-value)))))
  new)

;;;@validate('value')
(defun validate-value (object val)
  (if (slot-boundp object 'options) 
      (let ((valid (member val (options object) :test #'string= :key #'car)))
	(if valid
	    val
	    (error "New value for ~a is invalid: ~a" object val)))
	val))

;;;@observe('value')
(defun update-value (object name new old)
  (when (slot-boundp object 'options)
    (setf (index object) (position new (options object) :key #'cdr :test #'equal)))
  new) 

;;;@validate('label')
(defun validate-label (object val)
  (if (slot-boundp object 'label)
      (let ((valid (find val (options-labels object) :test #'string=)))
	(if valid
	    val
	    (error "New value for ~a is invalid: ~a" object val)))
	val))

;;;@observe('label')
(defun update-label (object name new old)
  (let ((index (position new (options-labels object) :test #'string=)))
    (unless (= (index object) index)
      (setf (index object) index))
    new))

  
;;;validate('index') for ranged selection widgets
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

;;;@validate('index') for multiple-selection widgets
(defun validate-multiple-indexes (object val)
  (if (slot-boundp object 'options)
      (let ((valid (length (options object))))
	(loop for i in val
	   do
	     (when (or (> i (length (options-labels object)) (< i 0)))
	       (error "Invalid Selection: index is out of range. Please select an index in range of options."))))
      val))





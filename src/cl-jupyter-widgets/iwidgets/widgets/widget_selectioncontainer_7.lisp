(in-package :cl-jupyter-widgets)
;;;Python code: https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets6/widgets/widget_selectioncontainer.py#L16

(defclass %selection-container (Box core-widget)
  ((%titles :accessor titles
	    :type list ;Python dict
	    :initform ()
	    :metadata (:sync t
			     :json-name "titles"
			     :help "titles of the pages"))
   (selected_index :initarg :selected_index :accessor selected_index
		   :type integer
		   :initform 0
		   :validator validate-index
		   :metadata (:sync t
				     :json-name "selected_index"
				     :help "The index of the selected page. This is either an integer selecting a particular sub-widget, or None to have no widgets selected.")))
  (:metaclass traitlets:traitlet-class))


(defmethod set-title ((self %selection-container) index title)
  (let* ((index (write-to-string index))
	 (a (assoc index (titles self) :test #'string=)))
    (if a
	(setf (cdr a) title)
	(push (cons index title) (titles self)))))


(defun get-title (self index)
  (setf index (unicode index))
  (with-slots (titles) self
  (if (assoc index titles :test #'equal)
      (getf titles index))
      (values)))
;;;https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget_selectioncontainer.py#L36

(defclass-widget-register accordion (%selection-container);Displays children each on a separate accordion page.
  ()
  (:default-initargs
   :view-name (unicode "AccordionView")
    :model-name (unicode "AccordionModel"))
  (:metaclass traitlets:traitlet-class))

(defclass-widget-register tab (%selection-container);Displays children each on a separate accordion tab.
  ()
  (:default-initargs
   :view-name (unicode "TabView")
    :model-name (unicode "TabModel"))
  (:metaclass traitlets:traitlet-class))

(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))

(defmethod validate-index (object val)
  (if (slot-boundp 'children object)
      (let ((valid (length (children object))))
	(when (and (<= 0 val) (< val valid))
	  val))
      val))

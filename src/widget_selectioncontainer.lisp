(in-package :cl-jupyter-widgets)


(defclass %selection-container (Box core-widget)
  ((_titles :accessor _titles
	    :type list
	    :metadata (:sync t
			     :json-name "_titles"
			     :help "titles of the pages"))
   (selected_index :initarg :selected_index :accessor selected_index
		   :type integer
		   :initform 0
		   :metadata (:sync t
				     :json-name "selected_index"
				     :help "Selection")))
  (:default-initargs
   :model-module (unicode "jupyter-js-widgets")
    :view-module (unicode "jupyter-js-widgets"))
  (:metaclass traitlets:traitlet-class))



(defun set-title (self index title)
  (setf index (unicode index) (getf (_titles self)  index)  title))
;;;self.send_state('_titles')?????
;;;https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget_selectioncontainer.py#L23
;;;I am trying to copy this functionality. I don't know what self.send_stat('_titles') is doing.Does the unicode function work properly with numbers?? Find out when we test it!

(defun get-title (self index)
  (setf index (unicode index))
  (with-slots (_titles) self
  (if (assoc index _titles :test #'equal)
      (getf _titles index)
      nil)))
;;;https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget_selectioncontainer.py#L36
;;;I want to copy this functionality. I don't know how to make sure index is part of titles.


(defclass accordion (%selection-container);Displays children each on a separate accordion page.
  ()
  (:default-initargs
   :view-name (unicode "AccordionView")
    :model-name (unicode "AccordionModel"))
  (:metaclass traitlets:traitlet-class))

(defclass tab (%selection-container);Displays children each on a separate accordion tab.
  ()
  (:default-initargs
   :view-name (unicode "TabView")
    :model-name (unicode "TabModel"))
  (:metaclass traitlets:traitlet-class))

(defmethod widget-slot-value ((w widget) slot-name)
  (slot-value w slot-name))

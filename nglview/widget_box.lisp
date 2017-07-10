(in-package :nglv)

(defclass BoxNGL (box)
  ((_gui_style :initarg :_gui_style :accessor _gui_style
	       :type unicode
	       :initform (unicode "row")
	       :metadata (:sync t
				:json-name "_gui_style"
				:help "Options: row or column"))
   (_is_beautified :initarg :_is_beautified :accessor _is_beautified
		   :type bool
		   :initform :false))
  (:metaclass traitlets:traitlet-class))

(defmethod box.__init__ ((self BoxNGL) #|uh oh|# &key)
  (setf (layout self) (make_form_item_layout)))

(defmethod _update_gui_style ((self BoxNGL) change)
  (let ((what (aref change "new")))
    (setf (flex_flow (layout self)) (lower what))))

(defmethod _ipython_display_ (self #|uh oh|# &key)
  (_beautify))

(defmethod _update_size (self)
  (loop for widget across (children self)
       (if (type-p widget 'NGLWidget)
	   (_remote_call widget "setSize" :target "widget" :args '("60%" "60%")))))

(defmethod _beautify (self)
  (if not (_is_beautified self)
      (_set_notebook_width js_utils "60%" :left_padding :null);;I DONT KNOW IF NONE IS VALID
      (_update_size self)
      (setf (_is_beautified self) t)))

(defclass DraggableBox(Box)
  ((_dialog :initarg :_dialog :accessor _dialog
	    :type unicode
	    :initform (unicode "off")
	    :metadata (:sync t
			     :json-name "_dialog"))
   (_ngl_command :initarg :_ngl_command :accessor _ngl_command
		 :type unicode
		 :initform (unicode)
		 :metadata (:sync t
				  :json-name "_ngl_command")))
  (:default-initargs
   :view-name (unicode "NGLBox")
   :view-module (unicode "nglview"))
  (:metaclass traitlets:traitlet-class))
	   

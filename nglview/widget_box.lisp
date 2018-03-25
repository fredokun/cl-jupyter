(in-package :nglv)

(defclass BoxNGL (cljw::box)
  ((%gui-style :initarg :%gui-style :accessor %gui-style
	       :type unicode
	       :initform (unicode "row")
	       :observers (%update-gui-style)
	       :metadata (:sync t
				:json-name "_gui_style"
				:help "Options: row or column"))
   (%is-beautified :initarg :%is-beautified :accessor %is-beautified
		   :type bool
		   :initform :false))
  (:metaclass traitlets:traitlet-class))

(defmethod box.--init-- ((self BoxNGL) #|uh oh|# &key)
  (setf (layout self) (make-form-item-layout)))

(defmethod %update-gui-style (object name new old)
  (let ((what new))
    (setf (flex-flow (layout object)) (lower what))))

(defmethod %ipython-display- (self #|uh oh|# &key)
  (%beautify self))

(defmethod %update-size (self)
  (loop for widget across (children self)
       (if (type-p widget 'NGLWidget)
	   (%remote-call widget "setSize" :target "widget" :args '("60%" "60%")))))

(defmethod %beautify (self)
  (if not (%is-beautified self)
      (%set-notebook-width js-utils "60%" :left-padding :null);;I DONT KNOW IF NONE IS VALID
      (%update-size self)
      (setf (%is-beautified self) t)))


(defclass DraggableBox(cljw::box)
  ((%dialog :initarg :%dialog :accessor %dialog
            :type unicode
            :initform (unicode "off")
            :metadata (:sync t
                             :json-name "_dialog"))
   (%ngl-command :initarg :%ngl-command :accessor %ngl-command
                 :type unicode
                 :initform (unicode)
                 :metadata (:sync t
                                  :json-name "_ngl_command")))
  (:default-initargs
   :view-name (unicode "NGLBox")
   :view-module (unicode "nglview"))
  (:metaclass traitlets:traitlet-class))
           

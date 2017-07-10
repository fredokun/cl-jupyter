(in-package :nglv)

(defclass Structure ()
  ((ext :accessor ext :initform "pdb")
   (params :accessor params :initform nil)
   (id :accessor id :initform (format nil "~W" (uuid:make-v4-uuid)))))

(defmethod get_structure_string((Structure Structure))
  (error "Error in get_structure_string: Not Implemented Error!!! Python code not implemented"))

(defclass Trajectory ()
  ((id :accessor id :initform (format nil "~W" (uuid:make-v4-uuid)))
   (shown :accessor shown :type bool :initform :true)))

(defmethod get_coordinates ((Trajectory Trajectory) index)
  (error "Error in get_coordinates: Not Implemented Error!!! Python code not implemented"))

(defmethod n_frames ((Trajectory Trajectory))
  (error "Error in n_frames: Not Implemented Error!!! Python code not implemented"))

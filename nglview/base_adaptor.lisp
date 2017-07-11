(in-package :nglv)

(defclass Structure ()
  ((ext :accessor ext :initform "pdb")
   (params :accessor params :initform nil)
   (id :accessor id :initform (format nil "~W" (uuid:make-v4-uuid)))))

(defmethod get-structure-string((Structure Structure))
  (error "Error in get-structure-string: Not Implemented Error!!! Python code not implemented"))

(defclass Trajectory ()
  ((id :accessor id :initform (format nil "~W" (uuid:make-v4-uuid)))
   (shown :accessor shown :type bool :initform :true)))

(defmethod get-coordinates ((Trajectory Trajectory) index)
  (error "Error in get-coordinates: Not Implemented Error!!! Python code not implemented"))

(defmethod n-frames ((Trajectory Trajectory))
  (error "Error in n-frames: Not Implemented Error!!! Python code not implemented"))

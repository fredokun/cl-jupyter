(defpackage #:traitlets
  (:use #:cl)
  (:export #:traitlet-class)
  (:export #:traitlet-metadata))

(in-package #:traitlets)

(defclass traitlet (clos:slot-definition)
  ((metadata :initarg :metadata :accessor metadata :initform nil)))

(defclass direct-traitlet (traitlet clos:standard-direct-slot-definition)
  ())

(defclass effective-traitlet (traitlet clos:standard-effective-slot-definition)
  ())

;;; If a user tries to probe a (super)class slot with no metadata, return no metadata.
(defmethod metadata ((slot clos:effective-slot-definition))
  nil)

;;; User interface
(defun traitlet-metadata (class-designator slot-name key)
  (check-type class-designator (or symbol class)
	      "a class designator")
  (let* ((class (if (symbolp class-designator)
		    (find-class class-designator)
		    class-designator))
	 (_ (unless (clos:class-finalized-p class)
	      (clos:finalize-inheritance class)))
	 (slots (clos:class-slots class))
	 (slot (or (find slot-name slots :key #'clos:slot-definition-name)
		   (error "slot missing in class ~a: ~a" class slot-name)))
	 (md (metadata slot)))
    (declare (ignore _))
    (getf md key)))

(defclass traitlet-class (standard-class) ())

(defmethod clos:validate-superclass ((class traitlet-class) (super standard-class)) t)

(defmethod clos:direct-slot-definition-class ((class traitlet-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'direct-traitlet))

(defmethod clos:effective-slot-definition-class ((class traitlet-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'effective-traitlet))

(defmethod clos:compute-effective-slot-definition :around ((class traitlet-class) name direct-slot-definitions)
  ;; This doesn't seem like the right way to do it, but according to AMOP we have to let the standard
  ;; method fire, and let it return its result. (Just as well, we don't know how to compute everything.)
  (declare (ignore name))
  (let ((result (call-next-method)))
    (setf (metadata result)
	  ;; Metadata are plists, so just append.
	  (loop for dsd in direct-slot-definitions appending (metadata dsd)))
    result))

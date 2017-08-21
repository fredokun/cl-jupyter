
(in-package #:traitlets)

(cl-jupyter-widgets:widget-log "Loading traitlets.lisp~%")

;;;; Something like Python traitlets.
;;;; Defines a metaclass traitlet-class that enables most features.
;;;; Adds additional slot options:
;;;; * :metadata has an arbitrary plist, retrievable by calling
;;;;   METACLASS on the slotd, or more conveniently with TRAITLET-METADATA.
;;;; * :validator has something coercable to a function.
;;;;   When the value of the slot is set, the validator is called with
;;;;   the instance and the new value as arguments, and whatever it returns
;;;;   is what's actually put in the slot.
;;;; * :observers has a list of things coerceable to functions.
;;;;   When the value of the slot is set, the observers are called in an
;;;;   undefined order with four arguments: the object, the name of the
;;;;   slot, the new value in the slot, and the old value in the slot.
;;;;   If the slot was previously unbound, the observer is not called.
;;;;   It's not specified when this is done in relation to changing the
;;;;   slot value; observers should use their arguments to get the values.
;;;;   However if there's a validator that's run first.

;;;; An additional class synced-object, if inherited from, puts a read-write
;;;; lock on slots with :sync <true> in their metadata.

;;; Example use of validation:
#+(or)
(progn
  (defclass range ()
    ((%min :initarg :min :accessor minimum) (%max :initarg :max :accessor maximum)
     (%value :accessor value :validator validate-range))
    (:metaclass traitlet-class))
  (defun validate-range (instance val)
    (let ((min (minimum instance)) (max (maximum instance)))
      (cond ((< val min) min)
            ((> val max) max)
            (t val))))
  (let ((instance (make-instance 'range :min 0 :max 10)))
    (setf (value instance) 17)
    (value instance)) ; => 10
)
;;; Example use of observers:
#+(or)
(progn
  (defclass foo ()
    ((%bar :initarg :bar :accessor bar :observers (display-change)))
    (:metaclass traitlet-class))
  (defun display-change (object name new old)
    (format t "Slot ~a of ~a changed: ~a -> ~a~%" name object old new))
  (let ((i (make-instance 'foo :bar 7))) ; no output
    (setf (bar i) 3)) ; Slot %BAR of #<FOO> changed: 7 -> 3
  )

;;; metadata, syncing, validation, and whatever else could be separate classes,
;;; but they're all pretty simple and we're basically just here to mimic traitlets.

(defclass traitlet (clos:slot-definition)
  ((validator :initarg :validator :accessor validator)
   (observers :initarg :observers :accessor observers :initform nil)
   (metadata :initarg :metadata :accessor metadata :initform nil)))

(defclass direct-traitlet (traitlet clos:standard-direct-slot-definition)
  ())

(defclass effective-traitlet (traitlet clos:standard-effective-slot-definition)
  ())

;;; If a user tries to probe a (super)class slot with no metadata, return no metadata.
(defmethod metadata ((slot clos:effective-slot-definition))
  nil)

;;; Ditto for observers.
(defmethod observers ((slot clos:effective-slot-definition))
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
    (setf (observers result)
          ;; observers are lists, so just append.
          (loop for dsd in direct-slot-definitions appending (observers dsd)))
    (let ((validatorfs (loop for dsd in direct-slot-definitions
                             when (slot-boundp dsd 'validator)
                               collect (validator dsd))))
      (cond ((null validatorfs))
            ((null (rest validatorfs))
             (setf (validator result) (first validatorfs)))
            (t
             (setf (validator result)
                   (make-multi-validator validatorfs)))))
    result))

(defun make-multi-validator (validatorfs)
  ;; let inside validate first.
  ;; this isn't the most efficient way to do it, but who cares?
  (lambda (instance value)
    (loop for vf in validatorfs
          do (setf value (funcall vf instance value)))
    value))

;;; Abstract. All objects with :sync t should be a subclass of this, to get the slot.
(defclass synced-object ()
  ((%mutex :initform (mp:make-shared-mutex) :accessor mutex))
  (:metaclass traitlet-class))

#++
(defmethod (setf clos:slot-value-using-class) (val (class traitlet-class) self (slotd effective-traitlet))
  (if (slot-boundp slotd 'validator)
      (progn
	(cljw:widget-log "About to call validator and then set the slot: ~s~%" slotd)
	(call-next-method
	 (funcall (coerce (validator slotd) 'function) self val)
	 class self slotd))
      (call-next-method)))

(defmethod (setf clos:slot-value-using-class) :before
    (new-value (class traitlet-class) object (slotd effective-traitlet))
  (when (clos:slot-boundp-using-class class object slotd)
    (let ((old (clos:slot-value-using-class class object slotd)))
      (loop for observer in (observers slotd)
            do (funcall (coerce observer 'function)
                        object (clos:slot-definition-name slotd) new-value old)))))

(defmethod (setf clos:slot-value-using-class) :after
    (new-value (class traitlet-class) object (slotd effective-traitlet))
  (cljw:notify-change object (clos:slot-definition-name slotd) new-value))


(defmacro with-shared-lock (shared-mutex &body body)
  (let ((smutex (gensym "SHARED-MUTEX")))
    `(let ((,smutex ,shared-mutex))
       (unwind-protect
	    (progn (mp:shared-lock ,smutex)
		   ,@body)
	 (mp:shared-unlock ,smutex)))))

(defmacro with-write-lock (shared-mutex &body body)
  (let ((smutex (gensym "SHARED-MUTEX")))
    `(let ((,smutex ,shared-mutex))
       (unwind-protect
	    (progn (mp:write-lock ,smutex)
		   ,@body)
	 (mp:write-unlock ,smutex)))))

(defmethod clos:slot-value-using-class
    ((class traitlet-class) (object synced-object) (slotd effective-traitlet))
  #++(cljw:widget-log "Process: ~a READING slot -> ~s  (getf (metadata slotd) :sync) -> ~a~%" mp:*current-process* slotd (getf (metadata slotd) :sync))
  (if (getf (metadata slotd) :sync)
      (progn
	(cljw:widget-log " About to with-shared-lock~%")
	(with-shared-lock (mutex object) (call-next-method)))
      (call-next-method)))

#++(defmethod (setf clos:slot-value-using-class)
    (new-value (class traitlet-class) (object synced-object) (slotd effective-traitlet))
     #++(cljw:widget-log "Process: ~a WRITING slot -> ~s (getf (metadata slotd) :sync) -> ~s~%" mp:*current-process* slotd (getf (metadata slotd) :sync))
  (if (getf (metadata slotd) :sync)
      (progn
	(cljw:widget-log " About to with-write-lock~%")
	(with-write-lock (mutex object) (call-next-method)))
      (call-next-method)))

(defmethod (setf clos:slot-value-using-class)
    (new-value (class traitlet-class) (object synced-object) (slotd effective-traitlet))
  #++(cljw:widget-log "Process: ~a WRITING slot -> ~s (getf (metadata slotd) :sync) -> ~s~%" mp:*current-process* slotd (getf (metadata slotd) :sync))
  (if (getf (metadata slotd) :sync)
      (if (slot-boundp slotd 'validator)
	  (progn
	    (cljw:widget-log "About to call validator and then set the slot: ~s~%" slotd)
	    (let ((validated-value (funcall (coerce (validator slotd) 'function) object new-value)))
	      (with-write-lock (mutex object) (call-next-method validated-value class object slotd))))
	  (call-next-method))
      (call-next-method)))

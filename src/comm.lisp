(in-package #:cl-jupyter-widgets)

(defclass comm ()
  ((kernel :initarg :kernel :accessor kernel :initform *kernel*)
   (comm-id :initarg :comm-id :accessor comm-id
	    :initform (format nil "~W" (uuid:make-v4-uuid))
	    :documentation "Am I the primary or secondary Comm?")
   (primary :initarg :primary :accessor primary :initform t :type boolean)
   (target-name :initarg :target-name :accessor target-name :initform "comm" :type string)
   (target-module :initarg :target-module 
		  :accessor target-module
		  :initform :null
		  :type (or string (eql :null))
		  :documentation "requirejs module from which to load comm target.")
   (topic :initarg :topic :accessor topic)
   (%open-data :initarg :open-data :accessor %open-data :documentation "data dict, if any, to be included in comm-open")
   (%close-data :initarg :close-data :accessor %close-data :documentation "data dict, if any, to be included in comm-close")
   (%msg-callback :initarg :msg-callback :accessor %msg-callback)
   (%close-callback :initarg :close-callback :accessor %close-callback)
   (%closed :initarg :closed :accessor %closed :initform t :type boolean))
  )


;;; This does what comm.py::__init__ does
(defun comm.__init__ (&key target-name data metadata buffers)
  (let ((comm (if target-name
		  (make-instance 'comm :target-name target-name)
		  (make-instance 'comm))))
    (widget-log "*kernel* --> ~s  (kernel comm) -> ~s~%" *kernel* (kernel comm))
    (when (kernel comm)
      (if (primary comm)
	  (open comm :data data :metadata metadata :buffers buffers)
	  (setf (%closed comm) nil)))
    comm))
		  
(defmethod initialize-instance :after ((instance comm) &rest initargs)
  (unless (slot-boundp instance 'topic) (setf (topic instance) (format nil "comm-~s" (comm-id instance)))))

(defmethod %publish-msg ((self comm) msg-type &key (data nil) (metadata nil) buffers target-name target-module)
  (widget-log "Comm.%publish-msg (COMMON-LISP)  -+-+-+-+-+-+-+-+-+-+~%")
  (widget-log "msg-type -> ~a~%" msg-type)
  (widget-log "metadata -> ~a~%" metadata)
  (widget-log "(message-parent-header cl-jupyter::*parent-msg* -> ~a~%"
	      (myjson::encode-json-to-string (cl-jupyter::message-parent-header cl-jupyter::*parent-msg*)))
  (widget-log "ident/topic -> ~a~%" (topic self))
  (widget-log "buffers -> ~a~%" buffers)
  (widget-log "++++++++++++++ contents >>>>>>>>>~%")
  (let ((cleaned-json (json-clean `(("data" . ,data)
				    ("comm_id" . ,(comm-id self))
				    ("target_name" . ,target-name)
				    ("target_module" . ,target-module)))))
    (widget-log "   %publish-msg  cleaned-json -> ~a"
		(with-output-to-string (sout)
		  (print-as-python cleaned-json sout)))
    (widget-log "   session -> ~a~%" (cl-jupyter::kernel-session (kernel self)))
    (session-send (cl-jupyter::kernel-session (kernel self))
		  (cl-jupyter::kernel-iopub (kernel self))
		  msg-type
		  :content cleaned-json
		  :metadata (json-clean metadata)
		  :parent cl-jupyter::*parent-msg*
		  :ident (topic self)
		  :buffers buffers)
    (widget-log "   real-msg -> CAN I GET A REAL MESSAGE FROM SESSION-SEND???~%")))


(defmethod open ((self comm) &key (data (%open-data self)) metadata buffers)
  "Open the frontend-side version of this comm"
  (let ((comm-manager (or (make-comm-manager (kernel self)) (error "Comms cannot be opened without a kernel and a comm-manager attached to that kernel."))))
    (register-comm comm-manager self)
    (or (slot-boundp self 'target-name) (error "The slot target-name is not bound in the comm ~a" self))
    (unwind-protect
	 (progn
	   (%publish-msg self "comm_open"
			 :target-name (target-name self)
			 :target-module (target-module self)
			 :data data
			 :metadata metadata
			 :buffers buffers)
	   (setf (%closed self) nil))
      (unregister-comm comm-manager self))))

(defmethod close ((self comm) &key data metadata buffers)
  (when (%closed self) (return-from close))
  (setf (%closed self) t)
  (unless (kernel self) (return-from close))
  (unless data (setf data (%close-data self)))
  (%publish-msg self "comm_close" :data data :metadata metadata :buffers buffers)
  (unregister-comm (make-comm-manager (kernel self)) self))

(defmethod send ((self comm) &key data metadata buffers)
  "Send a message to the frontend-side version of this comm"
   (%publish-msg self "comm_msg" :data data :metadata metadata :buffers buffers))

(defmethod on-close ((self comm) callback)
  "Register a callback for comm-close

   Will be called with the DATA of the close message.

   Call (on-close nil) to disable an existing callback."
   (check-type callback (or function null) "A function of one argument or NIL")
   (setf (%close-callback self) callback))

(defmethod on-msg ((self comm) callback)
  "Register a callback for comm-msg

   Will be called with the DATA of any comm-msg messages.

   Call (on-msg nil) to disable an existing callback."
   (check-type callback (or function null) "A function of one argument or NIL")
   (setf (%msg-callback self) callback))

(defmethod handle-close ((self comm) msg)
  "Handle a comm-close message"
   (print (list 'handle-close (comm-id self) msg) *error-output*)
   (when (%close-callback self) (funcall (%close-callback self) msg)))

(defmethod handle-msg ((self comm) msg)
  "Handle a comm-msg message"
   (print (list 'handle-msg (comm-id self) msg) *error-output*)
   (when (%msg-callback self)
     (let ((shell (cl-jupyter::kernel-shell (kernel self))))
       (when shell (trigger (events shell) "pre_execute"))
       (funcall (%msg-callback self) msg)
       (when shell (trigger (events shell) "post_execute")))))

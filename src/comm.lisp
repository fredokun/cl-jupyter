(in-package #:cl-jupyter-widgets)

(defclass comm ()
  ((kernel :initarg :kernel :accessor kernel :initform cl-jupyter:*kernel*)
   (comm-id :initarg :comm-id :accessor comm-id
	    :initform (format nil "~W" (uuid:make-v4-uuid))
	    :documentation "Am I the primary or secondary Comm?")
   (primary :initarg :primary :accessor primary :initform t :type bool)
   (target-name :initarg :target-name :accessor target-name :initform "comm" :type string)
   (target-module :initarg :target-module 
		  :accessor target-module
		  :initform :null
		  :type (or string (eql :null))
		  :documentation "requirejs module from which to load comm target.")
   (topic :initarg :topic :accessor topic)
   (%open-data :initarg :open-data :accessor %open-data :documentation "data dict, if any, to be included in comm-open")
   (%close-data :initarg :close-data :accessor %close-data :documentation "data dict, if any, to be included in comm-close")
   (%msg-callback :initarg :msg-callback :accessor %msg-callback :initform nil)
   (%close-callback :initarg :close-callback :accessor %close-callback :initform nil)
   (%closed :initarg :closed :accessor %closed :initform t :type bool))
  )


#++
(defmethod (setf comm-id) :around (value (x comm))
  (widget-log "!!!!!! (setf comm-id) to ~a~%" value)
  (call-next-method))

;;; This does what comm.py::__init__ does
(defun comm.__init__ (&key target-name data metadata (buffers #()))
  (check-type buffers array)
  (let ((comm (if target-name
		  (make-instance 'comm :target-name target-name)
		  (make-instance 'comm))))
    (widget-log "comm.__init__  *kernel* --> ~s  (kernel comm) -> ~s~%" cl-jupyter:*kernel* (kernel comm))
    (when (kernel comm)
      (if (primary comm)
	  (open comm :data data :metadata metadata :buffers buffers)
	  (setf (%closed comm) nil)))
    comm))
		  
(defmethod initialize-instance :after ((instance comm) &rest initargs)
  (unless (slot-boundp instance 'topic) (setf (topic instance) (format nil "comm-~s" (comm-id instance)))))

(defmethod %publish-msg ((self comm) msg-type &key (data nil) (metadata nil) (buffers #()) keys)
  (check-type buffers array)
  (widget-log "Comm._publish_msg (COMMON-LISP)  -+-+-+-+-+-+-+-+-+-+~%")
  (widget-log "msg-type -> ~a~%" msg-type)
  (widget-log "metadata -> ~a~%" metadata)
  (widget-log "(message-header cl-jupyter::*parent-msg* -> ~a~%"
	      (and cl-jupyter:*parent-msg*
		   (myjson::encode-json-to-string (cl-jupyter::message-header cl-jupyter::*parent-msg*))))
  (widget-log "ident/topic -> ~a~%" (topic self))
  (widget-log "buffers -> ~a~%" buffers)
  (widget-log "++++++++++++++ contents >>>>>>>>>~%")
  (let ((cleaned-json (json-clean (list* (cons "data" data)
					 (cons "comm_id"  (comm-id self))
					 keys))))
    (widget-log "~a" (with-output-to-string (sout)
		       (print-as-python cleaned-json sout :indent 4)))
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


(defmethod open ((self comm) &key (data (%open-data self)) metadata (buffers #()))
  "Open the frontend-side version of this comm"
  (check-type buffers array)
  (let ((comm-manager (or (gethash (kernel self) *kernel-comm-managers*)
			  (error "Comms cannot be opened without a kernel and a comm-manager attached to that kernel."))))
    (widget-log "comm::open  About to register with comm-manager comm: ~a~%" self)
    (register-comm comm-manager self)
    (or (slot-boundp self 'target-name) (error "The slot target-name is not bound in the comm ~a" self))
    (handler-case
	(progn
	  (%publish-msg self "comm_open"
			:target-name (target-name self)
			:target-module (target-module self)
			:data data
			:metadata metadata
			:buffers buffers
			:keys (list (cons "target_name" (target-name self))
				    (cons "target_module" (target-module self))))
	  (setf (%closed self) nil))
      (error (err)
	(unregister-comm comm-manager self)))))

(defmethod close ((self comm) &key data metadata (buffers #()))
  (check-type buffers array)
  (when (%closed self) (return-from close))
  (setf (%closed self) t)
  (unless (kernel self) (return-from close))
  (unless data (setf data (%close-data self)))
  (%publish-msg self "comm_close" :data data :metadata metadata :buffers buffers)
  (unregister-comm (gethash (kernel self) *kernel-comm-managers*) self))

(defmethod send ((self comm) &key data metadata (buffers #()))
  "Send a message to the frontend-side version of this comm"
  (check-type buffers array)
  (widget-log "comm.send  data -> ~s~%" data)
  (prog1
      (%publish-msg self "comm_msg" :data data :metadata metadata :buffers buffers)
    (widget-log "comm.send DONE~%")))

(defmethod on-close ((self comm) callback)
  "Register a callback for comm-close

   Will be called with the DATA of the close message.

   Call (on-close nil) to disable an existing callback."
   (check-type callback (or function null) "A function of one argument or NIL")
   (setf (%close-callback self) callback))

(defmethod on-msg ((self comm) callback &key)
  "Register a callback for comm-msg

   Will be called with the widget and the DATA of any comm-msg messages.

   Call (on-msg nil) to disable an existing callback."
  (check-type callback (or function null) "A function of one argument or NIL")
  (widget-log "Got comm::on-msg with callback: ~a~%" callback)
  (setf (%msg-callback self) callback))

(defmethod handle-close ((self comm) msg)
  "Handle a comm-close message"
   (print (list 'handle-close (comm-id self) msg) *error-output*)
   (when (%close-callback self) (funcall (%close-callback self) msg)))

(defmethod handle-msg ((self comm) msg)
  "Handle a comm-msg message"
  (widget-log "Handling message =================================~%")
  (widget-log "comm-id -> ~a~%" (comm-id self))
  (widget-log "msg -> ~a~%" msg);;(as-python msg))
  (if (%msg-callback self)
      (let ((shell (cl-jupyter::kernel-shell (kernel self))))
	(widget-log "About to trigger pre_execute skipping for now - for shell: ~a~%" shell)
	#+(or)(when shell (trigger (events shell) "pre_execute"))
	(widget-log "About to funcall (%msg-callback self) -> ~a~%" (%msg-callback self))
	(funcall (%msg-callback self) msg)
	(widget-log "About to trigger post_execute - skipping for now~%")
	#+(or)(when shell (trigger (events shell) "post_execute")))
      (widget-log "The comm msg_callback is unbound")))

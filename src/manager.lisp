;;;
;;; Comments are copied from ipykernel
(in-package #:cl-jupyter-widgets)



(defmacro log-error (fmt &rest args)
  `(format t ,fmt ,@args))

(defvar *kernel-comm-managers* (make-hash-table)
  "Map kernels to comm-managers")


(defclass comm-manager ()
  ((kernel :initarg :kernel :reader kernel)
   (comms :initform (make-hash-table :test #'equal) :reader comms)
   (targets :initform (make-hash-table :test #'equal) :reader targets)))

(defun make-comm-manager (kernel)
  (make-instance 'comm-manager :kernel kernel))

(defmethod register-target ((self comm-manager) target-name func)
  "Register a function func for a given target name.
   func takes two arguments (comm-instance comm-message)"
  (setf (gethash target-name (targets self)) func))

(defmethod unregister-target ((self comm-manager) target-name)
  (remhash target-name (targets self)))

(defmethod register-comm ((self comm-manager) comm)
  (let ((comm-id (comm-id comm)))
    (setf (kernel comm) (kernel self))
    (setf (gethash comm-id (comms self)) comm)
    comm-id))

(defmethod unregister-comm ((self comm-manager) comm)
  (remhash (comm-id comm) (comms self)))

(defmethod get-comm ((self comm-manager) comm-id)
  (let ((comm (gethash comm-id (comms self) nil)))
    (or comm
	(progn
	  (warn "No such comm: ~a" comm-id)
	  nil))))

(defmethod comm-open ((self comm-manager) stream ident msg)
  "Handle comm_open messages"
  (let* ((content (cdr (assoc "content" msg)))
	 (comm-id (cdr (assoc "comm_id" content)))
	 (target-name (cdr (assoc "target_name" content)))
	 (func (gethash target-name (targets self)))
	 (comm (make-instance 'comm
			      :comm-id comm-id
			      :primary nil
			      :target-name target-name)))
    (register-comm self comm)
    (if func
	(handler-case
	    (progn
	      (funcall func comm msg)
	      (return-from comm-open))
	  (error (err)
	    (log-error "Exception opening comm with target: ~a  error: ~a" target-name err)))
	(log-error "No such comm target registered: ~a" target-name))
    (handler-case
	(close comm)
      (error (err)
	(log-error "Could not close comm during comm-open failure cleanup. The comm may not have been opened yet err: ~a" err)))))

(defmethod comm-msg ((self comm-manager) stream ident msg)
    (let* ((content (cdr (assoc "content" msg)))
	   (comm-id (cdr (assoc "comm_id" content)))
	   (comm (get-comm self comm-id)))
      (unless comm (return-from comm-msg nil))
      (handler-case
	  (handle-msg comm msg)
	(error (err)
	  (log-error "Exception in comm-msg for %s error: %s" comm-id err)))))

(defmethod comm-close ((self comm-manager) stream ident msg)
  (let* ((content (cdr (assoc "content" msg)))
	 (comm-id (cdr (assoc "comm_id" content)))
	 (comm (get-comm self comm-id)))
    (unless comm (return-from comm-close nil))
    (remhash comm-id (comms self))
    (handler-case
	(handle-close comm msg)
      (error (err)
	(log-error "Exception in comm-close for %s error: %s" comm-id err)))))
    
      

	  



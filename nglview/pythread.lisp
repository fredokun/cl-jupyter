(in-package :pythread)

(cljw:widget-log "pythread.lisp~%")



(defclass remote-call-callback ()
  ((%callback :initarg :callback :accessor callback)
   (%method-name :initarg :method-name :accessor method-name)))

(defun fire-callback (widget callback)
  (funcall (callback callback) widget))

(defun remote-call-thread-run (widget registered-funcs)
  "Keep pulling callbacks out of the queue and evaluating them"
  (loop
     (multiple-value-bind (callback found)
	 (mp:queue-wait-dequeue-timed (nglv:remote-call-thread-queue widget) 1000)
       (when found
	 ;; messages are sent within the dynamic environment of a specific *parent-msg*,*shell* and *kernel*
	 (funcall (callback callback) widget)
	 (when (member (method-name callback) registered-funcs :test #'string=)
	   (cljw:widget-log "method-name is one of ~s - waiting until callback is finished~%" registered-funcs)
	   (%wait-until-finished widget))
	 (cljw:widget-log "Callback finished~%")))))


(defun remote-call-add (widget callback)
  #+(or)(mp:queue-enqueue (remote-call-thread-queue widget) callback)
  (fire-callback widget callback))

(cljw:widget-log "defclass event  pythread.lisp~%")

(defclass event ()
  ((%shared-mutex :initform (mp:make-shared-mutex 'event)
		  :accessor shared-mutex)
   (%event-value  :initform nil
		  :accessor event-value)))

(cljw:widget-log "defmethod is-set  pythread.lisp~%")

(defmethod is-set ((event event))
  (unwind-protect
       (progn
	 (mp:shared-lock (shared-mutex event))
	 (event-value event))
    (mp:shared-unlock (shared-mutex event))))


(defmethod event-set ((event event))
  (unwind-protect
       (progn
	 (mp:write-lock (shared-mutex event))
	 (setf (event-value event) t))
    (mp:write-unlock (shared-mutex event))))


(cljw:widget-log "defmethod clear  pythread.lisp~%")

(defmethod clear ((event event))
  (unwind-protect
       (progn
	 (mp:write-lock (shared-mutex event))
	 (setf (event-value event) nil))
    (mp:write-unlock (shared-mutex event))))

(cljw:widget-log "done  pythread.lisp~%")





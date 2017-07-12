(in-package :pythread)

(cljw:widget-log "pythread.lisp~%")


(defun remote-call-thread-run (widget)
  "Keep pulling callbacks out of the queue and evaluating them"
  (loop
       (multiple-value-bind (callback found)
	   (mp:queue-wait-dequeue-timed (remote-call-thread-queue widget) 1000)
	 (when found
	   (funcall (car callback) widget)
	 (when (string= (cdr callback) "loadFile")
	   (%wait-until-finished widget))))))


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

(defmethod set ((event event))
  (unwind-protect
       (progn
	 (mp:lock (shared-mutex event))
	 (setf (event-value event) t))
    (mp:shared-lock (shared-mutex event))))

(cljw:widget-log "defmethod clear  pythread.lisp~%")

(defmethod clear ((event event))
  (unwind-protect
       (progn
	 (mp:lock (shared-mutex event))
	 (setf (event-value event) nil))
    (mp:shared-lock (shared-mutex event))))

(cljw:widget-log "done  pythread.lisp~%")

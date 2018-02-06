(in-package :pythread)

(cljw:widget-log "pythread.lisp~%")



(defclass remote-call-callback ()
  ((%callback :initarg :callback :accessor callback)
   (%widget :initarg :widget :accessor widget)
   (%method-name :initarg :method-name :accessor method-name)
   (%special-variables :initarg :special-variables :accessor special-variables)
   (%special-values :initarg :special-values :accessor special-values)))


(defun make-remote-call-callback (&key widget method-name callback)
  (make-instance 'remote-call-callback
                 :widget widget
                 :method-name method-name
                 :callback callback
                 :special-variables cl-jupyter:*special-variables*
                 :special-values (mapcar #'symbol-value cl-jupyter:*special-variables*)))

                 
(defun fire-callback (callback &optional passed-widget)
  (when passed-widget
    (unless (eq passed-widget (widget callback))
      (error "passed-widget ~s does not match callback widget ~s" passed-widget (widget callback))))
  (progv (special-variables callback) (special-values callback)
    (funcall (callback callback) (widget callback))))

(defun remote-call-thread-run (registered-funcs)
  "Keep pulling callbacks out of the queue and evaluating them"
  (cljw:widget-log "Starting remote-call-thread-run~%")
  (loop
    (let ((callback (clext.queue:dequeue *remote-call-thread-queue*))) ;; (nglv:remote-call-thread-queue widget())))
      ;; messages are sent within the dynamic environment of a specific *parent-msg*,*shell* and *kernel*
      (cljw:widget-log "remote-call-thread-run callback: ~s~%" callback)
      (cond
        ((eq callback :shutdown)
         (return-from remote-call-thread-run nil))
        ((eq callback :status)
         (format t "I am still alive~%"))
        ((eq callback :ping)
         (format t "PONG~%"))
        ((typep callback 'remote-call-callback)
         (fire-callback callback)
         (when (member (method-name callback) registered-funcs :test #'string=)
           (cljw:widget-log "method-name is one of ~s - waiting until callback is finished~%" registered-funcs)
           (%wait-until-finished widget))
         (cljw:widget-log "Callback finished~%"))
        (t
         (format t "Handle remote-call-thread-run callback: ~a~%" callback)
         (cljw:widget-log "Handle remote-call-thread-run callback: ~a~%" callback)))
      (cljw:widget-log "remote-call-thread-run done handling callback: ~s~%" callback))))

(defun remote-call-add (message-or-callback)
  (clext.queue:enqueue *remote-call-thread-queue* message-or-callback))

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


(defparameter *remote-call-thread-queue* (clext.queue:make-queue 'remote-call-thread-queue))

(defparameter *remote-call-thread* (mp:process-run-function
                                    'remote-call-thread
                                    (lambda () (remote-call-thread-run
                                                (list "loadFile" "replaceStructure")))))




(in-package :pythread)

(cljw:widget-log "pythread.lisp~%")



(defclass remote-call-callback ()
  ((%callback :initarg :callback :accessor callback)
   (%ngl-msg :initarg :ngl-msg :accessor ngl-msg)
   (%widget :initarg :widget :accessor widget)
   (%method-name :initarg :method-name :accessor method-name)
   (%special-variables :initarg :special-variables :accessor special-variables)
   (%special-values :initarg :special-values :accessor special-values)))


(defun make-remote-call-callback (&rest args)
  (apply 'make-instance
         'remote-call-callback
       :special-variables cl-jupyter:*special-variables*
       :special-values (mapcar #'symbol-value cl-jupyter:*special-variables*)
       args))

                 
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
           (nglv:wait-until-finished (widget callback)))
         (cljw:widget-log "Callback finished~%"))
        (t
         (format t "Handle remote-call-thread-run callback: ~a~%" callback)
         (cljw:widget-log "Handle remote-call-thread-run callback: ~a~%" callback)))
      (cljw:widget-log "remote-call-thread-run done handling callback: ~s~%" callback))))

(defun remote-call-add (message-or-callback)
  (clext.queue:enqueue *remote-call-thread-queue* message-or-callback))

(cljw:widget-log "defclass event  pythread.lisp~%")

(defclass event ()
  ((%shared-mutex :initform (bordeaux-threads:make-lock 'event)
		  :accessor shared-mutex)
   (%event-value  :initform nil
		  :accessor event-value)))

(cljw:widget-log "defmethod is-set  pythread.lisp~%")

(defmethod is-set ((event event))
  (unwind-protect
       (progn
	 (bordeaux-threads:acquire-lock (shared-mutex event))
	 (event-value event))
    (bordeaux-threads:release-lock (shared-mutex event))))


(defmethod event-set ((event event))
  (unwind-protect
       (progn
	 (bordeaux-threads:acquire-lock (shared-mutex event))
	 (setf (event-value event) t))
    (bordeaux-threads:release-lock (shared-mutex event))))


(cljw:widget-log "defmethod clear  pythread.lisp~%")

(defmethod clear ((event event))
  (unwind-protect
       (progn
	 (bordeaux-threads:acquire-lock (shared-mutex event))
	 (setf (event-value event) nil))
    (bordeaux-threads:release-lock (shared-mutex event))))

(cljw:widget-log "done  pythread.lisp~%")


(defparameter *remote-call-thread-queue* (clext.queue:make-queue 'remote-call-thread-queue))

(defparameter *remote-call-thread* (bordeaux-threads:make-thread 
                                    (lambda () (remote-call-thread-run
                                                (list "loadFile" "replaceStructure")))
                                    :name 'remote-call-thread))





(in-package #:cl-jupyter-widgets)

(defvar *debug-cl-jupyter-widgets* t)

(defun backtrace-as-string ()
  (with-output-to-string (*standard-output*)
    (core::clasp-backtrace)))


;;; Use widget-log to log messages to a file
(defvar *widget-log* nil)
(defvar *widget-log-lock* nil)

(eval-when (:compile-toplevel)
  (let ((val (ext:getenv "USE_WIDGET_LOG")))
    (when val
      (push :use-widget-log *features*))))
;;; Always turn it off for now
(eval-when (:compile-toplevel :load-toplevel :execute)
     (pushnew :use-widget-log *features*))

(eval-when (:execute :load-toplevel)
  (let ((log-file-name (cond
			 ((probe-file "/home/app/logs/")
			  "/home/app/logs/cl-jupyter.log")
			 (t "/tmp/cl-jupyter.log"))))
    (setf *widget-log* (cl:open log-file-name
				:direction :output
				:if-exists :append
				:if-does-not-exist :create))
    (format *widget-log* "Log started up~%")
    (setf *widget-log-lock* (mp:make-lock :name 'widget-log))
    (format *widget-log* "About to start widget-log process~%")
    (defun always-widget-log (fmt &rest args)
      (let ((msg (apply #'format nil fmt args)))
        (unwind-protect
             (progn
               (mp:lock *widget-log-lock* t)
               (princ msg *widget-log*)
               (finish-output *widget-log*))
          (mp:unlock *widget-log-lock*))))
    (format *widget-log* "===================== new run =======================~%")))

#+use-widget-log
(defmacro widget-log (fmt &rest args)
  `(always-widget-log ,fmt ,@args))
#-use-widget-log
(defmacro widget-log (fmt &rest args)
  nil)



(eval-when (:execute :load-toplevel)
  (setf *debugger-hook*
	#'(lambda (condition &rest args)
	    (always-widget-log "~a~%" condition)
	    (always-widget-log "~a~%" (backtrace-as-string)))))

(export 'widget-log)

(widget-log "Starting load   packages -> ~a~%" (list-all-packages))

(defmacro with-error-handling (msg &body body)
  (let ((wrn (gensym))
	(err (gensym)))
    `(handler-case
	 (handler-bind
	     ((simple-warning
	       #'(lambda (,wrn)
		   (format *error-output* "~&~a ~A: ~%" ,msg (class-name (class-of ,wrn)))
		   (apply (function format) *error-output*
			  (simple-condition-format-control   ,wrn)
			  (simple-condition-format-arguments ,wrn))
		   (format *error-output* "~&")
		   (muffle-warning)))
	      (warning
	       #'(lambda (,wrn)
		   (format *error-output* "~&~a ~A: ~%  ~A~%"
			   ,msg (class-name (class-of ,wrn)) ,wrn)
		   (muffle-warning)))
	      (serious-condition
	       #'(lambda (,err)
		   (format t "!!!!! A serious condition was encountered in with-error-handling - check log~%")
		   (finish-output)
		   (widget-log "~a~%" ,msg)
		   (widget-log "An error occurred of type ~a~%" (class-name (class-of ,err)))
;;		   (widget-log "~a~%" ,err)
		   (widget-log "~a~%" (backtrace-as-string)))))
	   (progn ,@body))
       (simple-condition (,err)
	 (format *error-output* "~&~A: ~%" (class-name (class-of ,err)))
	 (apply (function format) *error-output*
		(simple-condition-format-control   ,err)
		(simple-condition-format-arguments ,err))
	 (format *error-output* "~&"))
       (serious-condition (,err)
	 (format *error-output* "~&2An error occurred of type: ~A: ~%  ~S~%"
		 (class-name (class-of ,err)) ,err)))))








(defun json-clean (json)
  json)


(defun extract-message-content (msg)
  (myjson:parse-json-from-string (cl-jupyter:message-content msg)))

(defun assoc-contains (key-string alist)
  (assoc key-string alist :test #'string=))

(defun assoc-value (key-string alist &optional (default nil default-p))
  (or (listp alist) (error "alist must be a list"))
  (let ((pair (assoc key-string alist :test #'string=)))
    (if pair
	(cdr pair)
	(if default-p
	    default
	    (error "Could not find key ~a in dict ~a" key-string alist)))))

(defparameter *python-indent* 0)

(defparameter *sort-encoded-json* nil)

(defun print-as-python (object stream &key indent)
  (let ((*sort-encoded-json* t))
    (myjson::encode-json stream object :indent indent)))

#+(or)
(progn
  (defgeneric print-as-python (object stream))


  (defmethod print-as-python :around (object stream)
	     (let ((indent (make-string *python-indent* :initial-element #\space)))
	       (format stream "~a" indent)
	       (let ((*python-indent* (+ *python-indent* 4)))
		 (call-next-method))))

  (defmethod print-as-python ((object t) stream)
    (prin1 object stream))

  (defmethod print-as-python ((object string) stream)
    (prin1 object stream))

  (defmethod print-as-python ((object array) stream)
    (format stream "[= ")
    (loop for value across object
       do (let ((value-as-string (with-output-to-string (sout)
				   (print-as-python value sout))))
	    (format stream "~a, " value-as-string)))
    (format stream "]~%"))

  (defmethod print-as-python ((object symbol) stream)
    (cond
      ((eq object nil)
       (princ "[]" stream))
      ((eq object :null)
       (princ "null" stream))
      ((eq object :false)
       (princ "false" stream))
      ((eq object :true)
       (princ "true" stream))
      (t 
       (let ((as-string (string-downcase (string object))))
	 (prin1 as-string stream)))))


  (defun looks-like-python-dict-p (object)
    (and (listp object)
	 (every (lambda (x) (and (consp x) (stringp (car x)))) object )))

  (defmethod print-as-python ((object list) stream)
    (declare (optimize (debug 3)))
    (cond
      ((looks-like-python-dict-p object)
       (format stream "{~%")
       (let ((sorted-dict (sort (copy-list object) #'string<= :key #'car)))
	 (loop for (key . value) in sorted-dict
	    do (let ((value-as-string (with-output-to-string (sout)
					(print-as-python value sout))))
		 (format stream "~vt~s::: ~a,~%" *python-indent* key value-as-string))))
       (format stream "}"))
      (t 
       (format stream "[& ")
       (loop for value in object
	  do (let ((value-as-string (with-output-to-string (sout)
				      (print-as-python value sout))))
	       (format stream "~a, " value-as-string)))
       (format stream "]"))))
  )

(defun as-python (msg)
  (with-output-to-string (sout)
    (print-as-python msg sout)))


(defgeneric on-msg (target callback &key &allow-other-keys))


;;; ------------------------------------------------------------
;;;
;;; For debugging in slime
;;;
;;; In the jupyter notebook use (cl-jupyter-widgets::save-context)
;;;      That will save the context of that message.
;;; Then in slime evaluate (in-context (form))

(defparameter *debug-parent-msg* nil)
(defparameter *debug-shell* nil)
(defparameter *debug-kernel* nil)
(defparameter *debug-env* nil)

(defun save-jupyter-cell-state ()
  (setf *debug-env* (mapcar #'symbol-value cl-jupyter:*special-variables*)))

(defmacro in-jupyter-cell (form)
  `(progv ',cl-jupyter:*special-variables* ',*debug-env*
     ,form))

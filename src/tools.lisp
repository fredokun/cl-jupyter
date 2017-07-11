
(in-package #:cl-jupyter-widgets)

(defvar *debug-cl-jupyter-widgets* t)

(defun backtrace-to-stream (stream)
  (let ((*standard-output* stream))
    (core::clasp-backtrace)))


;;; Use widget-log to log messages to a file
(defvar *widget-log* nil)

(eval-when (:execute :load-toplevel)
  (setf *debugger-hook*
	#'(lambda (&rest args) (backtrace-to-stream *widget-log*)))
  (let ((log-file-name (cond
			 ((probe-file "/home/app/logs/")
			  "/home/app/logs/cl-jupyter.log")
			 (t "/tmp/cl-jupyter.log"))))
    (setf *widget-log* (cl:open log-file-name
				:direction :output
				:if-exists :append
				:if-does-not-exist :create))
    (format *widget-log* "===================== new run =======================~%")))

				     
(defun widget-log (fmt &rest args)
    (apply #'format *widget-log* fmt args)
    (finish-output *widget-log*))

#+(or)
(defmacro widget-log (fmt &rest args)
  nil)

(export 'widget-log)

(widget-log "Starting load   packages -> ~a~%" (list-all-packages))


(defmacro with-error-handling (msg &body body)
  `(handler-case
       (handler-bind
           ((simple-warning
             #'(lambda (wrn)
                 (format *error-output* "~&~a ~A: ~%" ,msg (class-name (class-of wrn)))
                 (apply (function format) *error-output*
                        (simple-condition-format-control   wrn)
                        (simple-condition-format-arguments wrn))
                 (format *error-output* "~&")
                 (muffle-warning)))
            (warning
             #'(lambda (wrn)
                 (format *error-output* "~&~a ~A: ~%  ~A~%"
                         ,msg (class-name (class-of wrn)) wrn)
                 (muffle-warning)))
	    (serious-condition
	     #'(lambda (err)
		 (let ((*standard-output* cl-jupyter-widgets::*widget-log*))
		   (format t "~&~a~%~a~%" ,msg err)
		   (core::clasp-backtrace)))))
	 (progn ,@body))
     (simple-condition (err)
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&"))
     (serious-condition (err)
       (format *error-output* "~&2An error occurred of type: ~A: ~%  ~S~%"
               (class-name (class-of err)) err))))








(defun json-clean (json)
  json)


(defun extract-message-content (msg)
  (myjson:parse-json-from-string (cl-jupyter:message-content msg)))


(defun assoc-value (key-string alist &optional (default nil default-p))
  (let ((pair (assoc key-string alist :test #'string=)))
    (if pair
	(cdr pair)
	(if default-p
	    default
	    (error "Could not find key ~a in dict ~a" key-string alist)))))

(defparameter *python-indent* 0)

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
	       (format stream "~vt~s : ~a,~%" *python-indent* key value-as-string))))
     (format stream "}~%"))
    (t 
     (format stream "[ ")
     (loop for value in object
	do (let ((value-as-string (with-output-to-string (sout)
				    (print-as-python value sout))))
	     (format stream "~a, " value-as-string)))
     (format stream "]~%"))))


(defun as-python (msg)
  (with-output-to-string (sout)
    (print-as-python msg sout)))

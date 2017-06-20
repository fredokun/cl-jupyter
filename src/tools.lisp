
(in-package #:cl-jupyter-widgets)

(defvar *debug-cl-jupyter-widgets* t)

;;; Use widget-log to log messages to a file
(defvar *widget-log* nil)
(eval-when (:execute :load-toplevel)
  (setf *widget-log* (cl:open "/home/app/logs/cl-jupyter.log"
			      :direction :output
			      :if-exists :append
			      :if-does-not-exist :create))
  (format *widget-log* "===================== new run =======================~%"))

(defun widget-log (fmt &rest args)
    (apply #'format *widget-log* fmt args)
    (finish-output *widget-log*))

#+(or)
(defmacro widget-log (fmt &rest args)
  nil)



(defun json-clean (json)
  json)





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

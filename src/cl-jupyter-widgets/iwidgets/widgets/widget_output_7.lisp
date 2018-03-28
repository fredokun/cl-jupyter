(in-package :cl-jupyter-widgets)

(defclass-widget-register output (domwidget)
  ((msg-id :initarg :msg-id :accessor msg-id
	   :type unicode
	   :initform (unicode "")
	   :metadata (:sync t
                      :json-name "msg_id"
                      :help "Parent message id of messages to capture."))
   (outputs :initarg :outputs :accessor outputs
	    :type tuple
	    :initform nil
	    :metadata (:sync t
                       :json-name "outputs"
                       :help "The output messages synced from the frontend.")))
  (:default-initargs
   :view-name (unicode "OutputView")
   :model-name (unicode "OutputModel")
   :view-module (unicode "@jupyter-widgets/output")
   :model-module (unicode "@jupyter-widgets/output")
   :view-module-version (unicode *jupyter-widgets-output-version*)
   :model-module-version (unicode *jupyter-widgets-output-version*)
   )
  (:metaclass traitlets:traitlet-class))

#+or
(defmethod %enter ((self output))
  (%flush self)
  (let ((ip (get-ipython)));FIXME: Does this function exist somewhere?
    (when (and (slot-exists-p ip 'kernel) (slot-exists-p (kernel ip) 'parent-header))
      (setf (msg-id self) ))))

#+or
(defmethod %exit ((self output) etype evalue tb)
  (let ((ip (get-ipython)))
    (%flush self)
    (setf (msg-id self) "")
    t))


(defmethod %flush ((self output))
  (finish-output)
  (values))

(defmethod %append-stream-output ((self output) text &key stream-name)
  "Append a stream output."
  (setf (outputs self) (append (outputs self)
                               (list (list (cons "output_type" "stream")
                                           (cons "name" stream-name)
                                           (cons "text" text))))))

(defmethod append-stdout ((self output) text)
  "append text to the stdout stream"
  (%append-stream-output self text :stream-name "stdout"))

(defmethod append-stderr ((self output) text)
  "append text to the stderr stream"
  (%append-stream-output self text :stream-name "stderr"))

(defmethod append-display-data ((self output) display-object)
  (error "Finish implementing me"))






(defmacro with-output (widget &body body)
  (let ((swidget (gensym "WIDGET")))
    `(let ((,swidget ,widget))
       (%enter ,swidget)
       (handler-case
	   (progn ,@body (%exit ,swidget))
	 (condition (c)
	   (%on-condition ,swidget c))))))



(in-package :cl-jupyter-widgets)

(defun unicode (string)
  (make-array (length string) :element-type 'character :initial-contents string))

(defun widget-to-json (x obj)
  (cond ((hash-table-p x)
	 (loop for key being the hash-keys of x
	    using (hash-value value)
	    collect (cons key (widget-to-json value obj))))
	((listp x)
	 (loop for (k . v) in x
	    collect (cons k (widget-to-json v obj))))
	((typep x 'widget)
	 (format nil "IPY_MODEL_~a" (model-id x)))
	(t x)))

(defun json-to-widget (x obj)
  (error "Implement json-to-widget as in https://github.com/drmeister/widget-dev/blob/master/ipywidgets/widgets/widget.py#L33"))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *widget-serialization*
    (list :from-json 'json-to-widget
	  :to-json 'widget-to-json)))
(deftype boolean () '(member :true :false :null))
(deftype unicode () '(simple-array character *))
(deftype cunicode () '(simple-array character *))
(deftype color () T)
(deftype instance () T)


(defclass callback-dispatcher ()
  ((%callbacks :initarg :callbacks :initform nil :accessor callbacks)))

(defmethod do-call ((self callback-dispatcher) &rest args)
  (let (value)
    (loop for cb in (callbacks self)
       do (handler-case (setf value (apply cb args))
	    (error (err)
	      (warn "Exception in callback ~a" cb))))
    value))

(defmethod register-callback ((self callback-dispatcher) callback remove)
  (if (and remove (find callback (callbacks self)))
      (setf (callbacks self) (delete callback (callbacks self)))
      (push callback (callbacks self))))




(defclass widget ()
  ((%widget-construction-callback
    :accessor widget-construction-callback
    :initform (make-instance 'callback-dispatcher))
   (%widgets :allocation :class :initform (make-hash-table) :accessor widgets)
   (%widget-types :allocation :class :initform (make-hash-table) :accessor widget-types)
   ;; Traits
   (%model-module :initarg :model-module :accessor model-module
		  :type unicode
		  :initform (unicode "jupyter-js-widgets")
		  :metadata (:json-name "_model_module"
					:sync t
					:help "A requirejs module name in which to find _model_name. If empty, look in the global registry."))
   (%model-name :initarg :model-name :accessor model-name
		:type unicode
		:initform (unicode "WidgetModel")
		:metadata (:sync t
				 :json-name "_model_name"))
   (%view-module :initarg :view-module :accessor view-module
		 :type (or null unicode)
		 :initform nil
		 :metadata (:sync t
				  :json-name "_view_module"))
   (%view-name :initarg :view-name :accessor view-name
	       :type (or null unicode)
	       :initform nil
	       :metadata
	       (:sync t
		      :json-name "_view_name"))
   (%msg-throttle :initarg :msg-throttle :accessor msg-throttle
		  :type integer
		  :initform 3
		  :metadata (:sync t
				   :json-name "msg_throttle") )
   (%comm :initarg :comm :accessor comm :initform nil)
   (%keys :initarg :keys :accessor keys)
   (%property-lock :initarg :property-lock :accessor property-lock)
   (%holding-sync :initarg :holding-sync :accessor holding-sync)
   (%states-to-send :initarg :states-to-send :accessor states-to-send)
   (%display-callbacks :initarg :display-callbacks :accessor display-callbacks
		       :initform (make-instance 'callback-dispatcher))
   (%msg-callbacks :initarg :msg-callbacks :accessor msg-callbacks
		   :initform (make-instance 'callback-dispatcher))
   (%model-id :initarg :model-id :accessor model-id :initform nil)
   )
  (:metaclass traitlets:traitlet-class))

;;; observe('comm')
(defmethod (setf comm) :after (value (widg widget))
  (setf (model-id widg) (comm-id value)))

(defmethod model-id ((widg widget))
  (if (comm widg)
      (comm-id (comm widg))))

(defun call-widget-constructed (w)
  (widget-log "call-widget-constructed widget -> ~a~%" w)
  (do-call (widget-construction-callback w) w))

(defmethod initialize-instance :around ((w widget) &rest initargs)
  (let ((w (call-next-method)))
    (call-widget-constructed w)
    (widget-open w)))

(defun widget-open (self)
  (multiple-value-bind (state buffer-keys buffers)
      (split-state-buffers self (get-state self))
    (widget-log "In widget-open~%")
    (widget-log "state -> ~a~%"
		(with-output-to-string (sout)
		  (print-as-python state sout)))
    (widget-log "buffer-keys -> ~s~%" buffer-keys)
    (widget-log "buffers -> ~s~%" buffers)
    (let ((kwargs (list :target-name "jupyter.widget"
			:data state)))
      (when (model-id self)
	(setf (getf kwargs :comm-id) (model-id self)))
      (setf (comm self) (apply #'comm.__init__ kwargs))
      (widget-log "    creating comm -> ~s~%" (comm self))
      (when buffers
	;; See comment about buffers at
	;; https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget.py#L205
	(send-state self)))))

(defun binary-types-p (obj)
  ;;; In python this test is
  ;; _binary_types = (memoryview, buffer)
  ;; isinstance(obj, _binary_types)
  nil)

(defun split-state-buffers (self state)
  (let (buffer-keys buffers new-state)
    (loop for (key . value) in state
       do (if (binary-types-p value)
	      (progn
		(push value buffers)
		(push key buffer-keys))
	      (push (cons key value) new-state)))
    (values new-state buffer-keys buffers)))

       
(defun send-state (self &key key)
  "From https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget.py#L252
   Sends the widget state, or a part of the widget state to the front-end.
*Arguments
key : a key or a list of keys (optional)
      A property's name or a list of property names to sync with the front-end"
  (let ((state (get-state self :key key)))
    (multiple-value-bind (state buffer-keys buffers)
	(split-state-buffers self state)
      (let ((msg (list (cons "method" "update")
		       (cons "state" state)
		       (cons "buffers" buffer-keys))))
	(widget-send* self msg :buffers buffers)))))

(defmethod widget-send (self content &key buffers)
  "Send a custom msg to the widget model in the front-end.
*Arguments
content : alist - Content of the message to send
buffers : list  - A list of binary buffers "
  (%send self (list (cons "method" "custom")
		    (cons "content" content))
	 :buffers buffers))

(defun %ipython-display (self &rest kwargs)
  (when (view-name self)
    (%send self '(("method" . "display")))
    (%handle-displayed self)))

(defmethod %handle-displayed ((self widget))
  (do-call (display-callbacks self) self))

    
(defun %send (self msg &key buffers)
  "Sends a message to the widget model in the front-end.
See: https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget.py#L485
Sends a message to the model in the front-end."
  (send (comm self) :data msg :buffers buffers))


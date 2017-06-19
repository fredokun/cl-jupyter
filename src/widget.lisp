(in-package :cl-jupyter-widgets)

(defparameter *Widget.widgets* (make-hash-table :test #'equal))

(defun unicode (&optional (string ""))
  (make-array (length string) :element-type 'character :initial-contents string))

(defun tuple (&rest args) (apply #'vector args))

(defun widget-to-json (x obj)
  (cond ((hash-table-p x)
	 (loop for key being the hash-keys of x
	    using (hash-value value)
	    collect (cons key (widget-to-json value obj))))
	((listp x)
	 (loop for (k . v) in x
	    collect (cons k (widget-to-json v obj))))
	((stringp x) x)
	((vectorp x)
	 (map 'vector (lambda (x) (widget-to-json x obj)) x))
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
(deftype tuple () '(vector * *))
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
   (%ipython-display :initarg :ipython-display :accessor ipython-display
		     :initform #'ipython-display-callback)
   )
  (:metaclass traitlets:traitlet-class))

;;; observe('comm')
(defmethod (setf comm) :after (comm (widg widget))
  (setf (model-id widg) (comm-id comm))
  (on-msg comm #'%handle-msg)
  (setf (gethash (model-id widg) *widget.widgets*) widg))

(defmethod model-id ((widg widget))
  (if (comm widg)
      (comm-id (comm widg))))

(defmethod widget-close ((self widget))
  (when (comm self)
    (remhash (model-id self) *Widget.widgets*)
    (close (comm self))
    (setf (comm self) nil)
    (setf (ipython-display self) nil)))

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

(defun disp (widget)
  (if (ipython-display widget)
      (funcall (ipython-display widget) widget)
      (warn "ipython-display callback is nil for widget ~a" widget)))

(defun ipython-display-callback (self &rest kwargs)
  "This is called to display the widget"
  (when (view-name self)
    (%send self '(("method" . "display")))
    (%handle-displayed self)))

(defun assoc-value (key-string alist &optional (default nil default-p))
  (let ((pair (assoc key-string alist :test #'string=)))
    (if pair
	(car pair)
	(if default-p
	    default
	    (error "Could not find key ~a in dict ~a" key-string alist)))))

  
(defmethod %handle-msg ((self widget) msg)
  (let* ((content (assoc-value "content" msg))
	 (data    (assoc-value "data" content))
	 (method  (assoc-value "method" data)))
    (cond
      ((string= method "backbone")
       (let ((sync-data (assoc-value "sync_data" data nil)))
	 (if (member "sync_data" data :key #'car :test #'string=)
	     ;; get binary buffers
	     ;; push them into sync-data as (buffer-key . buffer-value) pairs
	     (let ((sync-data (assoc-value "sync_data" data nil))
		   (buffers (assoc-value "buffers" msg)))
	       (when sync-data
		 (loop for buffer-key in buffer-keys
		    for index from 0
		    do (push (cons buffer-key (svref index buffers)) sync-data)))
	       ;; At this point sync-data should contain (buffer-key . buffer ) pairs
	       (set-state self sync-data)))))
      ((string= method "request_state")
       (send-state self))
      ((string= method "custom")
       (when (member "content" data :key #'car :test #'string=)
	 (handle-custom-msg self
			    (assoc-value "content" data)
			    (assoc-value "buffers" msg))))
      (t (cl-jupyter::log-error "Unknown front-end to back-end widget msg with method ~a" method)))))

(defmethod set-state ((self widget) sync-data)
  (error "Handle setting state of widgets using lock-property"))


#|

    # Event handlers
    @_show_traceback
    def _handle_msg(self, msg):
        """Called when a msg is received from the front-end"""
        data = msg['content']['data']
        method = data['method']

        # Handle backbone sync methods CREATE, PATCH, and UPDATE all in one.
        if method == 'backbone':
            if 'sync_data' in data:
                # get binary buffers too
                sync_data = data['sync_data']
                for i,k in enumerate(data.get('buffer_keys', [])):
                    sync_data[k] = msg['buffers'][i]
                self.set_state(sync_data) # handles all methods

        # Handle a state request.
        elif method == 'request_state':
            self.send_state()

        # Handle a custom msg from the front-end.
        elif method == 'custom':
            if 'content' in data:
                self._handle_custom_msg(data['content'], msg['buffers'])

        # Catch remainder.
        else:
            self.log.error('Unknown front-end to back-end widget msg with method "%s"' % method)


|#
(defmethod %handle-displayed ((self widget))
  (do-call (display-callbacks self) self))
    
(defun %send (self msg &key buffers)
  "Sends a message to the widget model in the front-end.
See: https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget.py#L485
Sends a message to the model in the front-end."
  (send (comm self) :data msg :buffers buffers))


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
(deftype bool () '(member :true :false :null))
(deftype unicode () '(simple-array character *))
(deftype cunicode () '(simple-array character *))
(deftype tuple () '(vector * *))
(deftype color () T)
(deftype instance () T)
(deftype dict () T) ;; alist?


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




(defclass widget (traitlets:synced-object)
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
   (%key-map :initarg :key-map :accessor key-map) ; alist of (slot-name . json-name)
   (%property-lock :initarg :property-lock :accessor property-lock :initform nil)
   (%holding-sync :initarg :holding-sync :accessor holding-sync :initform nil)
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
  (on-msg comm (lambda (msg) (funcall #'%handle-msg widg msg)))
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


(defun get-key-map (object)
  (loop for slot-def in (clos:class-slots (class-of object))
     when (eq (clos:slot-definition-allocation slot-def) :instance)
     when (getf (traitlets::metadata slot-def) :sync)
     collect (cons (clos:slot-definition-name slot-def)
		   (getf (traitlets::metadata slot-def) :json-name))))
    

(defmethod initialize-instance :around ((w widget) &rest initargs)
  (widget-log "widget.lisp initialize-instance initargs: ~a~%" initargs)
  (let* ((*send-updates* nil) ; suppress sending updates to browser when initializing
	 (w (call-next-method)))
    (setf (key-map w) (get-key-map w))
    (call-widget-constructed w)
    (widget-open w)))

(defvar *print-widget-backtrace* nil)

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
	(widget-log "widget.send-state msg -> ~a~%" msg)
	(%send self msg :buffers buffers)))))


(defmethod widget-send (self content &key buffers)
  "Send a custom msg to the widget model in the front-end.
*Arguments
content : alist - Content of the message to send
buffers : list  - A list of binary buffers "
  (%send self (list (cons "method" "custom")
		    (cons "content" content))
	 :buffers buffers))

(defun widget-display (widget)
  (if (ipython-display widget)
      (funcall (ipython-display widget) widget)
      (warn "ipython-display callback is nil for widget ~a" widget)))

(defun ipython-display-callback (self &rest kwargs)
  "This is called to display the widget"
  (when (view-name self)
    (%send self '(("method" . "display")))
    (%handle-displayed self)))

  
(defmethod %handle-msg ((self widget) msg)
  (widget-log "In %handle-msg~%")
  (let* ((content (extract-message-content msg))
	 (data    (assoc-value "data" content))
	 (method  (assoc-value "method" data)))
    (widget-log "      content -> ~s~%" content)
    (cond
      ((string= method "backbone")
       (widget-log "method backbone  data -> ~S~%" data)
       (if (member "sync_data" data :key #'car :test #'string=)
	   (let ((buffer-keys (assoc-value "buffer_keys" data #())))
	     (widget-log "found sync_data~%")
	     ;; get binary buffers
	     ;; push them into sync-data as (buffer-key . buffer-value) pairs
	     (let ((sync-data (assoc-value "sync_data" data nil))
		   (buffers (cl-jupyter:message-buffers msg)))
	       (widget-log "sync-data -> ~a~%" sync-data)
	       (widget-log "buffer-keys -> ~a~%" buffer-keys)
	       (when sync-data
		 (loop for buffer-key across buffer-keys
		    for index from 0
		    do (push (cons buffer-key (svref index buffers)) sync-data)))
	       ;; At this point sync-data should contain (buffer-key . buffer ) pairs
	       (widget-log "About to set-state with sync-data -> ~a~%" sync-data)
	       (set-state self sync-data)))
	   (widget-log "sync_data was not found in ~a" data)))
      ((string= method "request_state")
       (widget-log "method request_state~%")
       (send-state self))
      ((string= method "custom")
       (widget-log "method custom~%")
       (when (member "content" data :key #'car :test #'string=)
	 (handle-custom-msg self
			    (assoc-value "content" data)
			    (assoc-value "buffers" msg))))
      (t (widget-log "method unknown!!~%")
	 (log-error "Unknown front-end to back-end widget msg with method ~a" method)))))

(defun handle-custom-msg (widget content buffers)
  (error "handle-custom-msg"))

(defun slot-name-from-json-name (json-name widget-class)
  (let ((slots (clos:class-slots widget-class)))
    (loop for slot in slots
       for slot-definition-name = (clos:slot-definition-name slot)
       when (and slot-definition-name 
		 (string= (traitlets:traitlet-metadata widget-class slot-definition-name :json-name)
			  json-name))
       return slot-definition-name)))

(defmethod set-state ((widget widget) sync-data)
  (widget-log "set-state  sync-data -> ~s~%" sync-data)
  (unwind-protect
       ;; Create an alist of (slot-name . value) from sync-data and put it in lock-property
       (widget-log "calculating property-lock for sync-data -> ~s~%  key-map -> ~s" sync-data (key-map widget))
       (let ((plock (mapcar (lambda (pair)
			      (let ((found (rassoc (car pair) (key-map widget) :test #'string=)))
				(widget-log "      Searching for ~s   found -> ~s~%" (car pair) found)
				(or found (error "Could not find slot-name for ~s in ~s" (car pair) (key-map widget)))
				(cons (car found) (cdr pair))))
			    sync-data)))
	 (widget-log "property-lock -> ~s ~%" plock)
	 (setf (property-lock widget) plock)
	 (loop for (key . value) in sync-data
	    do (let ((slot-name (slot-name-from-json-name key (class-of widget))))
		 (setf (slot-value widget slot-name) value))))
    (setf (property-lock widget) nil)))

(defun get-state (object &key key)
  "Gets the widget state, or a piece of it.

        Parameters
        ----------
        key : unicode or iterable (optional)
            A single property's name or iterable of property names to get.

        Returns
        -------
        state : dict of states
        metadata : dict
            metadata for each field: {key: metadata}
        "
  (let ((keys (cond
		((null key) (mapcar #'car (key-map object)))
		((atom key) (check-type key symbol) (list key))
		((listp key) (mapc (lambda (x) (check-type x symbol))) key)
		(t (error "key must be a slot name, a list or NIL, key -> ~a" key))))
	state)
    (loop for slot-name in keys
       for slot-def = (or (find slot-name (clos:class-slots (class-of object))
				:key #'clos:slot-definition-name)
			  (error "Could not find slot-definition with name ~a" slot-name))
       for to-json = (or (traitlets:traitlet-metadata (class-of object) slot-name :to-json)
			 'widget-to-json)
       collect (cons (or (traitlets:traitlet-metadata (class-of object) slot-name :json-name)
			 (string (clos:slot-definition-name slot-def)))
		     (funcall to-json (widget-slot-value object slot-name) object)))))


(defmethod %should-send-property ((widget widget) key value)
  (check-type key symbol)
  (widget-log "%should-send-property key -> ~s (property-lock widget) -> ~s~%" key (property-lock widget))
  (let ((send (let* ((to-json (or (traitlets:traitlet-metadata (class-of widget) key :to-json)
				  'widget-to-json))
		     (lock-prop (assoc key (property-lock widget))))
		(if (and lock-prop
			 (equal (cdr lock-prop) (funcall to-json value widget)))
		    nil
		    (if (holding-sync widget)
			(progn
			  (push (states-to-send widget) key)
			  nil)
			t)))))
    (widget-log "      send -> ~s~%" send)
    send))

;;; This is different from the Python version because we don't
;;;   have the traitlet change[xxx] dictionary
;;;   Accept the slot-name and new value
(defmethod notify-change ((widget widget) slot-name &optional value)
  (check-type slot-name symbol)
  (when *send-updates*
    (when (and (comm widget) (assoc slot-name (key-map widget)))
      (when (%should-send-property widget slot-name value)
	(send-state widget :key slot-name)))))

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


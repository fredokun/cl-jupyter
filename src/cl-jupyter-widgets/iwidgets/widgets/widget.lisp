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
  (cond ((hash-table-p x)
	 (loop for key being the hash-keys of x
	    using (hash-value value)
	    collect (cons key (json-to-widget  value obj))))
	((listp x)
	 (loop for (k . v) in x
	    collect (cons k (widget-to-json v obj))))
	((and (stringp x)
	      (string= (subseq x 0 9) "IPY_MODEL_")
	      (string= (gethash '(subseq x 10) *Widget.widgets*)))
	 (gethash '(subseq x 10) *Widget.widgets))
	((vectorp x)
	 (map 'vector (lambda (x) (widget-to-json x obj)) x))
	(t x)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *widget-serialization*
    (list :from-json 'json-to-widget
	  :to-json 'widget-to-json)))


(deftype bool () '(member :true :false :null))
(deftype bytes () '(simple-array ext:byte8 *))
(deftype unicode () '(simple-array character *))
(deftype cunicode () '(simple-array character *))
(deftype tuple () '(vector * *))
(deftype color () T)
(deftype instance () T)
(deftype dict () T) ;; alist?

(deftype binary-types () '(or clasp-ffi:foreign-data (simple-array ext:byte8 *)))

(defclass instance-dict ()
  ())

(defun javascript-true-p (arg)
  (if (eq :true arg)
      t
      nil))

;;; See trait_types.py NumberFormat
(defun number-format (&key format)
  (unicode format))

(defun %put-buffers (state buffer-paths buffers)
    "The inverse of _remove_buffers, except here we modify the existing dict/lists.
    Modifying should be fine, since this is used when state comes from the wire.
    "
  (loop for buffer-path across buffer-paths
     for buffer across buffers
     for path-end = (path-end state buffer-path)
     do (setf (cdr path-end) buffer)))

(defun %separate-buffers (substate path buffer-paths buffers)
  "For internal, see %remove-buffers"
  ;; remove binary types from dicts and lists, but keep track of their paths
  ;; any part of the dict/list that needs modification will be cloned, so the original stays untouched
  ;; e.g. {'x': {'ar': ar}, 'y': [ar2, ar3]}, where ar/ar2/ar3 are binary types
  ;; will result in {'x': {}, 'y': [None, None]}, [ar, ar2, ar3], [['x', 'ar'], ['y', 0], ['y', 1]]
  ;; instead of removing elements from the list, this will make replacing the buffers on the js side much easier
  (cond
    ((typep substate 'vector)
     (let ((is-cloned nil))
       (loop for i from 0 below (length substate)
	  for v = (elt substate i)
	  do (if (typep v 'binary-types)
		 (progn
		   (when (not is-cloned)
		     (setf substate (copy-seq substate))
		     (setf is-cloned t))
		   (setf (elt substate i) nil)
		   (vector-push-extend v buffers)
		   (vector-push-extend (append path (list i)) buffer-paths))
		 (when (typep v '(or list vector))
		   (let ((vnew (%separate-buffers v (append path (list i)) buffer-paths buffers)))
		     (unless (eq v vnew)
		       (unless is-cloned
			 (setf substate (copy-seq substate))
			 (setf is-cloned t))
		       (setf (elt substate i) vnew))))))))
    ((typep substate 'list) ;; alist is dictionary
     (let ((is-cloned nil))
       (loop for (k . v) in substate
	  do (if (typep v 'binary-types)
		 (progn
		   (when (not is-cloned)
		     (setf substate (loop for (ks . vs) in substate
				       collect (cons ks vs)))
		     (setf is-cloned t))
		   (setf substate (loop for (ks . vs) in substate
				     unless (string= k ks)
				     collect (cons ks vs)))
		   (vector-push-extend v buffers)
		   (vector-push-extend (append path (list k)) buffer-paths))
		 (when (typep v '(or list vector))
		   (let ((vnew (%separate-buffers v (append path (list k)) buffer-paths buffers)))
		     (unless (eq v vnew)
		       (unless is-cloned
			 (setf substate (loop for (ks . vs) in substate
					   collect (cons ks vs)))
			 (setf is-cloned t))
		       (setf (cdr (assoc k substate)) vnew))))))))
    (t (error "expected state to be a vector or a dict, not ~a" substate))))

(defun %remove-buffers (state)
  "Return (state_without_buffers, buffer_paths, buffers) for binary message parts

    As an example:
    >>> state = {'plain': [0, 'text'], 'x': {'ar': memoryview(ar1)}, 'y': {'shape': (10,10), 'data': memoryview(ar2)}}
    >>> _remove_buffers(state)
    ({'plain': [0, 'text']}, {'x': {}, 'y': {'shape': (10, 10)}}, [['x', 'ar'], ['y', 'data']],
     [<memory at 0x107ffec48>, <memory at 0x107ffed08>])
  "
  (let ((buffer-paths (make-array 16 :fill-pointer 0 :adjustable t))
	(buffers (make-array 16 :fill-pointer 0 :adjustable t)))
    (let ((new-state (%separate-buffers state nil buffer-paths buffers)))
      (values state buffer-paths buffers))))

(defun %buffer-list-equal (buffera bufferb)
  "Compare two lists of buffers for equality.

Used to decide whether two sequences of buffers (binary-types)
differ, such that a sync is needed.
Return T if equal, NIL if unequal"
  (cond
    ((/= (length buffera) (length bufferb))
     nil)
    ((= (length buffera) (length bufferb) 0)
     t)
    ((eq buffera bufferb)
     t)
    (t (loop for ia across buffera
             for ib across bufferb
             when (not (equal ia ib))
                  do (return-from %buffer-list-equal nil))))
  t)
        
           
(defclass callback-dispatcher ()
  ((%callbacks :initarg :callbacks :initform nil :accessor callbacks)))

(defmethod do-call ((self callback-dispatcher) &rest args)
  (let (value)
    (loop for cb in (callbacks self)
       do (with-error-handling "In do-call"
	    (setf value (apply cb args))))
    value))

(defmethod register-callback ((self callback-dispatcher) callback &key remove)
  (if (and remove (find callback (callbacks self)))
      (setf (callbacks self) (delete callback (callbacks self)))
      (push callback (callbacks self))))

;;; --------------------------------------------------
;;;
;;; What am I going to do with WidgetRegistry??????
;;;
;;;


(defclass widget-registry ()
  ((%registry :initform (make-hash-table :test #'equal) :accessor registry)))

(defmacro set-default (key hash-table default)
  (let ((value (gensym))
	(foundp (gensym)))
    `(multiple-value-bind (,value ,foundp)
	 (gethash ,key ,hash-table)
       (if ,foundp
	   ,value
	   (setf (gethash ,key ,hash-table) ,default)))))

(defmethod widget-registry-register ((self widget-registry) model-module model-module-version-range model-name view-module view-module-version-range view-name klass)
  (let* ((model-module-ht  (set-default model-module               (registry self)  (make-hash-table :test #'equal)))
	 (model-version-ht (set-default model-module-version-range model-module-ht  (make-hash-table :test #'equal)))
	 (model-name-ht    (set-default model-name                 model-version-ht (make-hash-table :test #'equal)))
	 (view-module-ht   (set-default view-module                model-name-ht    (make-hash-table :test #'equal)))
	 (view-version-ht  (set-default view-module-version-range  view-module-ht   (make-hash-table :test #'equal))))
    (setf (gethash view-name view-version-ht) klass)))
		     
(defmethod widget-registry-get ((self widget-registry) model-module model-module-version model-name view-module view-module-version-range view-name)
  (let* ((module-versions-ht (gethash model-module (registry self)))
	 (model-names-ht (first (loop for value being the hash-values in module-versions-ht collect value)))
	 (view-modules-ht (gethash model-name model-names-ht))
	 (view-versions-ht (gethash view-module view-modules-ht))
	 (view-names-ht (first (loop for value being the hash-values in view-versions-ht collect value)))
	 (widget-class (gethash view-name view-names-ht)))
    widget-class))


(defmethod widget-registry-items ((self widget-registry))
  (error "Implement widget-registry-items"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *the-widget-registry* (make-instance 'widget-registry)))

(defun slot-default-value (widget-class slot-name)
  (let* ((class-slots (clos:class-slots widget-class))
	 (slot (find slot-name class-slots :key #'clos:slot-definition-name)))
    (if slot
	(funcall (clos:slot-definition-initfunction slot))
	(error "Could not find slot with name ~a in ~a" slot-name class-slots))))

(defun do-register (widget-class-name)
  (let ((widget-class (find-class widget-class-name)))
    (clos:finalize-inheritance widget-class)
    (widget-registry-register *the-widget-registry*
			      (slot-default-value widget-class '%model-module)
			      (slot-default-value widget-class '%model-module-version)
			      (slot-default-value widget-class '%model-name)
			      (slot-default-value widget-class '%view-module)
			      (slot-default-value widget-class '%view-module-version)
			      (slot-default-value widget-class '%view-name)
			      widget-class)
    widget-class))

(defmacro register (widget-class-name)
  "Register a widget class in the widget registry."
  `(do-register ',widget-class-name))


(defmacro defclass-widget-register (name &body body)
  `(defclass ,name ,@body)
  #|`(progn
     (defclass ,name ,@body)
     (register ,name))
|#
)

(defgeneric %ipython-display-callback (widget iopub parent-msg execution-count key))


(defclass widget (traitlets:synced-object)
  ((%widget-construction-callback
    :accessor widget-construction-callback
    :initform (make-instance 'callback-dispatcher))
   (%widgets :allocation :class :initform (make-hash-table) :accessor widgets)
   (%widget-types :allocation :class :initform *the-widget-registry* :accessor widget-types)
   ;; Traits
   (%model-name :initarg :model-name :accessor model-name
		:type unicode
		:initform (unicode "WidgetModel")
		:metadata (:sync t :json-name "_model_name"
				 :help "Name of the model." :read-only t))
   (%model-module :initarg :model-module :accessor model-module
		  :type unicode
		  :initform (unicode "@jupyter-widgets/base")
		  :metadata (:json-name "_model_module"
					:sync t
					:help "A requirejs module name in which to find _model_name. If empty, look in the global registry."))
   (%model-module-version :initarg :model-module-version :accessor model-module-version
			  :type unicode
			  :initform (unicode *jupyter-widgets-base-version*)
			  :metadata (:json-name "_model_module_version"
						:sync t
						:help "A semver requirement for namespace version containg the model"
						:read-only t))
   (%view-name :initarg :view-name :accessor view-name
	       :type (or null unicode)
	       :initform nil
	       :metadata
	       (:sync t
		      :json-name "_view_name"
		      :help "The name of the view"))
   (%view-module :initarg :view-module :accessor view-module
		 :type (or null unicode)
		 :initform nil
		 :metadata (:sync t :json-name "_view_module"))
   (%view-module-version :initarg :view-module-version :accessor view-module-version
			 :type unicode
			 :initform ""
			 :metadata (:sync t :json-name "_view_module_version"))
   (%view-count :initarg :view-count :accessor view-count
		:type integer
		:initform :null
		:metadata (:sync t :json-name "_view_count"
				 :help "EXPERIMENTAL: The number of views of the model displayed in the frontend. This attribute is experimental and may change or be removed in the future. None signifies that views will not be tracked. Set this to 0 to start tracking view creation/deletion."))
   #+(or)(%msg-throttle :initarg :msg-throttle :accessor msg-throttle
                        :type integer
                        :initform 3
                        :metadata (:sync t
				   :json-name "msg_throttle"))
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
   (%ipython-display :initarg :%ipython-display :accessor %ipython-display
		     :initform '%ipython-display-callback)
   )
  (:metaclass traitlets:traitlet-class))

;;; observe('comm')
(defmethod (setf comm) :after (comm (widg widget))
  (setf (model-id widg) (comm-id comm))
  (on-msg comm (lambda (msg) (funcall '%handle-msg widg msg)))
  (setf (gethash (model-id widg) *widget.widgets*) widg))

(defmethod model-id ((widg widget))
  (if (comm widg)
      (comm-id (comm widg))))

(defmethod widget-close ((self widget))
  (when (comm self)
    (remhash (model-id self) *Widget.widgets*)
    (close (comm self))
    (setf (comm self) nil)
    (setf (%ipython-display self) nil)))

(defun call-widget-constructed (w)
  (widget-log "call-widget-constructed widget -> ~a~%" w)
  (do-call (widget-construction-callback w) w))

(defun handle-comm-opened (comm msg)
  "Static method, called when a widget is constructed"
  (error "FINISH IMPLEMENTING ME"))

(defun keys (widget)
  (mapcar #'car (key-map widget)))

(defun get-key-map (object)
  (loop for slot-def in (clos:class-slots (class-of object))
     when (eq (clos:slot-definition-allocation slot-def) :instance)
     when (getf (traitlets::metadata slot-def) :sync)
     collect (cons (clos:slot-definition-name slot-def)
		   (getf (traitlets::metadata slot-def) :json-name))))
    

(defun check-*send-updates* ()
  (widget-log "In check-*send-updates* -> ~a~%" *send-updates*))

(defmethod initialize-instance :around ((w widget) &rest initargs)
  (widget-log "widget.lisp initialize-instance initargs: ~a~%" initargs)
  (unwind-protect
       (let* ((*send-updates* nil) ; suppress sending updates to browser when initializing
	      (w (progn
		   (check-*send-updates*)
		   (widget-log ">>>>   Suppressing sending updates to browser  *send-updates* -> ~a~%" *send-updates*)
		   (call-next-method))))
	 (setf (key-map w) (get-key-map w))
	 (call-widget-constructed w)
	 (prog1
	     (widget-open w)
	   (widget-log "widget.lisp initialize-instance done *send-updates* -> ~a~%" *send-updates*)))
    (widget-log "<<<< Leaving *send-updates* ~a context~%" *send-updates*)))

(defvar *print-widget-backtrace* nil)

(defun widget-open (self)
  (multiple-value-bind (state buffer-paths buffers)
      (%remove-buffers (get-state self))
    (widget-log "In widget-open~%")
    (widget-log "state -> ~a~%"
		(with-output-to-string (sout)
		  (print-as-python state sout :indent 4)))
    (widget-log "buffer-paths -> ~s~%" buffer-paths)
    (widget-log "buffers -> ~s~%" buffers)
    (let ((kwargs (list :target-name "jupyter.widget"
			:data (list (cons "state" state) (cons "buffer_paths" buffer-paths))
                        :buffers buffers
			:metadata (list (cons "version" *protocol-version*))
)))
      (when (model-id self)
	(setf (getf kwargs :comm-id) (model-id self)))
      (setf (comm self) (apply #'comm.__init__ kwargs))
      (widget-log "    creating comm -> ~s~%" (comm self)))))

(defun binary-types-p (obj)
  ;;; In python this test is
  ;; _binary_types = (memoryview, buffer)
  ;; isinstance(obj, _binary_types)
  (typep obj 'binary-types))

(defun send-state (self &key key)
  "From https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget.py#L252
   Sends the widget state, or a part of the widget state to the front-end.
*Arguments
key : a key or a list of keys (optional)
      A property's name or a list of property names to sync with the front-end"
  (let ((fstate (get-state self :key key)))
    (when (> (length fstate) 0)
      (multiple-value-bind (state buffer-paths buffers)
          (%remove-buffers fstate)
        (let ((msg (list (cons "method" "update")
                         (cons "state" state)
                         (cons "buffer_paths" buffer-paths))))
          (widget-log "widget.send-state~%")
          (%send self msg :buffers buffers))))))


(defmethod widget-send (self content &key (buffers #()))
  "Send a custom msg to the widget model in the front-end.
*Arguments
content : alist - Content of the message to send
buffers : list  - A list of binary buffers "
  (check-type buffers array)
  (%send self (list (cons "method" "custom")
		    (cons "content" content))
	 :buffers buffers))

(defun ipython-display (widget iopub parent-msg execution-count key)
  (widget-log "widget::ipython-display  (%ipython-display widget) -> ~a~%" (%ipython-display widget))
  (if (%ipython-display widget)
      (funcall (%ipython-display widget) widget iopub parent-msg execution-count key)
      (warn "%ipython-display callback is nil for widget ~a" widget)))


(defmethod %ipython-display-callback ((self widget) iopub parent-msg execution-count key)
  "This is called to display the widget"
  (widget-log "widget::%ipython-display-callback~%")
  (when (view-name self)
    ;; The 'application/vnd.jupyter.widget-view+json' mimetype has not been registered yet.
    ;; See the registration process and naming convention at
    ;; http://tools.ietf.org/html/rfc6838
    ;; and the currently registered mimetypes at
    ;; http://www.iana.org/assignments/media-types/media-types.xhtml.
    (let ((data (list (cons "text/plain" "A Jupyter Widget")
                      (cons "text/html" (%fallback-html self))
                      (cons "application/vnd.jupyter.widget-view+json"
                            (list (cons "version_major" 2)
                                  (cons "version_minor" 0)
                                  (cons "model_id" (model-id self)))))))
      (widget-log "Calling cl-jupyter:display with data -> ~s~%" data)
;;; Rather than mimicking 'display(data,raw=True) the way that ipywidgets does
;;;      as in https://github.com/jupyter-widgets/ipywidgets/blob/master/ipywidgets/widgets/widget.py#L698
;;; I am going to do what cl-jupyter:display would do - create a cl-jupyter::display-object
;;;     and then return that to the caller - that should publish it.
      #+(or)(cl-jupyter:display data #| raw=True ???? |#)
      (let ((display-obj (make-instance 'cl-jupyter::display-object :value self :data data)))
        (cl-jupyter:send-execute-raw-display-object iopub parent-msg execution-count display-obj :key key)) 
      (%handle-displayed self))))

  
(defmethod %handle-msg ((self widget) msg)
  (widget-log "In %handle-msg~%")
  (let* ((content (extract-message-content msg))
	 (data    ([] content "data"))
	 (method  ([] data "method"))
         state)
    (widget-log "      content -> ~s~%" content)
    (cond
      ((string= method "update")
       (widget-log "method update  data -> ~S~%" data)
       (when ([]-contains  data "state")
         (let ((state ([] data "state")))
           (widget-log "Found state ~s in data~%" state)
           (when ([]-contains data "buffer_paths")
             (let ((buffer-paths ([] data "buffer_paths" #())))
               (widget-log "Found buffer_paths ~s in data~%" buffer-paths)
               (%put-buffers state ([] data "buffer_paths")
                             ([] content "buffers" #()))))
           (set-state self state))))
      ((string= method "request_state")
       (widget-log "method request_state~%")
       (send-state self))
      ((string= method "custom")
       (widget-log "method custom   data -> ~s~%" data)
       (when ([]-contains data "content")
	 (widget-log "About to call handle-custom-msg~%")
	 (handle-custom-msg self
			    ([] data "content")
			    (cl-jupyter:message-buffers msg))))
      (t (widget-log "method unknown!!~%")
	 (log-error "Unknown front-end to back-end widget msg with method ~a" method)))))

(defun handle-custom-msg (widget content buffers)
  (widget-log "In handle-custom-msg   content -> ~s  buffers -> ~s~%" content buffers)
  (do-call (msg-callbacks widget) widget content buffers))

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
  (block %should-send-property
    (let ((send
            (let* ((to-json (or (traitlets:traitlet-metadata (class-of widget) key :to-json)
                                'widget-to-json))
                   (key-in-property-lock (assoc key (property-lock widget))))
              (when key-in-property-lock
                (multiple-value-bind (split-value-state split-value-buffer-paths split-value-buffers)
                    (%remove-buffers (list (cons key (funcall to-json value widget))))
                  (multiple-value-bind (split-lock-state split-lock-buffer-paths split-lock-buffers)
                      (%remove-buffers (list (cons key (cdr key-in-property-lock))))
                    (widget-log "testing (and (equal split-value-state split-lock-state)
                               (equal split-value-buffer-paths split-lock-buffer-paths)
                               (%buffer-list-equal split-value-buffers split-lock-buffers))")
                    (widget-log "split-value-state -> ~s~%" split-value-state)
                    (widget-log "split-lock-state -> ~s~%" split-lock-state)
                    (widget-log "split-value-buffer-paths -> ~s~%" split-value-buffer-paths)
                    (widget-log "split-lock-buffer-paths -> ~s~%" split-lock-buffer-paths)
                    (widget-log "(%buffer-list-equal split-value-buffers split-lock-buffers) -> ~s~%" (%buffer-list-equal split-value-buffers split-lock-buffers))
                    (when (and (equal split-value-state split-lock-state)
                               (equal split-value-buffer-paths split-lock-buffer-paths)
                               (%buffer-list-equal split-value-buffers split-lock-buffers))
                      (return-from %should-send-property nil)))))
              (if (holding-sync widget)
                  (progn
                    (push (states-to-send widget) key)
                    nil)
                  t))))
      (widget-log "      send -> ~s~%" send)
      send)))

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
  (widget-log "In %handle-displayed (display-callbacks self) -> ~a~%" (display-callbacks self))
  (do-call (display-callbacks self) self))
    
(defun %send (self msg &key (buffers #()))
  "Sends a message to the widget model in the front-end.
See: https://github.com/drmeister/spy-ipykernel/blob/master/ipywidgets/widgets/widget.py#L485
Sends a message to the model in the front-end."
  (check-type buffers array)
  (send (comm self) :data msg :buffers buffers))


(defmethod on-msg ((self widget) callback &key remove)
    "(Un)Register a custom msg receive callback.

        Parameters
        ----------
        callback: callable
            callback will be passed three arguments when a message arrives::

                callback(widget, content, buffers)

        remove: bool
            True if the callback should be unregistered."
  (register-callback (msg-callbacks self) callback :remove remove))

(defmethod on-displayed ((self widget) callback &key remove)
  "(Un)Register a widget displayed callback.
        Parameters
        ----------
        callback: method handler
            Must have a signature of::
                callback(widget, **kwargs)
            kwargs from display are passed through without modification.
        remove: bool
            True if the callback should be unregistered."
  (register-callback (display-callbacks self) callback :remove remove))


(defmethod %fallback-html ((self widget))
  "<p>Failed to display Jupyter Widget of type <code>{widget_type}</code>.</p>
<p>
  If you're reading this message in the Jupyter Notebook or JupyterLab Notebook, it may mean
  that the widgets JavaScript is still loading. If this message persists, it
  likely means that the widgets JavaScript library is either not installed or
  not enabled. See the <a href=\"https://ipywidgets.readthedocs.io/en/stable/user_install.html\">Jupyter
  Widgets Documentation</a> for setup instructions.
</p>
<p>
  If you're reading this message in another frontend (for example, a static
  rendering on GitHub or <a href=\"https://nbviewer.jupyter.org/\">NBViewer</a>),
  it may mean that your frontend doesn't currently support widgets.
</p>
")

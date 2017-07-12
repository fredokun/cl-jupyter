(in-package :nglv)


(defclass nglwidget (cljw::dom-widget)
  ((%image-data :initarg :image-data
		:type cljw:unicode
		:initform (cljw:unicode "")
		:metadata (:sync t :json-name "_image_data"))
   (%selector :initarg :selector
	      :accessor selector
	      :type cljw:Unicode
	      :initform (cljw:Unicode)
	      :metadata (:sync t :json-name "selector"))
   (%frame :initarg :frame
	   :accessor frame
	   :type Integer
	   :initform 0
	   :metadata (:sync t :json-name "frame"))
   (%count :initarg :count
	   :accessor count
	   :type Integer
	   :initform 1
	   :metadata (:sync t :json-name "count"))
   (%background :initarg :background
		:accessor background
		:type cljw:unicode
		:initform (cljw:Unicode "white")
		:metadata (:sync t :json-name "background"))
   (%loaded :initarg :loaded
	    :accessor loaded
	    :type boolean
	    :initform nil)
   (%picked :initarg :picked
	    :accessor picked
	    :type cljw:dict
	    :initform nil
	    :metadata (:sync t :json-name "picked"))
   (%n-components :initarg :n-components
	    :accessor n-components
	    :type integer
	    :initform 0
	    :metadata (:sync t :json-name "n_components"))
   (%orientation :initarg :orientation
		 :accessor orientation
		 :type list
		 :initform nil
		 :metadata (:sync t :json-name "orientation"))
   (%first-time-loaded :initarg :first-time-loaded
		       :accessor first-time-loaded
		       :type boolean
		       :initform t)
   ;; hack to always display movie
   (%n-dragged-files :initarg :n-dragged-files
		     :accessor n-dragged-files
		     :type integer
		     :initform 0
		     :metadata (:sync t :json-name "_n_dragged_files"))
   (%init-representations :initarg :init-representations
			  :accessor init-representations
			  :initform nil
			  :metadata (:sync t :json-name "_init_representations"))
   (%init-structures-sync :initarg :init-structures-sync
			  :accessor init-structures-sync
			  :type list
			  :initform nil
			  :metadata (:sync t :json-name "_init_structures_sync"))
   ;;TODO: remove %parameters?
   (%parameters :initarg :parameters
		:accessor parameters
		:type cljw:dict
		:initform nil) ;not synchronized
   (%full-stage-parameters :initarg :full-stage-parameters
			   :accessor full-stage-parameters
			   :type cljw:dict
			   :initform nil
			   :metadata (:sync t :json-name "_full_stage_parameters"))
   (%original-stage-parameters :initarg :original-stage-parameters
			       :accessor original-stage-parameters
			       :type cljw:dict
			       :initform nil
			       :metadata (:sync t :json-name "_original_stage_parameters"))
   ;; Not sync'd
   (%coordinates-dict :initarg :coordinates-dict
		      :accessor coordinates-dict
		      :type cljw:dict
		      :initform nil)
   (%camera-str :initarg :camera-str
		:type cunicode
		:initform (cljw:unicode "orthographic")
		:metadata (:sync t :json-name "_camera_str"
				 :caseless-str-enum ( "perspective" "orthographic")))
   (%repr-dict :initarg :repr-dict
	       :accessor repr-dict
	       :type cljw:dict
	       :initform nil)
   (%ngl-componend-ids :initarg :ngl-component-ids
		       :accessor ngl-component-ids
		       :type list
		       :initform nil)
   (%ngl-component-names :initarg :ngl-component-names
			 :accessor ngl-component-names
			 :type list
			 :initform nil)
   (%already-constructed :initarg :already-constructed
			 :accessor already-constructed
			 :type boolean
			 :initform nil)
   (%ngl-msg :initarg :ngl-msg
	     :accessor ngl-msg
	     :type (or string null)
	     :initform nil)
   (%send-binary :initarg :send-binary
		 :accessor send-binary
		 :type cljw:bool
		 :initform :true)
   (%init-gui :initarg :init-gui
	      :accessor init-gui
	      :type boolean
	      :initform nil)
   (%hold-image :initarg :hold-image
		:accessor hold-image
		:type cljw:bool
		:initform :false)
   ;; internal variables
   (%gui :initarg :%gui :accessor %gui :initform nil)
   (%init-gui :initarg :gui :accessor gui :initform nil) ;; WHY? does nglview does this
   (%theme :initarg :theme :accessor theme :initform "default")
   (%widget-image :initarg :widget-image :accessor widget-image
		  :initform (make-instance 'cl-jupyter-widgets:image :width 900))
   (%image-array :initarg :image-array :accessor image-array :initform nil)
   (%event :initarg :event :accessor event :initform (make-instance 'pythread:event))
   (%ngl-displayed-callbacks-before-loaded-reversed
    :initarg :ngl-displayed-callbacks-before-loaded-reversed
    :accessor ngl-displayed-callbacks-before-loaded-reversed
    :initform nil)
   (%ngl-displayed-callbacks-after-loaded-reversed
    :initarg :ngl-displayed-callbacks-after-loaded-reversed
    :accessor ngl-displayed-callbacks-after-loaded-reversed
    :initform nil)
   (%shape :initarg :shape :accessor shape
	   :initform (make-instance 'shape))
;;; FIXME:  Would this be a Clasp mp:PROCESS??
;;;   (%handle-msg-thread :initarg :handle-msg-thread :accessor handle-msg-thread :initform :threading.thread)
   #|
   self._handle_msg_thread = threading.Thread(target=self.on_msg,
   args=(self._ngl_handle_msg,))
   # # register to get data from JS side
   self._handle_msg_thread.daemon = True
   self._handle_msg_thread.start()
   |#
   (%remote-call-thread
    :initarg :remote-call-thread
    :accessor remote-call-thread
    :initform nil)
   (%remote-call-thread-queue
    :initarg :remote-call-thread-queue
    :accessor remote-call-thread-queue
    :initform (core:make-cxx-object 'mp:blocking-concurrent-queue))
   (%handle-msg-thread
    :accessor handle-msg-thread
    :initform nil)
   ;; keep track but making copy
   ;;; FIXME - fix this nonsense below
#||
        self._set_unsync_camera()
        self.selector = str(uuid.uuid4()).replace('-', '')
        self._remote_call('setSelector', target='Widget', args=[self.selector,])
        self.selector = '.' + self.selector # for PlaceProxy
        self._place_proxy = PlaceProxy(child=None, selector=self.selector)
        self.player = TrajectoryPlayer(self)
        self._already_constructed = True
   ||#
   (%trajlist :initform nil
	      :accessor %trajlist)
   (%init-structures :initarg :init-structures
		     :accessor init-structures
		     :initform nil)
   )
  (:default-initargs
   :view-name (cljw:unicode "NGLView")
   :view-module (cljw:unicode "nglview-js-widgets"))
  (:metaclass traitlets:traitlet-class))


(defun make-nglwidget (&rest kwargs-orig
		       &key (structure nil structure-p)
			 (representations nil representations-p)
			 (parameters nil parameters-p)
			 (gui nil gui-p)
			 (theme "default" theme-p)
		       &allow-other-keys)
  (cljw:widget-log "make-nglwidget~%")
  (let ((kwargs (copy-list kwargs-orig)))
    (when structure-p (remf kwargs :structure))
    (when representations-p (remf kwargs :representations))
    (when parameters-p (remf kwargs :parameters))
    (when gui-p (remf kwargs :gui))
    (when theme-p (remf kwargs :theme))
    (let ((widget (apply #'make-instance 'nglwidget kwargs)))
      (when representations
	(setf (%init-representations widget) representations))
      (when gui-p (setf (init-gui widget) gui))
      (when theme-p (setf (theme widget) theme))
      (warn "What do we do about add_repr_method_shortcut")
      (setf (handle-msg-thread widget)
	    (mp:process-run-function
	     'handle-msg-thread
	     (lambda ()
	       (cljw:on-msg widget #'%ngl-handle-msg)
	       (loop
		  ;; yield while respecting callbacks to ngl-handle-msg
		  (mp:process-yield)))))
      (when parameters-p (setf (parameters widget) parameters))
      (cond
	((typep structure 'Trajectory)
	 (warn "Handle trajectory"))
	((consp structure)
	 (warn "Handle list of structures"))
	(structure
	 (add-structure widget structure)))
      ;; call before setting representations
      (%set-initial-structure widget (init-structures widget))
      (when representations-p
	(if representations
	    (setf (init-representations widget) representations)
	    (setf (init-representations widget)
		  '((("type" ."cartoon")
		     ("params" . ( "sele" . "polymer")))
		    (("type" . "ball+stick")
		     ("params" . ( "sele" . "hetero OR mol")))
		    (("type" . "ball+stick")
		     ("params" . ( "sele" . "not protein and not nucleic")))))))
      (%set-unsync-camera widget)
      (setf (selector widget) (remove #\- (format nil "~W" (uuid:make-v4-uuid))))
      (%remote-call widget "setSelector" :target "Widget" :args (list (selector widget)))
      (setf (selector widget) (format nil ".~a" (selector widget)))
      (warn "How do I set %place-proxy with a PlaceProxy")
      (warn "How do I set the player")
      (setf (already-constructed widget) t)
      widget)))
  #|
   if isinstance(structure, Trajectory):
   name = py_utils.get_name(structure, kwargs)
   self.add_trajectory(structure, name=name)
   elif isinstance(structure, (list, tuple)):
   trajectories = structure
   for trajectory in trajectories:
   name = py_utils.get_name(trajectory, kwargs)
   self.add_trajectory(trajectory, name=name)
   else:
   if structure is not None:
   self.add_structure(structure, **kwargs)
   |#
#|   
   # call before setting representations
   self._set_initial_structure(self._init_structures)
   if representations:
   self._init_representations = representations
   else:
   self._init_representations = [
   {"type": "cartoon", "params": {
   "sele": "polymer"
   }},
   {"type": "ball+stick", "params": {
   "sele": "hetero OR mol"
   }},
   {"type": "ball+stick", "params": {
   "sele": "not protein and not nucleic"
   }}
   ]
   |#
  (defmethod %set-initial-structure ((widget nglwidget) structures)
  "initialize structures for Widget
        Parameters
        ----------
        structures : list
            list of Structure or Trajectory
  "
  (let ((init-structures-sync (if (consp structures)
				  structures
				  (list structures))))
    (when init-structures-sync
      (mapc (lambda (structure)
	      (setf (init-structures-sync widget)
		    (list (cons "data" (get-structure-string structure))
			  (cons "ext" (ext structure))
			  (cons "params" (params structure))
			  (cons "id" (id structure)))))
	    init-structures-sync))))




(defmethod %ngl-handle-msg ((widget nglwidget) msg)
      (cljw:widget-log  "What do I do with received msg: ~s~%" msg)
      (setf (ngl-msg widget) msg)
      (warn "%ngl-handle-msg received a message ~s  - what do I do with it?" msg))

(defmethod (setf clos:slot-value-using-class)
    (new-value (class traitlets:traitlet-class) (object nglwidget) (slotd traitlets:effective-traitlet))
  (call-next-method)
  (let ((slot-name (clos:slot-definition-name slotd)))
    (when (eq slot-name '%loaded)
      (when new-value
	(cljw:widget-log "%loaded is true - firing callbacks~%")
	(%fire-callbacks object (reverse (ngl-displayed-callbacks-before-loaded-reversed object)))))))

(defmethod %fire-callbacks ((widget nglwidget) callbacks)
  (loop for callback in callbacks
     do (progn
	  (funcall (car callback) widget)
	  (when (string= (cdr callback) "loadFile")
	    (%wait-until-finished widget)))))
    
(defmethod %wait-until-finished ((widget nglwidget) &optional (timeout 0.0001))
  (pythread:clear (event widget))
  (loop
     (sleep timeout)
     (when (pythread:is-set (event widget))
       (return-from %wait-until-finished))))





;;;Starting from the bottom down below. SCROLL!



(defmethod on-loaded ((self nglwidget))
  (setf (loaded self) t)
  (%fire-callbacks self (ngl-displayed-callbacks-before-loaded-reversed self))
  (values))

(defmethod %fire-callbacks ((self nglwidget) callbacks)
  (flet ((%call (event)
	   (loop for callback in callbacks
	      do
		(callback self)
		(if (string= (%method-name callback) "loadFile")
		    (%wait-until-finished self)))))
    (%run-on-another-thread self %call (event self)))
  (values))




(defmacro pop-from-alist (key alist)
  (let ((k (gensym "KEY")) (a (gensym "ALIST"))
	(pair (gensym "PAIR")))
    `(let* ((,k ,key)
	    (,a ,alist)
	    (,pair (assoc ,k ,a)))
       (when ,pair
	 (prog1 (cdr ,pair)
	 (setf ,alist (remove ,pair ,a)))))))
(defmacro pop-from-hash-table (key table)
  (let ((k (gensym "KEY")) (tab (gensym "TABLE")))
    `(let ((,k ,key) (,tab ,table))
       (prog1 (gethash ,k ,tab)
	 (remhash ,k ,tab)))))
#|
(defmethod %ngl-handle-msg ((self nglwidget) widget msg buffers)
  "store message sent from Javascript How? use view.on_msg(get_msg)"
  (setf (%ngl-msg self) msg)
  (let ((msg-type (cdr (assoc "type" (%ngl-msg self) :test #'string=))))
    (cond ((string= msg-type "request_frame")
	   (incf (frame self) (step (player self)))
	   (if (> (frame self) (count self))
	       (setf (frame self) 0)
	       (if (< (frame self) 0)
		   (setf (frame self) (- (count self) 1)))))
	  ((string= msg-type "repr_parameters")
	   (let* ((data-dict (cdr (assoc "data" (%ngl-msg self) :test #'string=)))
		  (name (concatenate 'string (pop-from-alist "name" data-dict) "\n"))
		  (selection (concatenate 'string (cdr (assoc "sele" data-dict :test #'string=)) "\n")))
					;FIXME: how to implement json.dumps
		    #| data_dict_json = json.dumps(data_dict).replace('true', 'True').replace('false', 'False')
		   data_dict_json = data_dict_json.replace('null', '"null"')|#
	     (if (widget_repr (player self))
		 (error "FIXME")
		 (error "FIXME"))))
		  ;FIXME: widget_utils methods??
	#| repr_name_text = widget_utils.get_widget_by_name(self.player.widget_repr, 'repr_name_text')
                repr_selection = widget_utils.get_widget_by_name(self.player.widget_repr, 'repr_selection')
                repr_name_text.value = name
		  repr_selection.value = selection|#
	  ((string= msg-type "request_loaded")
	   (if (not (loaded self))
	       (setf (loaded self) nil))
	   (setf (loaded self) (cdr (assoc "data" msg :test #'string=))))
	  ((string= msg-type "all_reprs_info")
	   (setf (%repr-dict self) (cdr (assoc "data" (%ngl-msg self) :test #'string=))))
	  ((string= msg-type "state_parameters")
	   (setf %full-stage-parameters (cdr (assoc "data" msg :test #'string=))))
	  ((string= msg-type "async_message")
	   (if (string= (cdr (assoc "data" msg :test #'string=)) "ok")
	       (error "FIXME"))))))
					;;;FIXME: self._event.set()

		

(defmethod %request-repr-parameters ((self nglwidget) &key (component 0) (repr-index 0))
  (%remote-call self
		"requestReprParameters"
		:target "Widget"
		:args (list component repr-index))
  (values))
|#
(defmethod add-structure ((self NGLWidget) structure &rest kwargs &key &allow-other-keys)
  (if (not (typep structure 'Structure))
      (error "~s is not an instance of Structure" structure))
  (if (or (loaded self) (already-constructed self))
      (apply #'%load-data self structure kwargs)
      (progn
	(setf (init-structures self) (append (init-structures self) (list structure)))
	(let ((name (apply #'get-name structure kwargs)))
	  (setf (ngl-component-names self) (append (ngl-component-names self) (list name))))))
  (setf (ngl-component-ids self) (append (ngl-component-ids self) (list (id structure))))
  (center-view self :component (- (length (ngl-component-ids self)) 1))
  (%update-component-auto-completion self))


(defmethod add-trajectory ((self NGLWidget) trajectory &rest kwargs &key &allow-other-keys)
  (let ((backends *BACKENDS*)
	(package-name nil))
    (error " I want package-name to be all the characters of trajector.--module-- up until the first period. I do not know how to do that")
    ))

(defmethod add-pdbid ((self NGLWidget) pdbid)
  (error " I want something like thif but what is .format(pdbid)??(add-component self rcsb://{}.pdb.format(pdbid)"))

(defmethod add-component ((self NGLWidget) filename &rest kwargs &key &allow-other-keys)
  (apply #'%load-data self filename kwargs)
  (append (%ngl-component-ids self) (list (uuid:make-v4-uuid)))
  (%update-component-auto-completion self))

(defmethod %load-data ((self NGLWidget) obj &rest kwargs &key &allow-other-keys)
  (let ((kwargs2 (%camelize-dict kwargs)))
    (error "Help %load-data in widget.lisp")))
#|    def _load_data(self, obj, **kwargs):
'''

Parameters
----------
obj : nglview.Structure or any object having 'get-structure-string' method or
string buffer (open(fn).read())
'''
kwargs2 = _camelize_dict(kwargs)

try:
is_url = FileManager(obj).is_url
except NameError:
is_url = False

if 'defaultRepresentation' not in kwargs2:
kwargs2['defaultRepresentation'] = True

if not is_url:
if hasattr(obj, 'get-structure-string'):
blob = obj.get-structure-string()
kwargs2['ext'] = obj.ext
passing_buffer = True
binary = False
else:
fh = FileManager(obj,
ext=kwargs.get('ext'),
compressed=kwargs.get('compressed'))
# assume passing string
blob = fh.read()
passing_buffer = not fh.use_filename

if fh.ext is None and passing_buffer:
raise ValueError('must provide extension')

kwargs2['ext'] = fh.ext
binary = fh.is_binary
use_filename = fh.use_filename

if binary and not use_filename:
# send base64
blob = base64.b64encode(blob).decode('utf8')
blob_type = 'blob' if passing_buffer else 'path'
args=[{'type': blob_type, 'data': blob, 'binary': binary}]
else:
# is_url
blob_type = 'url'
url = obj
args=[{'type': blob_type, 'data': url, 'binary': False}]

name = py_utils.get_name(obj, kwargs2)
self._ngl_component_names.append(name)
self._remote_call("loadFile",
target='Stage',
args=args,
kwargs=kwargs2)
|#

(defmethod remove-component ((self NGLWidget) component-id)
  (%clear-component-auto-completion self)
  (if (%trajlist self)
      (loop for traj in (%trajlist self)
	 do (if (equal (id traj) component-id)
		(remove traj (%trajlist self) :test #'equal))))
  (let ((component-index (aref (%ngl-component-ids self) component-id)))
    (remove component-id (%ngl-component-ids self) :test #'equal)
    (remove component-index (%ngl-component-names))
    (error "Should that have been pop not remove???")
    (%remote-call self
		  "removeComponent"
		  :target "Stage"
		  :args (list component-index))))

(defmethod %remote-call ((self NGLWidget) method-name &key (target "Widget") args kwargs)
  "call NGL's methods from Common Lisp
        
        Parameters
        ----------
        method_name : str
        target : str, (member \"Stage\" \"Viewer\" \"compList\" \"StructureComponent\")
        args : list
        kwargs : alist
            if target is \"compList\", \"component_index\" could be passed
            to specify which component will call the method.

        Examples
        --------
        (%remote-call view \"loadFile\" :args '(\"1L2Y.pdb\")
                          :target \"Stage\" :kwargs '((\"defaultRepresentation\" . :true)))

        # perform centerView for 1-th component
        # component = Stage.compList[1];
        # component.centerView(true, \"1-12\");
        (%remote-call view \"centerView\"
                          :target \"component\"
                          :args (list :true, \"1-12\" )
                          :kwargs '((\"component_index\" . 1)))
        "
  (let (msg)
    (let ((component-index (assoc "component_index" kwargs :test #'string=)))
      (when component-index
	(push component-index msg)
	(setf kwargs (remove component-index kwargs))))
    (let ((repr-index (assoc "repr_index" kwargs :test #'string=)))
      (when repr-index
	(push repr-index msg)
	(setf kwargs (remove repr-index kwargs))))
    (push (cons "target" target) msg)
    (push (cons "type" "call_method") msg)
    (push (cons "methodName" method-name) msg)
    (push (cons "args" args) msg)
    (push (cons "kwargs" kwargs) msg)
    (let ((callback (cons (lambda (self) (cljw:widget-send self msg)) method-name)))
      (if (loaded self)
	  (mp:queue-enqueue (remote-call-thread-queue self) callback)
	  (push callback (ngl-displayed-callbacks-before-loaded-reversed self)))
      (push callback (ngl-displayed-callbacks-after-loaded-reversed self)))))

(defmethod %get-traj-by-id ((self NGLWidget) itsid)
  (loop for traj in (%trajlist self)
     do
       (if (equal (id traj) itsid)
	   (return traj)))
  nil)

#+(or)
(defmethod hide ((self NGLWidget) indices)
  (let ((traj-ids (loop for traj in (%trajlist self) collect (id traj)))
	(comp-id nil)
	(traj nil))
    (loop for index in indices
       do
	 (setf comp-id (aref (%ngl-component-ids self) index))
	 (if t
	     (progn
	       (error "the above line is wrong. Should be 'if comp-id in traj-ids'")
	       (setf traj (%get-traj-by-id self comp-id)
		     (shown traj nil))))
	 (%remote-call self
		       "setVisibility"
		       :target "compList"
		       :args '(nil)
		       :kwargs (list (cons "component_index" index)))))
  (values))

(defmethod show ((self NGLWidget) &rest kwargs &key &allow-other-keys)
  (apply #'show-only self kwargs)
  (values))

(defmethod show-only ((self NGLWidget) &key (indices "all"))
  (let ((traj-ids (loop for traj in (%trajlist self) collect (id traj)))
	(indices% "")
	(index 0)
	(traj nil)
	(args '(nil)))
    (setf traj-ids (remove-duplicates traj-ids :test #'equal))
    (if (string= indices "all")
	(setf indices% (loop for i from 0 below (n-components self) collect i))
	(progn
	  (setf indices% (loop for index in indices collect index)
		indices% (remove-duplicates indices% :test #'equal))))
    (loop for comp-id in (%ngl-component-ids self)
       do
	 (if t
	     (progn
	       (error "the line above is wrong and should be 'if comp-id in traj-ids")
	       (setf traj (%get-traj-by-id self comp-id)))
	     (setf traj nil))
	 (if t
	     (progn
	       (error "the line above is wrong and should be 'if index in indices%")
	       (setf args '(t))
	       (if traj
		   (setf (shown traj) t)))
	     (progn
	       (setf args '(nil))
	       (if traj
		   (setf (shown traj) nil))))
	 (%remote-call self
		       "setVisiblity"
		       :target "compList"
		       :args args
		       :kwargs (list (cons "component_index" index)))))
  (error "Figure out 'if index in indices%' in show-only in  widget.lisp")
  (values))



	     
	     
   
    

(defmethod %js-console ((self NGLWidget))
  (error "implement %js-console in widget.lisp"))

#|
def _js_console(self):
self.send(dict(type='get', data='any'))
|#

(defmethod %get-full-params ((self NGLWidget))
  (error "Implement %get-full-params in widget.lisp"))
#|
def _get_full_params(self):
self.send(dict(type='get', data='parameters'))
|#

(defmethod %display-image ((self NGLWidget))
  (error "help %display-image widget.lisp"))
#|
def _display_image(self):
'''for testing
'''
from IPython import display
return display.Image(self._image_data)
|#

(defmethod %clear-component-auto-completion ((self NGLWidget))
  (let ((index 0))
    (loop for id in (%ngl-component-ids self)
       do
	 (let ((name (concatenate 'string "component_" (write-to-string index))))
	   (incf index)
	   (error "WE NEED A DELATTR IN %clear-component-auto-completion in widget.lisp")))))
#|
def _clear_component_auto_completion(self):
for index, _ in enumerate(self._ngl_component_ids):
name = 'component_' + str(index)
delattr(self, name)
|#

(defmethod %update-component-auto-completion ((self NGLWidget))
  (warn "Do something for %update-component-auto-completion")
 #+(or) (let ((trajids (loop for traj in (%trajlist self) collect (id traj)))
	(index 0))
    (loop for cid in (%ngl-component-ids self)
       do
	 (let ((comp (make-instance 'ComponentViewer :%view self :%index index))
	       (name (concatenate 'string "component_" (write-to-string index))))
	   (incf index)
	   (error "We need a setattr function!!!!")
	   (error "we need an in function! Maybe we have one. %update-component-auto-completion in widget.lisp")))))
#|
def _update_component_auto_completion(self):
trajids = [traj.id for traj in self._trajlist]

for index, cid in enumerate(self._ngl_component_ids):
comp = ComponentViewer(self, index) 
name = 'component_' + str(index)
setattr(self, name, comp)

if cid in trajids:
traj_name = 'trajectory_' + str(trajids.index(cid))
setattr(self, traj_name, comp)
|#

(defmethod %-getitem-- ((self NGLWidget) index)
  "return ComponentViewer"
  (let ((positive-index (get-positive-index py-utils index (length (%ngl-component-ids self)))))
    (make-instance 'ComponentViewer :%view self :%index positive-index))
  (error "Help! We don't have a py-utils thingy in %-getitem-- in widget.lisp"))


(defmethod %-iter-- ((self NGLWidget))
  "return ComponentViewer"
  (let ((index 0))
    (loop for item in (%ngl-component-ids self))
    (error "Implementer %-iter-- in widget.lisp")))
#|
def __iter__(self):
"""return ComponentViewer
        """
for i, _ in enumerate(self._ngl_component_ids):
yield self[i]
|#	 

(defmethod detach ((self NGLWidget) &key (split nil))
  "detach player from its original container."
  (if (not (loaded self))
      (error "must display view first"))
  (if split
      (%move-notebook-to-the-right js-utils))
  (%remote-call self "setDialog" :target "Widget"))

(defclass ComponentViewer ()
  ((%view :initarg :%view :accessor %view
	 :initform nil)
   (%index :initarg :%index :accessor %index
	  :initform nil)))

#+(or)
(defmethod initialize-instance :after ((self ComponentViewer))
  (%add-repr-method-shortcut self (%view self))
  (%borrow-attribute self (%view self) (list "clear_representations"
					     "_remove_representations_by_name"
					     "_update_representations_by_name"
					     "center_view"
					     "center"
					     "clear"
					     "set_representations")
		     (list "get-structure-string"
			   "get_coodinates"
			   "n_frames")))

(defmethod id ((self ComponentViewer))
  (aref (%ngl-component-ids (%view self)) (%index self)))

(defmethod hide ((self ComponentViewer))
  "set invisibility for given components (by their indices)"
  (%remote-call (%view self)
		"setVisibility"
		:target "compList"
		:args (list nil)
		:kwargs (list (cons "component_index" (%index self))))
  (let ((traj (%get-traj-by-id (%view self) (id self))))
    (if traj
	(setf (shown traj) nil))
    (values)))

#+(or)
(defmethod show ((self ComponentViewer))
  "set invisibility for given components (by their indices)"
  (%remote-call (%view self)
		"setVisiblity"
		:target "compList"
		:args (list t)
		:kwargs (list (cons "component_index" (%index self))))
  (let ((traj (%get-traj-by-id (%view self) (id self))))
    (if traj
	(setf (shown traj) t))
    (values)))

#+(or)
(defmethod show ((self ComponentViewer) repr-type &optional (selection "all") &rest kwargs &key &allow-other-keys)
  (setf (aref kwargs "component") (%index self))
  (add-representation (%view self) :repr-type repr-type :selection selection kwargs))

#+(or)
(defmethod %borrow-attribute ((self ComponentViewer) view attributes &key (trajectory-atts nil))
  (let ((traj (%get-traj-by-id view (id self))))
    (loop for attname in attributes
       do
	 (let ((view-att nil)))))
  (error "Help me!!!"))

(defmethod cl-jupyter-widgets:widget-close ((widget nglwidget))
  (call-next-method)
  (mp:process-kill (remote-call-thread widget))
  (mp:process-kill (handle-msg-thread-widget))
  ;;; FIXME: Kill handle-msg-thread 
  )



#||
    def display(self, gui=False, use_box=False):
        if gui:
            if use_box:
                from nglview.widget_box import BoxNGL
                box = BoxNGL([self, self.player._display()])
                box._gui_style = 'row'
                return box
            else:
                display(self)
                display(self.player._display())
                display(self._place_proxy)
                return None
        else:
            return self
||#

(defmethod %update-component-auto-completions ((widget nglwidget))
  (warn "What do I do in %update-component-auto-completions?"))


(defmethod center-view ((widget nglwidget) &key (zoom t) (selection "*") (component 0))
  "center view for given atom selection
        Examples
        --------
        view.center_view(selection='1-4')
  "
  (%remote-call widget "centerView"
		:target "compList"
		:args (list zoom selection)
		:kwargs (list (cons "component_index" component))))

(defmethod %set-draggable ((self nglwidget) &key (yes t))
  (if yes
      (%remote-call self "setDraggable"
		    :target "Widget"
		    :args '(""))
      (%remote-call self "setDraggable"
		    :target "Widget"
		    :args '("destroy"))))

(defmethod %set-sync-frame ((self nglwidget))
  (%remote-call self "setSyncFrame" :target "Widget"))

(defmethod %set-unsync-frame ((self nglwidget))
  (%remote-call self "setUnSyncFrame" :target "Widget"))

(defmethod %set-sync-camera ((self nglwidget))
  (%remote-call self "setSyncCamera" :target "Widget"))

(defmethod %set-unsync-camera ((self nglwidget))
  (%remote-call self "setUnSyncCamera" :target "Widget"))

(defmethod %set-delay ((self nglwidget) delay)
  "unit of millisecond
  "
  (%remote-call self "setDelay" :target "Widget"
		:args (list delay)))

(defmethod %set-spin ((self nglwidget) axis angle)
  (%remote-call self "setSpin"
		:target "Stage"
		:args (list axis angle)))

(defmethod %set-selection ((self nglwidget) &key selection (component 0) (repr-index 0))
  (%remote-call self "setSelection"
		:target "Representation"
		:args (list selection)
		:kwargs (list (cons "component_index" component)
			      (cons "repr_index" repr-index))))

(defmethod %set-color-by-residue ((self nglwidget) &key colors (component-index 0) (repr-index 0))
  (%remote-call self "setColorByResidue"
		:target "Widget"
		:args (list colors component-index repr-index)))

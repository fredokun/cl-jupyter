(in-package :nglv)


(defun remote-call-thread-run (widget)
  "Keep pulling callbacks out of the queue and evaluating them"
  (loop
       (multiple-value-bind (callback found)
	   (mp:queue-wait-dequeue-timed (remote-call-thread-queue widget) 1000)
	 (when found
	   (funcall (car callback) widget)
	 (when (string= (cdr callback) "loadFile")
	   (%wait-until-finished widget))))))



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
   ;; hack to always display movie
   (%n-dragged-files :initarg :n-dragged-files
		     :accessor n-dragged-files
		     :type integer
		     :initform 0
		     :metadata (:sync t :json-name "_n_dragged_files"))
   (%init-structures-sync :initarg :init-structures-sync
			  :accessor init-structures-sync
			  :type list
			  :initform nil
			  :metadata (:sync t :json-name "_init_structures_sync"))
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
   (%parameters :initarg :parameters
		:accessor parameters
		:type cljw:dict
		:initform nil) ; not synchronized https://github.com/drmeister/spy-ipykernel/blob/master/nglview/widget.py#L124
   (%ngl-component-names :initarg :ngl-component-names
			 :accessor ngl-component-names
			 :type list
			 :initform nil)
   (%send-binary :initarg :send-binary
		 :accessor send-binary
		 :type cljw:bool
		 :initform :true)
   (%init-gui :initarg :init-gui
	      :accessor init-gui
	      :type cljw:bool
	      :initform :false)
   (%hold-image :initarg :hold-image
		:accessor hold-image
		:type cljw:bool
		:initform :false)
   (%ngl-msg :initarg :ngl-msg
	     :accessor ngl-msg
	     :type (or string null)
	     :initform nil)
   ;; internal variables
   (%gui :initarg :%gui :accessor %gui :initform nil)
   (%init-gui :initarg :gui :accessor gui :initform nil) ;; WHY? does nglview does this
   (%theme :initarg :theme :accessor theme :initform "default")
   (%widget-image :initarg :widget-image :accessor widget-image
		  :initform (make-instance 'cl-jupyter-widgets:image))
   (%image-array :initarg :image-array :accessor image-array :initform nil)
;;; FIXME:  I don't know how to translate the creation of a threading.Event() from python
;;;         into Common Lisp
   ;;   (%event :initarg :event :accessor event :initform :threading.event.object)
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
   (%structure :initarg :structure :accessor structure)
   (%init-representations :initarg :init-representations
			  :accessor init-representations
			  :initform '((("type" ."cartoon")
				       ("params" . ( "sele" . "polymer")))
				      (("type" . "ball+stick")
				       ("params" . ( "sele" . "hetero OR mol")))
				      (("type" . "ball+stick")
				       ("params" . ( "sele" . "not protein and not nucleic")))))
   (%representations :initarg :representations :accessor representations
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
   )
  (:default-initargs
   :view-name (cljw:unicode "NGLView")
   :view-module (cljw:unicode "nglview-js-widgets"))
  (:metaclass traitlets:traitlet-class))


(defmethod initialize-instance :around ((instance nglwidget)  &rest initargs &key &allow-other-keys)
  (call-next-method)
  ;;    (%add-repr-method-shortcut instance instance)
  (when (representations instance)
    (setf (%init-representations instance) (representations instance)))
  (%set-unsync-camera instance)
  (setf (remote-call-thread instance)
	(mp:process-run-function 'remote-call-thread (lambda () (remote-call-thread-run instance))))
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
  )






;;;Starting from the bottom down below. SCROLL!




(defmethod add_structure ((self NGLWidget) structure &rest kwargs &key &allow-other-keys)
  (if (not (typep structure 'Structure))
      (error "{} is not an instance of Structure"))
  (if (or (loaded self) (_already_constructed self))
      (apply #'_load_data self structure kwargs)
      (progn
	(append (_init_structures self) (list structure))
	(let ((name (apply #'get_name py_utils structure kwargs)))
	  (error "name can't be right! how do py_utils.get_name")
	  (append (_ngl_component_names self) (list name)))))
  (append (_ngl_component_ids) (list (id structure)))
  (center_view self :component (- (length (_ngl_component_ids self)) 1))
  (_update_component_auto_completion self))

(defmethod add_trajectory ((self NGLWidget) trajectory &rest kwargs &key &allow-other-keys)
  (let ((backends *BACKENDS*)
	(package_name nil))
    (error " I want package_name to be all the characters of trajector.__module__ up until the first period. I do notttt know how to do that")
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
    (let ((callback (cons (lambda () (cljw:widget-send self msg)) method-name)))
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
  (let ((trajids (loop for traj in (%trajlist self) collect (id traj)))
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


(in-package :nglv)

(defclass nglwidget (cljw:domwidget)
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
	   :initform 1
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
	    :type cljw:bool
	    :initform :false
	    :metadata (:sync t :json-name "loaded"))
   (%picked :initarg :picked
	    :accessor picked
	    :type cljw:dict
	    :initform nil
	    :metadata (:sync t :json-name "picked"))
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
   (%parameters :initarg :parameters
		:accessor parameters
		:type cljw:dict
		:initform nil) ; not synchronized https://github.com/drmeister/spy-ipykernel/blob/master/nglview/widget.py#L124
   (%full-stage-parameters :initarg :full-stage-parameters
			   :accessor full-stage-parameters
			   :type cljw:dict
			   :initform nil
			   :metadata (:sync t :json-name "_full_stage_parameters"))
   ;; Not sync'd
   (%coordinates-dict :initarg :coordinates-dict
		      :accessor coordinates-dict
		      :type cljw:dict
		      :initform nil)
   (%repr-dict :initarg :repr-dict
	       :accessor repr-dict
	       :type cljw:dict
	       :initform nil)
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
   (%init-gui :initarg :gui :accessor gui :initform nil)  ;; WHY? does nglview does this
   (%theme :initarg :theme :accessor theme :initform "default")
   (%widget-image :initarg :widget-image :accessor widget-image
		  :initform (make-instance 'cl-jupyter-widgets:image))
   (%image-array :initarg :image-array :accessor image-array :initform nil)
   ;;; FIXME:  I don't know how to translate the creation of a threading.Event() from python
   ;;;         into Common Lisp
;;   (%event :initarg :event :accessor event :initform :threading.event.object)
   (%ngl-displayed-callbacks-before-loaded :initarg :ngl-displayed-callbacks-before-loaded
					   :accessor ngl-displayed-callbacks-before-loaded
					   :initform nil)
   (%ngl-displayed-callbacks-after-loaded :initarg :ngl-displayed-callbacks-after-loaded
					   :accessor ngl-displayed-callbacks-after-loaded
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
        self._remote_call_thread = RemoteCallThread(self)
        self._remote_call_thread.start()
|#   
   (%parameters :initarg :parameters
		:accessor parameters)
   (%structure :initarg :structure :accessor structure)
   (%init-representations :initarg :init-representations
			  :accessor init-representations
			  :initform '((("type" ."cartoon")
				       ("params" . ( "sele" . "polymer")))
				      (("type" . "ball+stick")
				       ("params" . ( "sele" . "hetero OR mol")))
				      (("type" . "ball+stick")
				       ("params" . ( "sele" . "not protein and not nucleic")))))
   ;; keep track but making copy
   ;;; FIXME - fix this nonsense below
#||
   (%representations :initarg :representations :accessor representations)
        self._set_unsync_camera()
        self.selector = str(uuid.uuid4()).replace('-', '')
        self._remote_call('setSelector', target='Widget', args=[self.selector,])
        self.selector = '.' + self.selector # for PlaceProxy
        self._place_proxy = PlaceProxy(child=None, selector=self.selector)
        self.player = TrajectoryPlayer(self)
        self._already_constructed = True
||#
   )
  (:default-initargs
   :view-name (cljw:unicode "NGLView")
   :view-module (cljw:unicode "nglview-js-widgets"))
  (:metaclass traitlets:traitlet-class))


#||  Down to the bottom
    
(defmethod initialize-instance ((instance nglview)  &rest initargs &key &allow-other-keys)
  (%add-repr-method-shortcut instance instance)
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




#|
(defmethod add_trajectory ((self NGLWidget) trajectory &rest kwargs &key &allow-other-keys)
  (let ((backends *BACKENDS*)
	(package_name 
	 |#

(defmethod add_pdbid ((self NGLWidget pdbid))
  (error " I want something like thif but what is .format(pdbid)??(add_component self rcsb://{}.pdb.format(pdbid)"))

(defmethod add_component ((self NGLWidget) filename &rest kwargs &key &allow-other-keys)
  (apply #'_load_data self filename kwargs)
  (append (_ngl_component_ids self) (list (uuid:make-v4-uuid)))
  (_update_component_auto_completion self))

(defmethod _load_data ((self NGLWidget) obj &rest kwargs &key &allow-other-keys)
  (let ((kwargs2 (_camelize_dict kwargs)))
    (error "Help _load_data in widget.lisp")))
#|    def _load_data(self, obj, **kwargs):
        '''

        Parameters
        ----------
        obj : nglview.Structure or any object having 'get_structure_string' method or
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
            if hasattr(obj, 'get_structure_string'):
                blob = obj.get_structure_string()
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

(defmethod remove_component ((self NGLWidget) component_id)
  (_clear_component_auto_completion self)
  (if (_trajlist self)
      (loop for traj in (_trajlist self)
	   (if (equal (id traj) component_id)
	       (remove traj (_trajlist self) :test #'equal))))
  (let ((component_index (aref (_ngl_component_ids self) component_id)))
    (remove component_id (_ngl_component_ids self) :test #'equal)
    (remove component_index (_ngl_component_names))
    (error "Should that have been pop not remove???")
    (_remote_call self
		  "removeComponent"
		  :target "Stage"
		  :args (list component_index))))

(defmethod _remote_call ((self NGLWidget) method_name &optional (target "Widget") (args ()) &rest (kwargs nil) &key &allow-other-keys)
  (let ((msg nil))
    (if t
	(progn
	  (error "FIX THE LINE ABOVE. Should resemble 'if 'component_index' in kwargs")
	  (error "FIX THIS LINE. Should resemble 'msg['component_index'] = kwargs.pop('component_index')")))
    (if t
	(progn
	  (error "FIX THE LINE ABOVE. Should resemble 'if 'repr_index' in kwargs")
	  (error "FIX THIS LINE. Should resemble 'msg['repr_index'] = kwargs.pop('repr_index')")))
    (setf (aref msg "target") target
	  (aref msg "type") "call_method"
	  (aref msg "methodName") method_name
	  (aref msg "args") args
	  (aref msg "kwargs") kwargs)
    (flet ((callback (widget &key (msg msg))
	     (send widget msg)
	     (values)))
      (setf (_method_name callback) method_name)
      (if (loaded self)
	  (error "how do: self._remote_call_thread.q.append(callback)")
	  (error "how do: self._ngl_displayed_callbacks_before_loaded.apped(callback)"))
      (error "how do: self._ngl_displayed_callbacks_after_loaded.append(callback)"))))

(defmethod _get_traj_by_id ((self NGLWidget) itsid)
  (loop for traj in (_trajlist self)
     do
       (if (equal (id traj) itsid)
	   (return traj)))
  nil)

(defmethod hide ((self NGLWidget) indices)
  (let ((traj_ids (loop for traj in (_trajlist self) collect (id traj)))
	(comp_id nil)
	(traj nil))
    (loop for index in indices
       do
	 (setf comp_id (aref (_ngl_component_ids self) index))
	 (if t
	     (progn
	       (error "the above line is wrong. Should be 'if comp_id in traj_ids'")
	       (setf traj (_get_traj_by_id self comp_id)
		     (shown traj nil))))
	 (_remote_call self
		       "setVisibility"
		       :target "compList"
		       :args '(nil)
		       :kwargs (list (cons "component_index" index)))))
  (values))

(defmethod show ((self NGLWidget) &rest kwargs &key &allow-other-keys)
  (apply #'show_only self kwargs)
  (values))

(defmethod show_only ((self NGLWidget) &key (indices "all"))
  (let ((traj_ids (loop for traj in (_trajlist self) collect (id traj)))
	(indices_ "")
	(index 0)
	(traj nil)
	(args '(nil)))
    (setf traj_ids (remove-duplicates traj_ids :test #'equal))
    (if (string= indices "all")
	(setf indices_ (loop for i from 0 to (n_components self) collect i))
	(progn
	  (setf indices_ (loop for index in indices collect index)
		indices_ (remove-duplicates indices_ :test #'equal))))
    (loop for comp_id in (_ngl_component_ids self)
       do
	 (if t
	     (progn
	       (error "the line above is wrong and should be 'if comp_id in traj_ids")
	       (setf traj (_get_traj_by_id self comp_id)))
	     (setf traj nil))
	 (if t
	     (progn
	       (error "the line above is wrong and should be 'if index in indices_")
	       (setf args '(t))
	       (if traj
		   (setf (shown traj) t)))
	     (progn
	       (setf args '(nil))
	       (if traj
		   (setf (shown traj) nil))))
	 (_remote_call self
		       "setVisiblity"
		       :target "compList"
		       :args args
		       :kwargs (list (cons "component_index" index)))))
  (error "Figure out 'if index in indices_' in show_only in  widget.lisp")
  (values))
	       
	     
	     
   
    

(defmethod _js_console ((self NGLWidget))
  (error "implement _js_console in widget.lisp"))

#|
    def _js_console(self):
        self.send(dict(type='get', data='any'))
|#

(defmethod _get_full_params ((self NGLWidget))
  (error "Implement _get_full_params in widget.lisp"))
#|
    def _get_full_params(self):
        self.send(dict(type='get', data='parameters'))
|#

(defmethod _display_image ((self NGLWidget))
  (error "help _display_image widget.lisp"))
#|
    def _display_image(self):
        '''for testing
        '''
        from IPython import display
        return display.Image(self._image_data)
|#

(defmethod _clear_component_auto_completion ((self NGLWidget))
  (let ((index 0))
    (loop for id in (_ngl_component_ids self)
       do
	 (let ((name (concatenate 'string "component_" (write-to-string index))))
	   (incf index)
	   (error "WE NEED A DELATTR IN _clear_component_auto_completion in widget.lisp")))))
#|
    def _clear_component_auto_completion(self):
        for index, _ in enumerate(self._ngl_component_ids):
            name = 'component_' + str(index)
            delattr(self, name)
|#

(defmethod _update_component_auto_completion ((self NGLWidget))
  (let ((trajids (loop for traj in (_trajlist self) collect (id traj)))
	(index 0))
    (loop for cid in (_ngl_component_ids self)
       do
	 (let ((comp (make-instance 'ComponentViewer :_view self :_index index))
	       (name (concatenate 'string "component_" (write-to-string index))))
	   (incf index)
	   (error "We need a setattr function!!!!")
	   (error "we need an in function! Maybe we have one. _update_component_auto_completion in widget.lisp")))))
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

(defmethod __getitem__ ((self NGLWidget) index)
  "return ComponentViewer"
  (let ((positive_index (get_positive_index py_utils index (length (_ngl_component_ids self)))))
    (make-instance 'ComponentViewer :_view self :_index positive_index))
  (error "Help! We don't have a py_utils thingy in __getitem__ in widget.lisp"))


(defmethod __iter__ ((self NGLWidget))
  "return ComponentViewer"
  (let ((index 0))
    (loop for item in (_ngl_component_ids self))
    (error "Implementer __iter__ in widget.lisp")))
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
      (_move_notebook_to_the_right js_utils))
  (_remote_call self "setDialog" :target "Widget"))

(defclass ComponentViewer ()
  ((_view :initarg :_view :accessor _view
	 :initform nil)
   (_index :initarg :_index :accessor _index
	  :initform nil)))

(defmethod initialize-instance :after ((self ComponentViewer))
  (_add_repr_method_shortcut self (_view self))
  (_borrow_attribute self (_view self) (list "clear_representations"
					     "_remove_representations_by_name"
					     "_update_representations_by_name"
					     "center_view"
					     "center"
					     "clear"
					     "set_representations")
		     (list "get_structure_string"
			   "get_coodinates"
			   "n_frames")))

(defmethod id ((self ComponentViewer))
  (aref (_ngl_component_ids (_view self)) (_index self)))

(defmethod hide ((self ComponentViewer))
  "set invisibility for given components (by their indices)"
  (_remote_call (_view self)
		"setVisibility"
		:target "compList"
		:args (list nil)
		:kwargs (list (cons "component_index" (_index self))))
  (let ((traj (_get_traj_by_id (_view self) (id self))))
    (if traj
	(setf (shown traj) nil))
    (values)))

(defmethod show ((self ComponentViewer))
  "set invisibility for given components (by their indices)"
  (_remote_call (_view self)
		"setVisiblity"
		:target "compList"
		:args (list t)
		:kwargs (list (cons "component_index" (_index self))))
  (let ((traj (_get_traj_by_id (_view self) (id self))))
    (if traj
	(setf (shown traj) t))
    (values)))

(defmethod show ((self ComponentViewer) repr_type &optional (selection "all") &rest kwargs &key &allow-other-keys)
  (setf (aref kwargs "component") (_index self))
  (add_representation (_view self) :repr_type repr_type :selection selection kwargs))

(defmethod _borrow_attribute ((self ComponentViewer) view attributes &key (trajectory_atts nil))
  (let ((traj (_get_traj_by_id view (id self))))
    (loop for attname in attributes
       do
	 (let ((view_att nil)))))
  (error "Help me!!!"))
		
||#

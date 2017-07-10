(in-package :nglv)

(defclass nglwidget (cl-jupyter:domwidget)
  ((%image-data :initarg :image-data
		:type unicode
		:initform (unicode "")
		:metadata (:sync t :json-name "_image_data"))
   (%selector :initarg :selector
	      :accessor selector
	      :type Unicode
	      :initform (Unicode)
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
		:type unicode
		:initform (Unicode "white")
		:metadata (:sync t :json-name "background"))
   (%loaded :initarg :loaded
	    :accessor loaded
	    :type bool
	    :initform :false
	    :metadata (:sync t :json-name "loaded"))
   (%picked :initarg :picked
	    :accessor picked
	    :type dict
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
		:type dict
		:initform nil)
   (%full-stage-parameters :initarg :full-stage-parameters
			   :accessor full-stage-parameters
			   :type dict
			   :initform nil
			   :metadata (:sync t :json-name "_full_stage_parameters"))
   ;; Not sync'd
   (%coordinates-dict :initarg :coordinates-dict
		      :accessor coordinates-dict
		      :type dict
		      :initform nil)
   (%repr-dict :initarg :repr-dict
	       :accessor repr-dict
	       :type dict
	       :initform nil)
   (%ngl-component-names :initarg :ngl-component-names
			 :accessor ngl-component-names
			 :type list
			 :initform nil)
   (%send-binary :initarg :send-binary
		 :accessor send-binary
		 :type bool
		 :initform :true)
   (%init-gui :initarg :init-gui
	      :accessor init-gui
	      :type bool
	      :initform :false)
   (%hold-image :initarg :hold-image
		:accessor hold-image
		:type bool
		:initform :false)
   (%ngl-msg :initarg :ngl-msg
	     :accessor ngl-msg
	     :type (or string (eql nil))
	     :initform nil)
   ;; internal variables
   (%gui :initarg :%gui :accessor %gui :initform nil)
   (%init-gui :initarg :gui :accessor gui :initform nil)  ;; WHY? does nglview does this
   (%theme :initarg :theme :accessor theme :initform "default")
   (%widget-image :initarg :widget-image :accessor widget-image :initform (cl-jupyter-widgets:image))
   (%image-array :initarg :image-array :accessor image-array :initform nil)
   (%event :initarg :event :accessor event :initform :threading.event.object)
   (%ngl-displayed-callbacks-before-loaded :initarg :ngl-displayed-callbacks-before-loaded
					   :accessor ngl-displayed-callbacks-before-loaded
					   :initform nil)
   (%ngl-displayed-callbacks-after-loaded :initarg :ngl-displayed-callbacks-after-loaded
					   :accessor ngl-displayed-callbacks-after-loaded
					   :initform nil)
   (%shape :initarg :shape :accessor shape
	   :initform (make-instance nglv:shape))
   (%handle-msg-thread :initarg :handle-msg-thread
		       :accessor handle-msg-thread
		       :initform :threading.thread)
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
   (%representations :initarg :representations :accessor representations)
        self._set_unsync_camera()
        self.selector = str(uuid.uuid4()).replace('-', '')
        self._remote_call('setSelector', target='Widget', args=[self.selector,])
        self.selector = '.' + self.selector # for PlaceProxy
        self._place_proxy = PlaceProxy(child=None, selector=self.selector)
        self.player = TrajectoryPlayer(self)
        self._already_constructed = True

		
   
   )
  (:default-initargs
   :view-name (unicode "NGLView")
    :view-module (unicode "nglview-js-widgets")))
    
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

(in-package :nglv)

(defun strip (string)
  (string-trim #(#\Space #\Newline #\Return) string))

(defclass TrajectoryPlayer (cljw::domwidget)
  ((%step :initarg :%step :accessor %step
	 :type integer
	 :initform 1 ;Original default is 0 but init makes it 1
	 :metadata (:sync t
			  :json-name "step"))
   (sync-frame :initarg :sync-frame :accessor sync-frame
	       :type boolean
	       :initform nil ;Original default is true but init makes it false
	       :metadata (:sync t
				:json-name "sync_frame"))
   (interpolate :initarg :interpolate :accessor interpolate
		:type bool
		:initform :false
		:metadata (:sync t
				 :json-name "interpolate"))
   (delay :initarg :delay :accessor delay
	  :type float
	  :initform 100.0 ;Original default is 0.0 but init makes it 100
	  :metadata (:sync t
			   :json-name "delay"))
   (%parameters :initarg :parameters :accessor trajectory-player-parameters
	       :type cljw:dict
	       :initform nil
	       :metadata (:sync t
				:json-name "parameters"))
   (iparams :initarg :iparams :accessor iparams
	    :type cljw:dict
	    :initform nil
	    :metadata (:sync t
			     :json-name "iparams"))
   (%interpolation-t :initarg :%interpolation-t :accessor %interpolation-t
		     :type float
		     :initform 0.5 ;Original default is nil but init makes it 0.5
		     :metadata (:sync t
				      :json-name "_interpolation_t"))
   (%iterpolation-type :initarg :%iterpolation-type :accessor %iterpolation-type
			:type unicode
			:initform (unicode "linear");Original default is "" but init makes it "linear"
			:metadata (:sync t
					 :json-name "_iterpolation_type"
					 :caseless-str-enum '("linear" "spline")
					 :help "either linear or spline"))
   (spin :initarg :spin :accessor spin
	 :type bool
	 :initform :false
	 :metadata (:sync t
			  :json-name "spin"))
   (%spin-x :initarg :%spin-x :accessor %spin-x
	    :type integer
	    :initform 1
	    :metadata (:sync t
			     :json-name "_spin_x"))
   (%spin-y :initarg :%spin-y :accessor %spin-y
	    :type integer
	    :initform 0
	    :metadata (:sync t
			     :json-name "_spin_y"))
   (%spin-z :initarg :%spin-z :accessor %spin-z
	    :type integer
	    :initform 0
	    :metadata (:sync t
			     :json-name "_spin_z"))
   (%spin-speed :initarg :%spin-speed :accessor %spin-speed
		:type float
		:initform 0.005
		:metadata (:sync t
				 :json-name "_spin_speed"))
   (camera :initarg :camera :accessor trajectory-player-camera
	   :type unicode
	   :initform (unicode "perspective")
	   :metadata (:sync t
			    :json-name "camera"
			    :caseless-str-enum '("perspective" "orthographic")
			    :help "Options: perspective or orthographic"))
   (%render-params :initarg :%render-params :accessor %render-params
		   :type cljw:dict
		   :initform nil
		   :metadata (:sync t
				    :json-name "_render_params"))
   (%real-time-update :initarg :%real-time-update :accessor %real-time-update
		      :type bool
		      :initform :false
		      :metadata (:sync t
				       :json-name "_real_time_update"))
   ;;gap in python code
   (widget-tab :initarg :widget-tab :accessor widget-tab
	       :initform nil
	       :metadata (:sync t
				:json-name "widget_tab"))
   (widget-repr :initarg :widget-repr :accessor widget-repr
		:initform nil
		:metadata (:sync t
				 :json-name "widget_repr"))
   (widget-repr-parameters :initarg :widget-repr-parameters :accessor widget-repr-parameters
			   :initform nil
			   :metadata (:sync t
					   :json-name "widget_repr_parameters"))
   (widget-quick-repr :initarg :widget-quick-repr :accessor widget-quick-repr
		      :initform nil
		      :metadata (:sync t
				       :json-name "widget_quick_repr"))
   (widget-general :initarg :widget-general :accessor widget-general
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_general"))
   (widget-picked :initarg :widget-picked :accessor widget-picked
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_picked"))
   (widget-preference :initarg :widget-preference :accessor widget-preference
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_preference"))
   (widget-extra :initarg :widget-extra :accessor widget-extra
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_extra"))
   (widget-theme :initarg :widget-theme :accessor widget-theme
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_theme"))
   (widget-help :initarg :widget-help :accessor widget-help
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_help"))
   (widget-export-image :initarg :widget-export-image :accessor widget-export-image
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_export_image"))
   (widget-component-slider :initarg :widget-component-slider :accessor widget-component-slider
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_component_slider"))
   (widget-repr-slider :initarg :widget-repr-slider :accessor widget-repr-slider
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_repr_slider"))
   (widget-repr-choices :initarg :widget-repr-choices :accessor widget-choices
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_repr_choices"))
   (widget-repr-control-buttons :initarg :widget-repr-control-buttons :accessor widget-repr-control-buttons
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_repr_control_buttons"))
   (widget-repr-add :initarg :widget-repr-add :accessor widget-repr-add
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_repr_add"))
   (widget-accordion-repr-parameters :initarg :widget-accordion-repr-parameters :accessor widget-accordion-repr-parameters
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_accordion_repr_parameters"))
   (widget-repr-parameters-dialog :initarg :widget-repr-parameters-dialog :accessor widget-repr-parameters-dialog
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_repr_parameters_dialog"))
   (widget-repr-name :initarg :widget-repr-name :accessor widget-repr-name
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_repr_name"))
   (widget-component-dropdown :initarg :widget-component-dropdown :accessor widget-component-dropdown
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_component_dropdown"))
   (widget-drag :initarg :widget-drag :accessor widget-drag
		   :initform nil
		   :metadata (:sync t
				    :json-name "widget_drag"))
   ;;;include the other parameters found in the __init__ function
  (view :initarg :view :accessor view
	:initform nil)
  (min-delay :initarg :min-delay :accessor min-delay
	     :type integer
	     :initform 40)
  (%widget-names :initarg :widget-names :accessor widget-names
		 :type list
		 :initform ()))
  (:metaclass traitlets:traitlet-class))
   
(defmethod initialize-instance :after ((player TrajectoryPlayer) &key)
  (setf (iparams player) (list (cons :t (%interpolation-t player))
			       (cons :step 1)
			       (cons :type (%iterpolation-type player)) ))
  (setf (render-params player) (list (cons :factor 4)
				    (cons :antialias :true)
				    (cons :trim :false)
				    (cons :transparent :false))))
  ;; the following doesn't appear to be used anywhere correct
  ;; https://github.com/drmeister/spy-ipykernel/blob/master/nglview/player.py#L80
  ;; self._widget_names = [w for w in dir(self) if w.startswith('wiget_')]

#|
 self._widget_names = [w for w in dir(self) if w.startswith('wiget_')]
        self.observe(self._on_widget_built, names=['widget_repr_parameters',
            'widget_repr',
            'widget_preference'])
|#

(defmethod %update-padding ((self TrajectoryPlayer) &optional (padding *DEFAULT-PADDING*))
  (with-slots (widget-general widget-repr widget-preference widget-repr-parameters widget-help widget-extra wiget-picked) self
    (let ((widget-collection (list widget-general widget-repr widget-preference widget-repr-parameters widget-help widget-extra widget-picked)))
      (dolist (widget widget-collection)
	(setf (padding (layout widget)) padding)))))

(defmethod %create-all-widgets ((self TrajectoryPlayer))
  (if not (widget-tab self)
      (setf (widget-tab self) (%display self)))
  (let ((old-index (selected-index (widget-tab self)))
	(new-index 0))
    (loop for (index) across (children (widget-tab self))
       do
	 (setf (selected-index (widget-tab self)) new-index)
	 (incf new-index))
    (setf (selected-index (widget-tab self)) old-index)))

(defmethod smooth ((self TrajectoryPlayer))
  (setf (interpolate self) t))

(defmethod on-camera-changed ((self TrajectoryPlayer) change)
  (let ((camera-type (aref change "new")))
    (%remote-call (%view self) "setParameters" :target "Stage" :kwargs (list :cameraType camera-type))))

(defmethod frame-setter ((self TrajectoryPlayer) value)
  (setf (frame (%view self)) value))

(defmethod update-sync-frame ((self TrajectoryPlayer) change)
  (let ((value (aref change "new")))
    (if value
	(%set-sync-frame (%view self))
	(%set-unsync-frame (%view self)))))

(defmethod update-delay ((self TrajectoryPlayer) change)
  (let ((delay (aref change "new")))
    (%set-delay (%view self) delay)))

(defmethod update-parameters ((self TrajectoryPlayer) change)
  (let ((params (aref change "new")))
    (setf (sync-frame self) (get params "sync_frame" (sync-frame self))
	  (delay self) (get params "delay" (delay self))
	  (step self) (get params "step" (step self)))))

(defmethod %interpolation-t-changed ((self TrajectoryPlayer) change)
  (setf (aref (iparams self) "t") (aref change "new")))

(defmethod on-spin-changed ((self TrajectoryPlayer) change)
  (setf (spin self) (aref change "new"))
  (if (spin self)
      (%set-spin (%view self) (list (%spin-x self) (%spin-y self) (%spin-z self)) (%spin-speed self))
      (%set-spin (%view self) nil nil)))

(defmethod on-spin-x-changed ((self TrajectoryPlayer) change)
  (setf (%spin-x self) (aref change "new"))
  (if (spin self)
      (%set-spin (%view self) (list (%spin-x self) (%spin-y self) (%spin-z self)) (%spin-speed self))))

(defmethod on-spin-y-changed ((self TrajectoryPlayer) change)
  (setf (%spin-y self) (aref change "new"))
  (if (spin self)
      (%set-spin (%view self) (list (%spin-x self) (%spin-y self) (%spin-z self)) (%spin-speed self))))

(defmethod on-spin-z-changed ((self TrajectoryPlayer) change)
  (setf (%spin-z self) (aref change "new"))
  (if (spin self)
      (%set-spin (%view self) (list (%spin-x self) (%spin-y self) (%spin-z self)) (%spin-speed self))))

(defmethod on-spin-speed-changed ((self TrajectoryPlayer) change)
  (setf (%spin-speed self) (aref change "new"))
  (if (spin self)
      (%set-spin (%view self) (list (%spin-x self) (%spin-y self) (%spin-z self)) (%spin-speed self))))

(defmethod %display ((self TrajectoryPlayer))
  (let* ((box-factory (list
		       (cons (%make-general-box self) "General")
		       (cons (%make-widget-repr self) "Representation")
		       (cons (%make-widget-preference self) "Preference")
		       (cons (%make-theme-box self) "Theme")
		       (cons (%make-extra-box self) "Extra")
		       (cons (%make-hide-tab-with-place-proxy self) "Hide")
		       (cons (%show-website self) "Help")))
	 (tab (%make-delay-tab box-factory :selected-index 0)))
    (setf (align-self (layout tab)) "center" (align-items (layout tab)) "stretch")
    (setf (widget-tab self) tab)
    (widget-tab self)))

(defmethod %make-widget-tab ((self TrajectoryPlayer))
  (%display self))

(defmethod %make-hide-tab-with-place-proxy ((self TrajectoryPlayer))
  (apply #'make-instance 'cl-jupyter-widgets::Box (%place-proxy (%view self))))

(defmethod %make-button-center ((self TrajectoryPlayer))
  (let ((button (make-instance 'cl-jupyter-widgets::button :description " Center" :icon "fa-bullseye")))
  (flet ((on-click (button)
    (center (%view self))))
    button)))
  ;;I need to figure out how to do @button.on_click to register this as the on_click method
 #|
    def _make_button_center(self):
        button = Button(description=' Center', icon='fa-bullseye')
        @button.on_click
        def on_click(button):
            self._view.center()
        return button
 |#

(defmethod %make-button-theme ((self TrajectoryPlayer))
  (make-instance 'cl-jupyter-widgets::Button :description "Oceans16")
  (error "Help!!! Implement %make-button-theme from player.lisp"))
 #|
    def _make_button_theme(self):
        button = Button(description='Oceans16')
        @button.on_click
        def on_click(button):
            from nglview import theme
            display(theme.oceans16())
            self._view._remote_call('cleanOutput',
                                    target='Widget')
        return button
 |#

(defmethod %make-button-reset-theme ((self TrajectoryPlayer) &optional (hide-toolbar nil))
  (error "Help! Implement me, %make-button-rest-theme from player.lisp"))
 
#|
    def _make_button_reset_theme(self, hide_toolbar=False):
        from nglview import theme

        if hide_toolbar:
            button = Button(description='Simplified Default')
            @button.on_click
            def on_click(button):
                theme.reset(hide_toolbar=True)
        else:
            button = Button(description='Default')
            @button.on_click
            def on_click(button):
                theme.reset()
        return button
 |#

(defmethod %make-button-clean-error-output ((self TrajectoryPlayer))
  (let ((button (make-instance 'cl-jupyter-widgets::button :description "Clear Error")))
     (error "poop error output button help me")))
;;;I need to figure out how to register this as the on_click method.

(defmethod %make-widget-preference ((self TrajectoryPlayer) &optional (width "100%"))
  (flet ((make-func ()
	   (let ((parameters (%full-stage-parameters (%view self))))
	     (flet ((func (&key (pan-speed (get parameters "panSpeed" 0.8))
				(rotate-speed (get parameters "rotateSpeed" 2))
				(zoom-speed (get parameters "zoomSpeed" 1.2))
				(clip-dist (get parameters "clipDist" 10))
				(camera-fov (get parameters "cameraFov" 40))
				(clip-far (get parameters "clipFar" 100))
				(clip-near (get parameters "clipNear" 0))
				(fog-far (get parameters "fogFar" 100))
				(fog-near (get parameters "fogNear" 50))
				(impostor (get parameters "impostor" t))
				(light-intensity (get parameters "lightIntensity" 1))
				(quality (get parameters "quality" "medium"))
				(sample-level (get parameters "sampleLevel" 1)))
		      (setf (parameters (%view self)) (list (cons "panSpeed" pan-speed)
							    (cons "rotateSpeed" rotate-speed)
							    (cons "zoomSpeed" zoom-speed)
							    (cons "clipDist" clip-dist)
							    (cons "clipFar" clip-far)
							    (cons "cameraFov" camera-fov)
							    (cons "fogFar" fog-far)
							    (cons "fogNear" fog-near)
							    (cons "impostor" impostor)
							    (cons "lightIntensity" light-intensity)
							    (cons "quality" quality)
							    (cons "sampleLevel" sample-level)))
		      ))
	       func)))
	 (make-widget-box ()
	   (let ((widget-sliders (make-instance 'cl-jupyter-widgets::interactive
						:%interact-f make-func
						:pan-speed '(0 10 0.1)
						:rotate-speed '(0 10 1)
						:zoom-speed '(0 10 1)
						:clip-dist '(0 200 5)
						:clip-far '(0 100 1)
						:clip-near '(0 100 1)
						:camera-fov '(15 120 1)
						:fog-far '(0 100 1)
						:fog-near '(0 100 1)
						:light-intensity '(0 10 0.02)
						:quality '("low" "medium" "high")
						:sample-level '(-1 5 1))))
	     (loop for child across (children widget-sliders)
		do
		  (if (or (typep child 'cl-jupyter-widgets::int-slider)
			  (typep child 'cl-jupyter-widgets::float-slider))
		      (setf (width (layout child)) *DEFAULT-SLIDER-WIDTH*)))
	     widget-sliders)))
    (when (not (widget-preference self))
      (let* ((widget-sliders (make-widget-box))
	     (reset-button (make-instance 'cl-jupyter-widgets::button :description "Reset")))
	    (setf (children widget-sliders) (vector reset-button (children widget-sliders)))
	(error "Help me complete myself! def on-click (reset-button) somewhere in player.lisp"))
      #| @reset_button.on_click
      def on_click(reset_button):
      self._view.parameters = self._view._original_stage_parameters
      self._view._full_stage_parameters = self._view._original_stage_parameters
      widget_sliders.children = [reset_button,] + list(make_widget_box().children)|#
	 
      (setf (widget-preference self) (%relayout-master (widget-sliders :width width)))
    (widget-prefence self))))


	

(defmethod %show-download-image ((self TrajectoryPlayer))
  (let ((button (make-instance 'cl-jupyter-widgets::button :description " Screenshot" :icon "fa-camera")))
    (flet ((on-click (button)
	     (download-image (%view self))))
      button)))
;;I need a way to register the on_click method to stay with the button.


(defmethod %make-button-url ((self TrajectoryPlayer) url description)
  (let ((button (make-instance 'cl-jupyter-widgets::button :description description)))
    (flet ((on-click (button)
	     (display (Javascript (format (open-url-template js-utils) :url url)))))
      button)))
;;;HELP ME WITH THIS ONE please. I don't understand the javascript part

(defmethod %show-website ((self TrajectoryPlayer) &optional (ngl-base-url *NGL-BASE-URL*))
  (error "-show-website in player.lisp not implemented"))
#| 
 buttons = [self._make_button_url(url.format(ngl_base_url), description) for url, description in
            [("'http://arose.github.io/nglview/latest/'", "nglview"),
             ("'{}/index.html'", "NGL"),
             ("'{}/tutorial-selection-language.html'", "Selection"),
             ("'{}/tutorial-molecular-representations.html'", "Representation")]
        ]
        self.widget_help = _make_autofit(HBox(buttons))
        return self.widget_help
 |#

(defmethod %make-button-qtconsole ((self TrajectoryPlayer))
  (let ((button (make-instance 'cl-jupyter-widgets::button :description "qtconsole" :tooltip "pop up qtconsole")))
    (flet ((on-click (button)
	     (funcall #'launch-qtconsole js-utils)))
      button)))
;;;Still don't know how to register my button on_click function

(defmethod %make-text-picked ((self TrajectoryPlayer))
  (let ((ta (Textarea :value (funcall dumps json (picked (%view self))) :description "Picked atom")))
    (setf (width (layout ta)) "300px")
    ta))

(defmethod %refresh ((self TrajectoryPlayer) component-slider repr-slideR)
  (%request-repr-parameters (%view self) :component (value component-slider) :repr-index (value repr-slider))
  (%update-repr-dict (%view self))
  (%handle-repr-dict-changed (%view self) :change (list (cons "new" (%repr-dict (%view self))))))


(defmethod %make-button-repr-control ((self TrajectoryPlayer) component-slider repr-slider repr-selection)
  (let ((button-refresh (make-instance 'cl-jupyter-widgets::button
				       :description " Refresh"
				       :tooltip "Get representation info"
				       :icon "fa-refresh"))
	(button-center-selection (make-instance 'cl-jupyter-widgets::button
						:description " Center"
						:tooltip "center selected atoms" :
						:icon "fa-bullseye"))
	(button-hide (make-instance 'cl-jupyter-widgets::button
				    :description " Hide"
				    :tooltip "Hide/Show current representation"
				    :icon "fa-eye-slash"))
	(button-remove (make-instance 'cl-jupyter-widgets::button
				      :description " Remove"
				      :tooltip "Remove current representation"
				      :icon "fa-trash"))
	(button-repr-parameter-dialog (make-instance 'cl-jupyter-widgets::button
						     :description " Dialog"
						     :tooltip "Pop up representation parameters control dialog")))
    (setf (%ngl-name button-center-selection) "button-center-selection")
  (error "Help %make-button-repr-control on click methods")))
 #|
        @button_refresh.on_click
        def on_click_refresh(button):
            self._refresh(component_slider, repr_slider)

        @button_center_selection.on_click
        def on_click_center(center_selection):
            self._view.center_view(selection=repr_selection.value,
                                   component=component_slider.value)

        @button_hide.on_click
        def on_click_hide(button_hide):
            component=component_slider.value
            repr_index=repr_slider.value

            if button_hide.description == 'Hide':
                hide = True
                button_hide.description = 'Show'
            else:
                hide = False
                button_hide.description = 'Hide'

            self._view._remote_call('setVisibilityForRepr',
                                    target='Widget',
                                    args=[component, repr_index, not hide])

        @button_remove.on_click
        def on_click_remove(button_remove):
            self._view._remove_representation(component=component_slider.value,
                                              repr_index=repr_slider.value)
            self._view._request_repr_parameters(component=component_slider.value,
                                                repr_index=repr_slider.value)

        @button_repr_parameter_dialog.on_click
        def on_click_repr_dialog(_):
            from nglview.widget_box import DraggableBox
            if self.widget_repr_parameters is not None and self.widget_repr_choices:
                self.widget_repr_parameters_dialog = DraggableBox([self.widget_repr_choices,
                                     self.widget_repr_parameters])
                self.widget_repr_parameters_dialog._ipython_display_()
                self.widget_repr_parameters_dialog._dialog = 'on'

        bbox = _make_autofit(HBox([button_refresh, button_center_selection,
                                   button_hide, button_remove,
                                   button_repr_parameter_dialog]))
        return bbox
 |#

(defmethod %make-widget-repr ((self TrajectoryPlayer))
  (setf (widget-repr-name self) (make-instance 'cl-jupyter-widgets::text :value "" :description "representation")
	(%ngl-name (widget-repr-name self)) "repr-name-text")
  (let ((repr-selection (make-instance 'cl-jupyter-widgets::text :value "" :description "selection")))
    (setf (ngl-name repr-selection) "repr-selection"
	  (width repr-selection) *DEFAULT-TEXT-WIDTH*
	  (width (widget-repr-name self)) *DEFAULT-TEXT-WIDTH*)
    (let ((max-n-components (max (% (n-components (%view self)) 1) 0)))
      (setf (widget-component-slider self) (make-instance 'cl-jupyter-widgets::int-slider :value 0 :max max-n-components :min 0 :description "component")
	    (%ngl-name (widget-component-slider self)) "component-slider")
      (let ((cvalue " "))
	(setf (widget-component-dropdown self) (make-instance 'cl-jupyter-widgets::dropdown
							      :value cvalue
							      :options '((cvalue . nil))
							      :description "component")
	      (%ngl-name (widget-component-dropdown self)) "component_dropdown"
	      (widget-repr-slider self) (make-instance 'cl-jupyter-widgets::int-slider
						       :value 0
						       :description "representation"
						       :width *DEFAULT-SLIDER-WIDTH*))
	(setf (%ngl-name (widget-repr-slider self)) "repr_slider"
	      (visible (widget-repr-slider self)) t
	      (width (layout (widget-component-slider self))) *DEFAULT-SLIDER-WIDTH*
	      (width (layout (widget-repr-slider self))) *DEFAULT-SLIDER-WIDTH*
	      (width (layout (widget-component-dropdown self))) *DEFAULT-TEXT-WIDTH*
	      (max-width (widget-component-dropdown self)) *DEFAULT-TEXT-WIDTH*
	      (display (layout (widget-component-dropdown self))) "none"
	      (description (widget-component-dropdown self)) ""
	      (widget-accordion-repr-parameters self) (make-instance 'cl-jupyter-widgets::tab)
	      (widget-repr-parameters self) (%make-widget-repr-parameters self (widget-component-slider self) (widget-repr-slider self) (widget-repr-name self)))
	(setf (children (widget-accordion-repr-parameters self)) (list (widget-repr-parameters self) (make-instance 'cl-jupyter-widgets::box)))
	(set-title (widget-accordion-repr-parameters self) 0 "Parameters")
	(set-title (widget-accordion-repr-parameters self) 1 "Hide")
	(setf (selected-index (widget-accordion-repr-parameters self)) 1)
	(let ((checkbox-reprlist (make-instance 'cl-jupyter-widgets::checkbox :value :false
						:description "reprlist")))
	  (setf (%ngl-name checkbox-reprlist) "checkbox_reprlist"
		(widget-repr-choices self) (%make-repr-name-choices self (widget-component-slider self) (widget-repr-slider self)))
	  (setf (%ngl-name (widget-repr-choices self)) "reprlist_choices"
		(widget-repr-add self) (%make-add-widget-repr self (widget-component-slider self)))
	  (flet ((on-update-checkbox-reprlist (change)
		   (setf (visible (widget-repr-choices self)) (aref change "new"))
		 (values)))
	    (observe checkbox-reprlist on-update-checkbox-reprlist :names "value")	  
	  (error "-make-widget-repr not finished!!")))))))
 #|
        def on_repr_name_text_value_changed(change):
            name = change['new'].strip()
            old = change['old'].strip()

            should_update = (self._real_time_update
                             and old and name
                             and name in REPRESENTATION_NAMES
                             and name != change['old'].strip())

            if should_update:
                component=self.widget_component_slider.value
                repr_index=self.widget_repr_slider.value
                self._view._remote_call('setRepresentation',
                                 target='Widget',
                                 args=[change['new'], {}, component, repr_index])
                self._view._request_repr_parameters(component, repr_index)

        def on_component_or_repr_slider_value_changed(change):
            self._view._request_repr_parameters(component=self.widget_component_slider.value,
                                                repr_index=self.widget_repr_slider.value)
            self.widget_component_dropdown.options = tuple(self._view._ngl_component_names)

            if self.widget_accordion_repr_parameters.selected_index >= 0:
                self.widget_repr_parameters.name = self.widget_repr_name.value
                self.widget_repr_parameters.repr_index = self.widget_repr_slider.value
                self.widget_repr_parameters.component_index = self.widget_component_slider.value

        def on_repr_selection_value_changed(change):
            if self._real_time_update:
                component = self.widget_component_slider.value
                repr_index = self.widget_repr_slider.value
                self._view._set_selection(change['new'],
                                          component=component,
                                          repr_index=repr_index)

        def on_change_component_dropdown(change):
            choice = change['new']
            if choice:
                 self.widget_component_slider.value = self._view._ngl_component_names.index(choice)

        self.widget_component_dropdown.observe(on_change_component_dropdown, names='value')

        self.widget_repr_slider.observe(on_component_or_repr_slider_value_changed, names='value')
        self.widget_component_slider.observe(on_component_or_repr_slider_value_changed, names='value')
        self.widget_repr_name.observe(on_repr_name_text_value_changed, names='value')
        repr_selection.observe(on_repr_selection_value_changed, names='value')

        self.widget_repr_control_buttons = self._make_button_repr_control(self.widget_component_slider,
        self.widget_repr_slider, repr_selection)

        blank_box = Box([Label("")])

        all_kids = [self.widget_repr_control_buttons,
                    blank_box,
                    self.widget_repr_add,
                    self.widget_component_dropdown,
                    self.widget_repr_name,
                    repr_selection,
                    self.widget_component_slider,
                    self.widget_repr_slider,
                    self.widget_repr_choices,
                    self.widget_accordion_repr_parameters
        ]

        vbox = VBox(all_kids)

        self._view._request_repr_parameters(component=self.widget_component_slider.value,
            repr_index=self.widget_repr_slider.value)

        self.widget_repr = _relayout_master(vbox, width='100%')

        self._refresh(self.widget_component_slider, self.widget_repr_slider)

        setattr(self.widget_repr, "_saved_widgets", [])
        for _box in self.widget_repr.children:
            if hasattr(_box, 'children'):
                for kid in _box.children:
                    self.widget_repr._saved_widgets.append(kid)

        return self.widget_repr
 |#

(defmethod %make-widget-repr-parameters ((self TrajectoryPlayer) component-slider repr-slider &optional (repr-name-text nil))
  (let ((name " "))
    (if repr-name-text
	(setf name (value repr-name-text)))
    (let ((widget (%display-repr (%view self)
		 :component (value component-slider)
		 :repr-index (value repr-slider)
		 :name name)))
      (setf (%ngl-name widget) "repr_parameters_box")
      widget)))
      
(defmethod %make-button-export-image ((self TrajectoryPlayer))
  (let ((slider-factor (make-instance 'cl-jupyter-widgets::int-slider
				      :value 4
				      :min 1
				      :max 10
				      :description "scale"))
	(checkbox-antialias (make-instance 'cl-jupyter-widgets::checkbox
					   :value :true
					   :description "antialias"))
	(checkbox-trim (make-instance 'cl-jupyter-widgets::checkbox
				      :value :false
				      :description "trim"))
	(checkbox-transparent (make-instance 'cl-jupyter-widgets::checkbox
					     :value :false
					     :description "transparent"))
	(filename-text (make-instance 'cl-jupyter-widgets::text
				      :value "Screenshot"
				      :description "Filename"))
	(delay-text (make-instance 'cl-jupyter-widgets::float-text
				   :value 1
				   :description "delay (s)"
				   :tooltip "hello"))
	(start-text (make-instance 'cl-jupyter-widgets::int-text
				   :value 0
				   :description "start"))
	(stop-text (make-instance 'cl-jupyter-widgets::int-text
				  :value (count (%view self))
				  :description "stop"))
	(step-text (make-instance 'cl-jupyter-widgets::int-text
				  :value 1
				  :description "step")))
    (setf (max-width (layout start-text)) *DEFAULT-TEXT-WIDTH*
	  (max-width (layout stop-text)) *DEFAULT-TEXT-WIDTH*
	  (max-width (layout step-text)) *DEFAULT-TEXT-WIDTH*
	  (max-width (layout filename-text)) *DEFAULT-TEXT-WIDTH*
	  (max-width (layout delay-text)) *DEFAULT-TEXT-WIDTH*)
    (let ((button-movie-images (make-instance 'cl-jupyter-widgets::button
					      :description "Export Images")))
      (flet ((download-image (filename)
	       (download-image (%view self)
			       :factor (value slider-factor)
			       :antialias (value checkbox-antialias)
			       :trim (value checkbox-trim)
			       :transparent (value checkbox-transparent)
			       :filename filename))
	     (on-click-images (button-move-images)
	       (error "Help implement on-click-images in player.lisp!")))
	(let* ((vbox (make-instance 'cl-jupyter-widgets::vbox
				   :children (vector button-movie-images
						     start-text
						     stop-text
						     step-text
						     delay-text
						     filename-text
						     slider-factor
						     checkbox-antialias
						     checkbox-trim
						     checkbox-transparent)))
	       (form-items (%relayout vbox make-form-item-layout))
	       (form (make-instance 'cl-jupyter-widgets::Box form-items :layout (%make-box-layout))))
	  form)))))
     #|
        @button_movie_images.on_click
        def on_click_images(button_movie_images):
            for i in range(start_text.value, stop_text.value, step_text.value):
                self._view.frame = i
                time.sleep(delay_text.value)
                download_image(filename=filename_text.value + str(i))
                time.sleep(delay_text.value)
 |#
	    
(defmethod %make-resize-notebook-slider ((self TrajectoryPlayer))
  (let ((resize-notebook-slider (make-instance 'cl-jupyter-widgets::int-slider :min 300 :max 2000 :description "resize notebook")))
    (flet ((on-resize-notebook(change)
	     (let ((width (aref change "new")))
	       (remote-call (%view self) "resizeNotebook" :target "Widget" :args (list width))
	       (values))))
      (observe resize-notebook-slider on-resize-notebook :names "value")
      resize-notebook-slider)))



(defmethod %make-add-widget-repr ((self TrajectoryPlayer) component-slider)
  (let ((dropdown-repr-name (make-instance 'cl-jupyter-widgets::dropdown
					   :options *REPRESENTATION-NAMES*
					   :value "cartoon"))
	(repr-selection (make-instance 'cl-jupyter-widgets::text
				       :value "*"
				       :description ""))
	(repr-button (make-instance 'cl-jupyter-widgets::button
				    :description "Add"
				    :tooltip "Add representation. You can also hit Enter in selection box.")))
    (setf (layout repr-button) (make-instance 'cl-jupyter-widgets::layout
					      :width "auto"
					      :flex "1 1 auto")
	  (width (layout dropdown-repr-name)) *DEFAULT-TEXT-WIDTH*
	  (width (layout repr-selection)) *DEFAULT-TEXT-WIDTH*)
    (flet ((on-click-or-submit (button-or-text-area)
	     (add-representation (%view self)
				 :selection (strip (value repr-selection))
				 :repr-type (value dropdown-repr-name)
				 :component (value component-slider))
	     (values)))
      (on-click repr-button on-click-or-submit)
      (on-submit repr-selection on-click-or-submit)
      (let ((add-repr-box (make-instance 'cl-jupyter-widgets::hbox
					 :children (vector repr-button
							   dropdown-repr-name
							   repr-selection))))
	(setf (%ngl-name add-repr-box) "add_repr_box")
	add-repr-box))))
 


(defmethod %make-repr-playground ((self TrajectoryPlayer))
  (error "-make-repr-playground in player.lisp needs your help"))
   #|
    def _make_repr_playground(self):
        vbox = VBox()
        children = []

        rep_names = REPRESENTATION_NAMES[:]
        excluded_names = ['ball+stick', 'distance']
        for name in excluded_names:
            rep_names.remove(name)

        repr_selection = Text(value='*')
        repr_selection.layout.width = default.DEFAULT_TEXT_WIDTH
        repr_selection_box  = HBox([Label('selection'), repr_selection])
        setattr(repr_selection_box, 'value', repr_selection.value)

        for index, name in enumerate(rep_names):
            button = ToggleButton(description=name)

            def make_func():
                def on_toggle_button_value_change(change, button=button):
                    selection = repr_selection.value
                    new = change['new'] # True/False
                    if new:
                        self._view.add_representation(button.description, selection=selection)
                    else:
                        self._view._remove_representations_by_name(button.description)
                return on_toggle_button_value_change

            button.observe(make_func(), names='value')
            children.append(button)

        button_clear = Button(description='clear', button_style='info',
                icon='fa-eraser')

        @button_clear.on_click
        def on_clear(button_clear):
            self._view.clear()
            for kid in children:
                # unselect
                kid.value = False

        vbox.children = children + [repr_selection, button_clear]
        _make_autofit(vbox)
        self.widget_quick_repr = vbox
        return self.widget_quick_repr
 |#

(defmethod %make-repr-name-choices ((self TrajectoryPlayer) component-slider repr-slider)
  (let ((repr-choices (make-instance 'cl-jupyter-widgets::dropdown :options '((" " . "")))))
    (flet ((on-chose (change)
	     (let ((repr-name (aref change "new"))
		   (repr-index (index (options repr-choices))))
	       (setf (value repr-slider) repr-index)
	       (values))))
      (observe repr-choices on-chose :names "value")
      (setf (width (layout repr-choices)) *DEFAULT-TEXT-WIDTH*
	    (widget-repre-choices self) repr-choices)
      (widget-repr-choices self)))
  (error "I don't think we have an observe or index function defined.  %make-repr-name-choices in player.lisp"))
 #|
    def _make_repr_name_choices(self, component_slider, repr_slider):
        repr_choices = Dropdown(options=[" ",])

        def on_chose(change):
            repr_name = change['new']
            repr_index = repr_choices.options.index(repr_name)
            repr_slider.value = repr_index

        repr_choices.observe(on_chose, names='value')
        repr_choices.layout.width = default.DEFAULT_TEXT_WIDTH

        self.widget_repr_choices = repr_choices
        return self.widget_repr_choices
 |#

(defmethod %make-drag-widget ((self TrajectoryPlayer))
  (error "only YOU can prevent this error message in %make-drag-widget in player.lisp"))
 #|
    def _make_drag_widget(self):
        button_drag = Button(description='widget drag: off', tooltip='dangerous')
        drag_nb = Button(description='notebook drag: off', tooltip='dangerous')
        button_reset_notebook = Button(description='notebook: reset', tooltip='reset?')
        button_dialog = Button(description='dialog', tooltip='make a dialog')
        button_split_half = Button(description='split screen', tooltip='try best to make a good layout')

        @button_drag.on_click
        def on_drag(button_drag):
            if button_drag.description == 'widget drag: off':
                self._view._set_draggable(True)
                button_drag.description = 'widget drag: on'
            else:
                self._view._set_draggable(False)
                button_drag.description = 'widget drag: off'

        @drag_nb.on_click
        def on_drag_nb(button_drag):
            if drag_nb.description == 'notebook drag: off':
                js_utils._set_notebook_draggable(True)
                drag_nb.description = 'notebook drag: on'
            else:
                js_utils._set_notebook_draggable(False)
                drag_nb.description = 'notebook drag: off'

        @button_reset_notebook.on_click
        def on_reset(button_reset_notebook):
            js_utils._reset_notebook()

        @button_dialog.on_click
        def on_dialog(button_dialog):
            self._view._remote_call('setDialog', target='Widget')

        @button_split_half.on_click
        def on_split_half(button_dialog):
            from nglview import js_utils
            import time
            js_utils._move_notebook_to_the_left()
            js_utils._set_notebook_width('5%')
            time.sleep(0.1)
            self._view._remote_call('setDialog', target='Widget')

        drag_box = HBox([button_drag, drag_nb, button_reset_notebook,
                        button_dialog, button_split_half])
        drag_box = _make_autofit(drag_box)
        self.widget_drag = drag_box
        return drag_box
 |#

(defmethod %make-spin-box ((self TrajectoryPlayer))
  (let ((checkbox-spin (apply #'make-instance 'cl-jupyter-widgets::checkbox (%spin-x self) :description "spin"))
	(spin-x-slide (apply #'make-instance 'cl-jupyter-widgets::int-slider (%spin-x self) :min -1 :max 1 :description "spin_x"))
	(spin-y-slide (apply #'make-instance 'cl-jupyter-widgets::int-slider (%spin-y self) :min -1 :max 1 :description "spin_y"))
	(spin-z-slide (apply #'make-instance 'cl-jupyter-widgets::int-slider (%spin-z self) :min -1 :max 1 :description "spin_z"))
	(spin-speed-slide (apply #'make-instance 'cl-jupyter-widgets::float-slider (%spin-speed self) :min 0 :max 0.2 :step 0.001 :description "spin speed")))
    (error "Only YOU can implement the link traitlet")
     #|
        link((checkbox_spin, 'value'), (self, 'spin'))
        link((spin_x_slide, 'value'), (self, '_spin_x'))
        link((spin_y_slide, 'value'), (self, '_spin_y'))
        link((spin_z_slide, 'value'), (self, '_spin_z'))
        link((spin_speed_slide, 'value'), (self, '_spin_speed'))
     |#

    (let ((spin-box (make-instance 'cl-jupyter-widgets::vbox :children (vector checkbox-spin spin-x-slide spin-y-slide spin-z-slide spin-speed-slide))))
      (setf spin-box (%relayout-master spin-box :width "75%"))
      spin-box)))

(defmethod %make-widget-picked ((self TrajectoryPlayer))
  (setf (widget-picked self) (%make-text-picked self))
  (let ((picked-box (make-instance 'cl-jupyter-widgets::hbox :children (vector (widget-picked self)))))
    (%relayout-master picked-box :width "75%")))

(defmethod %make-export-image-widget ((self TrajectoryPlayer))
  (if (not (widget-export-image self))
      (setf (widget-export-image self) (make-instance 'cl-jupyter-widgets::hbox :children (vector (funcall (%make-button-export-image self))))))
      (widget-export-image self))
;;;HELP! This can't be right. I don't think my vector works properly.

(defmethod %make-extra-box ((self TrajectoryPlayer))
  (if (not (widget-extra self))
      (let* ((extra-list (list
			 (cons (%make-drag-widget self) "Drag")
			 (cons (%make-spin-box self) "Spin")
			 (cons (%make-widget-picked) "Picked")
			 (cons (%make-repr-playground) "Quick")
			 (cons (%make-export-image-widget) "Image")
			 (cons (%make-command-box self) "Command")))
	     (extra-box (%make-delay-tab extra-list :selected-index 0)))
	(setf (widget-extra self) extra-box)))
  (widget-extra self))

(defmethod %make-theme-box ((self TrajectoryPlayer))
  (if (not (widget-theme self))
      (setf (widget-theme self) (apply #'make-instance 'cl-jupyter-widgets::box :children (vector (%make-button-theme self) (%make-button-reset-theme self :hide-toolbar nil) (%make-button-reset-theme self :hide-toolbar t) (%make-button-clean-error-output self)))))
  (widget-theme self))

(defmethod %make-general-box ((self TrajectoryPlayer))
  (if (not (widget-general self))
      (let ((step-slide (make-instance 'cl-jupyter-widgets::int-slider :value (step self) :min -100 :max 100 :description "step"))
	    (delay-text (make-instance 'cl-jupyter-widgets::int-slider :value (delay self) :min 10 :max 1000 :description "delay"))
	    (toggle-button-interpolate (apply #'make-instance 'cl-jupyter-widgets::toggle-button (interpolate self) :description "Smoothing" :tooltip "smoothing trajectory")))
	(error "Help me finish %make-general-box's implementation")
	;;link((toggle-button-interpolate, 'value'), (self, 'interpolate')
	(let ((background-color-picker (make-instance 'cl-jupyter-widgets::color-picker :value "white" :description "background"))
	      (camera-type (make-instance 'cl-jupyter-widgets::dropdown :value (camera self) :options '(("perspective" . "orthographic")) :description "camera")))
	  (error "Help me finish %make-general-box's implementation!")
	     #|          link((step_slide, 'value'), (self, 'step'))
            link((delay_text, 'value'), (self, 'delay'))
            link((toggle_button_interpolate, 'value'), (self, 'interpolate'))
            link((camera_type, 'value'), (self, 'camera'))
            link((background_color_picker, 'value'), (self._view, 'background'))
	   |#
	  (let* ((center-button (%make-button-center self))
		(render-button (%show-download-image self))
		(qtconsole-button (%make-button-qtconsole self))
		(center-render-hbox (%make-autofit (make-instance 'cl-jupyter-widgets::hbox :children (vector toggle-button-interpolate center-button render-button qtconsole-button))))
		 (v0-left (make-instance 'cl-jupyter-widgets::vbox :children (vector step-slide delay-text background-color-picker camera-type center-render-hbox))))
	    (setf v0-left (%relayout-master v0-left :width "100%"))
	    (setf (widget-general self) v0-left)
	    widget-general)))))

(defmethod %make-command-box ((self TrajectoryPlayer))
  (let ((widget-text-command (make-instance 'cl-jupyter-widgets::text)))
    (error "only YOU can prevent this error from existing. %make-command-box player.lisp")))
#|
   def _make_command_box(self):
        widget_text_command = Text()

        @widget_text_command.on_submit
        def _on_submit_command(_):
            command = widget_text_command.value
            js_utils.execute(command)
            widget_text_command.value = ''
        return widget_text_command
|#


(defmethod %create-all-tabs ((self TrajectoryPlayer))
  (let ((tab (display self))
	(index 0))
    (loop for child across (children tab)
       do
	 (setf (selected-index tab) index)
	 (incf index))
    (setf (widget-extra self) (%make-extra-box self)
	  index 0)
    (loop for child across (children (widget-extra self))
       do
	 (setf (selected-index (widget-extra self)) index)))
  (values))


(defmethod %simplify-repr-control ((self TrajectoryPlayer))
  (loop for widget in (%saved-widgets (widget-repr self))
     do
       (if (not (typep widget 'cl-jupyter-widgets::tab))
	   (setf (display (layout widget)) "none")))
  (setf (display (layout (widget-repr-choices self))) "flex"
	(selected-index (widget-accordion-repr-parameters self)) 0)
  (values))


(in-package :nglv)

(defun strip (string)
  (string-trim #(#\Space #\Newline #\Return) string))

(defclass TrajectoryPlayer (cl-jupyter::dom-widget)
  ((step :initarg :step :accessor step
	 :type integer
	 :initform 1 ;Original default is 0 but init makes it 1
	 :metadata (:sync t
			  :json-name "step"))
   (sync_frame :initarg :sync_frame :accessor sync_frame
	       :type bool
	       :initform :false ;Original default is true but init makes it false
	       :metadata (:sync t
				:json-name "sync_frame"))
   (interpolate :initarg :interpolate :accessor interpolate
		:type bool
		:initform :false
		:metadata (:sync nil
				 :json-name "interpolate"))
   (delay :initarg :delay :accessor delay
	  :type float
	  :initform 100 ;Original default is 0.0 but init makes it 100
	  :metadata (:sync t
			   :json-name "delay"))
   (parameters :initarg :parameters :accessor parameters
	       :type list
	       :initform ()
	       :metadata (:sync t
				:json-name "parameters"))
   (iparams :initarg :iparams :accessor iparams
	    :type list
	    :initform ()
	    :metadata (:sync nil
			     :json-name "iparams"))
   (_interpolation_t :initarg :_interpolation_t :accessor _interpolation_t
		     :type float
		     :initform 0.5 ;Original default is nil but init makes it 0.5
		     :metadata (:sync nil
				      :json-name "_interpolation_t"))
   (_iterpolation_type :initarg :_iterpolation_type :accessor _iterpolation_type
			:type unicode
			:initform (unicode "linear");Original default is "" but init makes it "linear"
			:metadata (:sync nil
					 :json-name "_iterpolation_type"
					 :help "either linear or spline"))
   (spin :initarg :spin :accessor spin
	 :type bool
	 :initform :false
	 :metadata (:sync nil
			  :json-name "spin"))
   (_spin_x :initarg :_spin_x :accessor _spin_x
	    :type integer
	    :initform 1
	    :metadata (:sync nil
			     :json-name "_spin_x"))
   (_spin_y :initarg :_spin_y :accessor _spin_y
	    :type integer
	    :initform 0
	    :metadata (:sync nil
			     :json-name "_spin_y"))
   (_spin_z :initarg :_spin_z :accessor _spin_z
	    :type integer
	    :initform 0
	    :metadata (:sync nil
			     :json-name "_spin_z"))
   (_spin_speed :initarg :_spin_speed :accessor _spin_speed
		:type float
		:initform 0.005
		:metadata (:sync nil
				 :json-name "_spin_speed"))
   (camera :initarg :camera :accessor camera
	   :type unicode
	   :initform (unicode "perspective")
	   :metadata (:sync nil
			    :json-name "camera"
			    :help "Options: perspective or orthographic"))
   (_render_params :initarg :_render_params :accessor _render_params
		   :type list
		   :initform ()
		   :metadata (:sync nil
				    :json-name "_render_params"))
   (_real_time_update :initarg :_real_time_update :accessor _real_time_update
		      :type bool
		      :initform :false
		      :metadata (:sync nil
				       :json-name "_real_time_update"))
   ;;gap in python code
   (widget_tab :initarg :widget_tab :accessor widget_tab
	       :initform nil
	       :metadata (:sync nil
				:json-name "widget_tab"))
   (widget_repr :initarg :widget_repr :accessor widget_repr
		:initform nil
		:metadata (:sync nil
				 :json-name "widget_repr"))
   (widget_repr_parameters :initarg :widget_repr_parameters :accessor widget_repr_parameters
			   :initform nil
			   :metadata (:sync nil
					   :json-name "widget_repr_parameters"))
   (widget_quick_repr :initarg :widget_quick_repr :accessor widget_quick_repr
		      :initform nil
		      :metadata (:sync nil
				       :json-name "widget_quick_repr"))
   (widget_general :initarg :widget_general :accessor widget_general
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_general"))
   (widget_picked :initarg :widget_picked :accessor widget_picked
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_picked"))
   (widget_preference :initarg :widget_preference :accessor widget_preference
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_preference"))
   (widget_extra :initarg :widget_extra :accessor widget_extra
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_extra"))
   (widget_theme :initarg :widget_theme :accessor widget_theme
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_theme"))
   (widget_help :initarg :widget_help :accessor widget_help
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_help"))
   (widget_export_image :initarg :widget_export_image :accessor widget_export_image
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_export_image"))
   (widget_component_slider :initarg :widget_component_slider :accessor widget_component_slider
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_component_slider"))
   (widget_repr_slider :initarg :widget_repr_slider :accessor widget_repr_slider
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_repr_slider"))
   (widget_repr_choices :initarg :widget_repr_choices :accessor widget_choices
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_repr_choices"))
   (widget_repr_control_buttons :initarg :widget_repr_control_buttons :accessor widget_general
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_repr_control_buttons"))
   (widget_repr_add :initarg :widget_repr_add :accessor widget_repr_add
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_repr_add"))
   (widget_accordion_repr_parameters :initarg :widget_accordion_repr_parameters :accessor widget_accordion_repr_parameters
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_accordion_repr_parameters"))
   (widget_repr_parameters_dialog :initarg :widget_repr_parameters_dialog :accessor widget_repr_parameters_dialog
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_repr_parameters_dialog"))
   (widget_repr_name :initarg :widget_repr_name :accessor widget_repr_name
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_repr_name"))
   (widget_component_dropdown :initarg :widget_component_dropdown :accessor widget_component_dropdown
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_component_dropdown"))
   (widget_drag :initarg :widget_drag :accessor widget_drag
		   :initform nil
		   :metadata (:sync nil
				    :json-name "widget_drag"))
   ;;;include the other parameters found in the __init__ function
  (view :initarg :view :accessor view
	:initform nil)
  (min_delay :initarg :min_delay :accessor min_delay
	     :type integer
	     :initform 40)
  (widget_names :initarg :widget_names :accessor widget_names
		 :type list
		 :initform ()))
  (:metaclass traitlets:traitlet-class))
   
(defmethod initialize-instance :after ((self TrajectoryPlayer) &key)
  (setf iparams '(:t (_interpolation_t self) :step 1 :type (_iterpolation_type))
	_render_params '(:factor 4 :antialias t :trim nil :transparent nil))
  (error "Help me! Fix widget_names and observe"))
#|
 self._widget_names = [w for w in dir(self) if w.startswith('wiget_')]
        self.observe(self._on_widget_built, names=['widget_repr_parameters',
            'widget_repr',
            'widget_preference'])
|#

(defmethod _on_widget_built ((self TrajectoryPlayer) change)
  (let ((widget (aref change "new")))
       (if widget
	   (setf (padding (layout widget)) "5%"))))
;;Not sure about this one

(defmethod _update_padding ((self TrajectoryPlayer) &optional (padding *DEFAULT_PADDING*))
  (with-slots (widget_general widget_repr widget_preference widget_repr_parameters widget_help widget_extra wiget_picked) self
    (let ((widget_collection (list widget_general widget_repr widget_preference widget_repr_parameters widget_help widget_extra widget_picked)))
      (dolist (widget widget_collection)
	(setf (padding (layout widget)) padding)))))

(defmethod _create_all_widgets ((self TrajectoryPlayer))
  (if not (widget_tab self)
      (setf (widget_tab self) (_display self)))
  (let ((old_index (selected_index (widget_tab self)))
	(new_index 0))
    (loop for (index) across (children (widget_tab self))
       do
	 (setf (selected_index (widget_tab self)) new_index)
	 (incf new_index))
    (setf (selected_index (widget_tab self)) old_index)))

(defmethod smooth ((self TrajectoryPlayer))
  (setf (interpolate self) :true))

(defmethod on_camera_changed ((self TrajectoryPlayer) change)
  (let ((camera_type (aref change "new")))
    (_remote_call (_view self) "setParameters" :target "Stage" :kwargs (list :cameraType camera_type))))

(defmethod frame ((self TrajectoryPlayer))
  (frame (_view self)))

(defmethod frame ((self TrajectoryPlayer) value)
  (setf (frame (_view self)) value))

(defmethod count ((self TrajectoryPlayer))
  (count (_view self)))

(defmethod update_sync_frame ((self TrajectoryPlayer) change)
  (let ((value (aref change "new")))
    (if value
	(_set_sync_frame (_view self))
	(_set_unsync_frame (_view self)))))

(defmethod update_delay ((self TrajectoryPlayer) change)
  (let ((delay (aref change "new")))
    (_set_delay (_view self) delay)))

(defmethod update_parameters ((self TrajectoryPlayer) change)
  (let ((params (aref change "new")))
    (setf (sync_frame self) (get params "sync_frame" (sync_frame self))
	  (delay self) (get params "delay" (delay self))
	  (step self) (get params "step" (step self)))))

(defmethod _interpolation_t_chaned ((self TrajectoryPlayer) change)
  (setf (aref (iparams self) "t") (aref change "new")))

(defmethod on_spin_changed ((self TrajectoryPlayer) change)
  (setf (spin self) (aref change "new"))
  (if (spin self)
      (_set_spin (_view self) (list (_spin_x self) (_spin_y self) (_spin_z self)) (_spin_speed self))
      (_set_spin (_view self) nil nil)))

(defmethod on_spin_x_changed ((self TrajectoryPlayer) change)
  (setf (_spin_x self) (aref change "new"))
  (if (spin self)
      (_set_spin (_view self) (list (_spin_x self) (_spin_y self) (_spin_z self)) (_spin_speed self))))

(defmethod on_spin_y_changed ((self TrajectoryPlayer) change)
  (setf (_spin_y self) (aref change "new"))
  (if (spin self)
      (_set_spin (_view self) (list (_spin_x self) (_spin_y self) (_spin_z self)) (_spin_speed self))))

(defmethod on_spin_z_changed ((self TrajectoryPlayer) change)
  (setf (_spin_z self) (aref change "new"))
  (if (spin self)
      (_set_spin (_view self) (list (_spin_x self) (_spin_y self) (_spin_z self)) (_spin_speed self))))

(defmethod on_spin_speed_changed ((self TrajectoryPlayer) change)
  (setf (_spin_speed self) (aref change "new"))
  (if (spin self)
      (_set_spin (_view self) (list (_spin_x self) (_spin_y self) (_spin_z self)) (_spin_speed self))))

(defmethod _display ((self TrajectoryPlayer))
  (let* ((box_factory (list
		       (cons (_make_general_box self) "General")
		       (cons (_make_widget_repr self) "Representation")
		       (cons (_make_widget_preference self) "Preference")
		       (cons (_make_theme_box self) "Theme")
		       (cons (_make_extra_box self) "Extra")
		       (cons (_make_hide_tab_with_place_proxy self) "Hide")
		       (cons (_show_website self) "Help")))
	 (tab (_make_delay_tab box_factory :selected_index 1)))
    (setf (align_self (layout tab)) "center" (align_items (layout tab)) "stretch")
    (setf (widget_tab self) tab)
    (widget_tab self)))

(defmethod _make_widget_tab ((self TrajectoryPlayer))
  (_display self))

(defmethod _make_hide_tab_with_place_proxy ((self TrajectoryPlayer))
  (apply #'make-instance 'cl-jupyter-widgets::Box (_place_proxy (_view self))))

(defmethod _make_button_center ((self TrajectoryPlayer))
  (let ((button (make-instance 'cl-jupyter-widgets::button :description " Center" :icon "fa-bullseye")))
  (flet ((on_click (button)
    (center (_view self))))
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

(defmethod _make_button_theme ((self TrajectoryPlayer))
  (make-instance 'cl-jupyter-widgets::Button :description "Oceans16")
  (error "Help!!! Implement _make_button_theme from player.lisp"))
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

(defmethod _make_button_reset_theme ((self TrajectoryPlayer) &optional (hide_toolbar nil))
  (error "Help! Implement me, _make_button_rest_theme from player.lisp"))
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

(defmethod _make_button_clean_error_output ((self TrajectoryPlayer))
  ((let ((button (make-instance 'cl-jupyter-widgets::button :description "Clear Error")))
     (flet ((on_click (_)
	      (clean_error_output js_utils)))
       button))))
;;;I need to figure out how to register this as the on_click method.

(defmethod _make_widget_preference ((self TrajectoryPlayer) &optional (width "100%"))
  (flet ((make_func ()
	   (let ((parameters (_full_stage_parameters (_view self))))
	     (flet ((func (&key (pan_speed (get parameters "panSpeed" 0.8))
				(rotate_speed (get parameters "rotateSpeed" 2))
				(zoom_speed (get parameters "zoomSpeed" 1.2))
				(clip_dist (get parameters "clipDist" 10))
				(camera_fov (get parameters "cameraFov" 40))
				(clip_far (get parameters "clipFar" 100))
				(clip_near (get parameters "clipNear" 0))
				(fog_far (get parameters "fogFar" 100))
				(fog_near (get parameters "fogNear" 50))
				(impostor (get parameters "impostor" t))
				(light_intensity (get parameters "lightIntensity" 1))
				(quality (get parameters "quality" "medium"))
				(sample_level (get parameters "sampleLevel" 1)))
		      (setf (parameters (_view self)) (list (cons "panSpeed" pan_speed)
							    (cons "rotateSpeed" rotate_speed)
							    (cons "zoomSpeed" zoom_speed)
							    (cons "clipDist" clip_dist)
							    (cons "clipFar" clip_far)
							    (cons "cameraFov" camera_fov)
							    (cons "fogFar" fog_far)
							    (cons "fogNear" fog_near)
							    (cons "impostor" impostor)
							    (cons "lightIntensity" light_intensity)
							    (cons "quality" quality)
							    (cons "sampleLevel" sample_level)))
		      ))
	       func)))
	 (make_widget_box ()
	   (let ((widget_sliders (make-instance 'cl-jupyter-widgets::interactive
						:_interact_f make_func
						:pan_speed '(0 10 0.1)
						:rotate_speed '(0 10 1)
						:zoom_speed '(0 10 1)
						:clip_dist '(0 200 5)
						:clip_far '(0 100 1)
						:clip_near '(0 100 1)
						:camera_fov '(15 120 1)
						:fog_far '(0 100 1)
						:fog_near '(0 100 1)
						:light_intensity '(0 10 0.02)
						:quality '("low" "medium" "high")
						:sample_level '(-1 5 1))))
	     (loop for child across (children widget_sliders)
		do
		  (if (or (typep child 'cl-jupyter-widgets::int-slider)
			  (typep child 'cl-jupyter-widgets::float-slider))
		      (setf (width (layout child)) *DEFAULT_SLIDER_WIDTH*)))
	     widget_sliders)))
    (if (not (widget_preference self))
	(let ((widget_sliders (make_widget_box))
	      (reset_button (make-instance 'cl-jupyter-widgets::button :description "Reset"))
	      ((children widget_sliders) (vector reset_button (children widget_sliders))))
	  (error "Help me complete myself! def on_click (reset_button) somewhere in player.lisp"))
	#| @reset_button.on_click
            def on_click(reset_button):
                self._view.parameters = self._view._original_stage_parameters
                self._view._full_stage_parameters = self._view._original_stage_parameters
                widget_sliders.children = [reset_button,] + list(make_widget_box().children)
	|#
	(setf (widget_preference self) (_relayout_master (widget_sliders :width width))))
    (widget_prefence self)))


	

(defmethod _show_download_image ((self TrajectoryPlayer))
  (let ((button (make-instance 'cl-jupyter-widgets::button :description " Screenshot" :icon "fa-camera")))
    (flet ((on_click (button)
	     (download_image (_view self))))
      button)))
;;I need a way to register the on_click method to stay with the button.


(defmethod _make_button_url ((self TrajectoryPlayer) url description)
  (let ((button (make-instance 'cl-jupyter-widgets::button :description description)))
    (flet ((on_click (button)
	     (display (Javascript (format (open_url_template js_utils) :url url)))))
      button)))
;;;HELP ME WITH THIS ONE please. I don't understand the javascript part

(defmethod _show_website ((self TrajectoryPlayer) &optional (ngl_base_url *NGL_BASE_URL*))
  (error "_show_website in player.lisp not implemented"))
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

(defmethod _make_button_qtconsole ((self TrajectoryPlayer))
  (let ((button (make-instance 'cl-jupyter-widgets::button :description "qtconsole" :tooltip "pop up qtconsole")))
    (flet ((on_click (button)
	     (funcall #'launch_qtconsole js_utils)))
      button)))
;;;Still don't know how to register my button on_click function

(defmethod _make_text_picked ((self TrajectoryPlayer))
  (let ((ta (Textarea :value (funcall dumps json (picked (_view self))) :description "Picked atom")))
    (setf (width (layout ta)) "300px")
    ta))

(defmethod _refresh ((self TrajectoryPlayer) component_slider repr_slideR)
  (_request_repr_parameters (_view self) :component (value component_slider) :repr_index (value repr_slider))
  (_remote_call (_view self) "requestReprInfo" :target "Widget")
  (_handle_repr_dict_changed (_view self) :change (list (cons "new" (_repr_dict (_view self))))))


(defmethod _make_button_repr_control ((self TrajectoryPlayer) component_slider repr_slider repr_selection)
  (let ((button_refresh (make-instance 'cl-jupyter-widgets::button
				       :description " Refresh"
				       :tooltip "Get representation info"
				       :icon "fa-refresh"))
	(button_center_selection (make-instance 'cl-jupyter-widgets::button
						:description " Center"
						:tooltip "center selected atoms" :
						:icon "fa-bullseye"))
	(button_hide (make-instance 'cl-jupyter-widgets::button
				    :description " Hide"
				    :tooltip "Hide/Show current representation"
				    :icon "fa-eye-slash"))
	(button_remove (make-instance 'cl-jupyter-widgets::button
				      :description " Remove"
				      :tooltip "Remove current representation"
				      :icon "fa-trash"))
	(button_repr_parameter_dialog (make-instance 'cl-jupyter-widgets::button
						     :description " Dialog"
						     :tooltip "Pop up representation parameters control dialog")))
    (setf (_ngl_name button_center_selection) "button_center_selection")
  (error "Help _make_button_repr_control on click methods")))
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

(defmethod _make_widget_repr ((self TrajectoryPlayer))
  (setf (widget_repr_name self) (make-instance 'cl-jupyter-widgets::text :value "" :description "representation")
	(_ngl_name (widget_repr_name self)) "repr_name_text")
  (let ((repr_selection (make-instance 'cl-jupyter-widgets::text :value "" :description "selection")))
    (setf (ngl_name repr_selection) "repr_selection"
	  (width repr_selection) *DEFAULT_TEXT_WIDTH*
	  (width (widget_repr_name self)) *DEFAULT_TEXT_WIDTH*)
    (let ((max_n_components (max (- (n_components (_view self)) 1) 0)))
      (setf (widget_component_slider self) (make-instance 'cl-jupyter-widgets::int-slider :value 0 :max max_n_components :min 0 :description "component")
	    (_ngl_name (widget_component_slider self)) "component_slider")
      (let ((cvalue " "))
	(setf (widget_component_dropdown self) (make-instance 'cl-jupyter-widgets::dropdown
							      :value cvalue
							      :options '((cvalue . nil))
							      :description "component")
	      (_ngl_name (widget_component_dropdown self)) "component_dropdown"
	      (widget_repr_slider self) (make-instance 'cl-jupyter-widgets::int-slider
						       :value 0
						       :description "representation"
						       :width *DEFAULT_SLIDER_WIDTH*))
	(setf (_ngl_name (widget_repr_slider self)) "repr_slider"
	      (visible (widget_repr_slider self)) t
	      (width (layout (widget_component_slider self))) *DEFAULT_SLIDER_WIDTH*
	      (width (layout (widget_repr_slider self))) *DEFAULT_SLIDER_WIDTH*
	      (width (layout (widget_component_dropdown self))) *DEFAULT_TEXT_WIDTH*
	      (max_width (widget_component_dropdown self)) *DEFAULT_TEXT_WIDTH*
	      (display (layout (widget_component_dropdown self))) "none"
	      (description (widget_component_dropdown self)) ""
	      (widget_accordion_repr_parameters self) (make-instance 'cl-jupyter-widgets::tab)
	      (widget_repr_parameters self) (_make_widget_repr_parameters self (widget_component_slider self) (widget_repr_slider self) (widget_repr_name self)))
	(setf (children (widget_accordion_repr_parameters self)) (list (widget_repr_parameters self) (make-instance 'cl-jupyter-widgets::box)))
	(set_title (widget_accordion_repr_parameters self) 0 "Parameters")
	(set_title (widget_accordion_repr_parameters self) 1 "Hide")
	(setf (selected_index (widget_accordion_repr_parameters self)) 1)
	(let ((checkbox_reprlist (make-instance 'cl-jupyter-widgets::checkbox :value :false
						:description "reprlist")))
	  (setf (_ngl_name checkbox_reprlist) "checkbox_reprlist"
		(widget_repr_choices self) (_make_repr_name_choices self (widget_component_slider self) (widget_repr_slider self)))
	  (setf (_ngl_name (widget_repr_choices self)) "reprlist_choices"
		(widget_repr_add self) (_make_add_widget_repr self (widget_component_slider self)))
	  (flet ((on_update_checkbox_reprlist (change)
		   (setf (visible (widget_repr_choices self)) (aref change "new"))
		 (values)))
	    (observe checkbox_reprlist on_update_checkbox_reprlist :names "value")	  
	  (error "_make_widget_repr not finished!!")))))))
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

(defmethod _make_widget_repr_parameters ((self TrajectoryPlayer) component_slider repr_slider &optional (repr_name_text nil))
  (let ((name " "))
    (if repr_name_text
	(setf name (value repr_name_text)))
    (let ((widget (_display_repr (_view self)
		 :component (value component_slider)
		 :repr_index (value repr_slider)
		 :name name)))
      (setf (_ngl_name widget) "repr_parameters_box")
      widget)))
      
(defmethod _make_button_export_image ((self TrajectoryPlayer))
  (let ((slider_factor (make-instance 'cl-jupyter-widgets::int-slider
				      :value 4
				      :min 1
				      :max 10
				      :description "scale"))
	(checkbox_antialias (make-instance 'cl-jupyter-widgets::checkbox
					   :value :true
					   :description "antialias"))
	(checkbox_trim (make-instance 'cl-jupyter-widgets::checkbox
				      :value :false
				      :description "trim"))
	(checkbox_transparent (make-instance 'cl-jupyter-widgets::checkbox
					     :value :false
					     :description "transparent"))
	(filename_text (make-instance 'cl-jupyter-widgets::text
				      :value "Screenshot"
				      :description "Filename"))
	(delay_text (make-instance 'cl-jupyter-widgets::float-text
				   :value 1
				   :description "delay (s)"
				   :tooltip "hello"))
	(start_text (make-instance 'cl-jupyter-widgets::int-text
				   :value 0
				   :description "start"))
	(stop_text (make-instance 'cl-jupyter-widgets::int-text
				  :value (count (_view self))
				  :description "stop"))
	(step_text (make-instance 'cl-jupyter-widgets::int-text
				  :value 1
				  :description "step")))
    (setf (max_width (layout start_text)) *DEFAULT_TEXT_WIDTH*
	  (max_width (layout stop_text)) *DEFAULT_TEXT_WIDTH*
	  (max_width (layout step_text)) *DEFAULT_TEXT_WIDTH*
	  (max_width (layout filename_text)) *DEFAULT_TEXT_WIDTH*
	  (max_width (layout delay_text)) *DEFAULT_TEXT_WIDTH*)
    (let ((button_movie_images (make-instance 'cl-jupyter-widgets::button
					      :description "Export Images")))
      (flet ((download_image (filename)
	       (download_image (_view self)
			       :factor (value slider_factor)
			       :antialias (value checkbox_antialias)
			       :trim (value checkbox_trim)
			       :transparent (value checkbox_transparent)
			       :filename filename))
	     (on_click_images (button_move_images)
	       (error "Help implement on_click_images in player.lisp!")))
	(let* ((vbox (make-instance 'cl-jupyter-widgets::vbox
				   :children (vector button_movie_images
						     start_text
						     stop_text
						     step_text
						     delay_text
						     filename_text
						     slider_factor
						     checkbox_antialias
						     checkbox_trim
						     checkbox_transparent)))
	       (form_items (_relayout vbox make_form_item_layout))
	       (form (make-instance 'cl-jupyter-widgets::Box form_items :layout (_make_box_layout))))
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
	    
(defmethod _make_resize_notebook_slider ((self TrajectoryPlayer))
  (let ((resize_notebook_slider (make-instance 'cl-jupyter-widgets::int-slider :min 300 :max 2000 :description "resize notebook")))
    (flet ((on_resize_notebook(change)
	     (let ((width (aref change "new")))
	       (remote_call (_view self) "resizeNotebook" :target "Widget" :args (list width))
	       (values))))
      (observe resize_notebook_slider on_resize_notebook :names "value")
      resize_notebook_slider)))



(defmethod _make_add_widget_repr ((self TrajectoryPlayer) component_slider)
  (let ((dropdown_repr_name (make-instance 'cl-jupyter-widgets::dropdown
					   :options *REPRESENTATION_NAMES*
					   :value "cartoon"))
	(repr_selection (make-instance 'cl-jupyter-widgets::text
				       :value "*"
				       :description ""))
	(repr_button (make-instance 'cl-jupyter-widgets::button
				    :description "Add"
				    :tooltip "Add representation. You can also hit Enter in selection box.")))
    (setf (layout repr_button) (make-instance 'cl-jupyter-widgets::layout
					      :width "auto"
					      :flex "1 1 auto")
	  (width (layout dropdown_repr_name)) *DEFAULT_TEXT_WIDTH*
	  (width (layout repr_selection)) *DEFAULT_TEXT_WIDTH*)
    (flet ((on_click_or_submit (button_or_text_area)
	     (add_representation (_view self)
				 :selection (strip (value repr_selection))
				 :repr_type (value dropdown_repr_name)
				 :component (value component_slider))
	     (values)))
      (on_click repr_button on_click_or_submit)
      (on_submit repr_selection on_click_or_submit)
      (let ((add_repr_box (make-instance 'cl-jupyter-widgets::hbox
					 :children (vector repr_button
							   dropdown_repr_name
							   repr_selection))))
	(setf (_ngl_name add_repr_box) "add_repr_box")
	add_repr_box))))
 


(defmethod _make_repr_playground ((self TrajectoryPlayer))
  (error "_make_repr_playground in player.lisp needs your help"))
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

(defmethod _make_repr_name_choices ((self TrajectoryPlayer) component_slider repr_slider)
  (let ((repr_choices (make-instance 'cl-jupyter-widgets::dropdown :options '((" " . "")))))
    (flet ((on_chose (change)
	     (let ((repr_name (aref change "new"))
		   (repr_index (index (options repr_choices)))
		   ((value repr_slider) repr_index))
	       (values))))
      ((observe repr_choices) on_chose :names "value")
      (setf (width (layout repr_choices)) *DEFAULT_TEXT_WIDTH*
	    (widget_repre_choices self) repr_choices)
      (widget_repr_choices self)))
  (error "I don't think we have an observe or index function defined.  _make_repr_name_choices in player.lisp"))
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

(defmethod _make_drag_widget ((self TrajectoryPlayer))
  (error "only YOU can prevent this error message in _make_drag_widget in player.lisp"))
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

(defmethod _make_spin_box ((self TrajectoryPlayer))
  (let ((checkbox_spin (apply #'make-instance 'cl-jupyter-widgets::checkbox (_spin_x self) :description "spin"))
	(spin_x_slide (apply #'make-instance 'cl-jupyter-widgets::int-slider (_spin_x self) :min -1 :max 1 :description "spin_x"))
	(spin_y_slide (apply #'make-instance 'cl-jupyter-widgets::int-slider (_spin_y self) :min -1 :max 1 :description "spin_y"))
	(spin_z_slide (apply #'make-instance 'cl-jupyter-widgets::int-slider (_spin_z self) :min -1 :max 1 :description "spin_z"))
	(spin_speed_slide (apply #'make-instance 'cl-jupyter-widgets::float-slider (_spin_speed self) :min 0 :max 0.2 :step 0.001 :description "spin speed")))
    (error "Only YOU can implement the link traitlet")
    #|
        link((checkbox_spin, 'value'), (self, 'spin'))
        link((spin_x_slide, 'value'), (self, '_spin_x'))
        link((spin_y_slide, 'value'), (self, '_spin_y'))
        link((spin_z_slide, 'value'), (self, '_spin_z'))
        link((spin_speed_slide, 'value'), (self, '_spin_speed'))
    |#

    (let ((spin_box (make-instance 'cl-jupyter-widgets::vbox :children (vector checkbox_spin spin_x_slide spin_y_slide spin_z_slide spin_speed_slide))))
      (setf spin_box (_relayout_master spin_box :width "75%"))
      spin_box)))

(defmethod _make_widget_picked ((self TrajectoryPlayer))
  (setf (widget_picked self) (_make_text_picked self))
  (let ((picked_box (make-instance 'cl-jupyter-widgets::hbox :children (vector (widget_picked self)))))
    (_relayout_master picked_box :width "75%")))

(defmethod _make_export_image_widget ((self TrajectoryPlayer))
  (if (not (widget_export_image self))
      (setf (widget_export_image self) (make-instance 'cl-jupyter-widgets::hbox :children (vector (funcall (_make_button_export_image self))))))
      (widget_export_image self))
;;;HELP! This can't be right. I don't think my vector works properly.

(defmethod _make_extra_box ((self TrajectoryPlayer))
  (if (not (widget_extra self))
      (let* ((extra_list (list
			 (cons (_make_drag_widget self) "Drag")
			 (cons (_make_spin_box self) "Spin")
			 (cons (_make_widget_picked) "Picked")
			 (cons (_make_repr_playground) "Quick")
			 (cons (_make_export_image_widget) "Image")
			 (cons (_make_command_box self) "Command")))
	     (extra_box (_make_delay_tab extra_list :selected_index 0)))
	(setf (widget_extra self) extra_box)))
  (widget_extra self))

(defmethod _make_theme_box ((self TrajectoryPlayer))
  (if (not (widget_theme self))
      (setf (widget_theme self) (apply #'make-instance 'cl-jupyter-widgets::box :children (vector (_make_button_theme self) (_make_button_reset_theme self :hide_toolbar nil) (_make_button_reset_theme self :hide_toolbar t) (_make_button_clean_error_output self)))))
  (widget_theme self))

(defmethod _make_general_box ((self TrajectoryPlayer))
  (if (not (widget_general self))
      (let ((step_slide (make-instance 'cl-jupyter-widgets::int-slider :value (step self) :min -100 :max 100 :description "step"))
	    (delay_text (make-instance 'cl-jupyter-widgets::int-slider :value (delay self) :min 10 :max 1000 :description "delay"))
	    (toggle_button_interpolate (apply #'make-instance 'cl-jupyter-widgets::toggle-button (interpolate self) :description "Smoothing" :tooltip "smoothing trajectory")))
	(error "Help me finish _make_general_box's implementation")
	;;link((toggle_button_interpolate, 'value'), (self, 'interpolate')
	(let ((background_color_picker (make-instance 'cl-jupyter-widgets::color-picker :value "white" :description "background"))
	      (camera_type (make-instance 'cl-jupyter-widgets::dropdown :value (camera self) :options '(("perspective" . "orthographic")) :description "camera")))
	  (error "Help me finish _make_general_box's implementation!")
	  #|            link((step_slide, 'value'), (self, 'step'))
            link((delay_text, 'value'), (self, 'delay'))
            link((toggle_button_interpolate, 'value'), (self, 'interpolate'))
            link((camera_type, 'value'), (self, 'camera'))
            link((background_color_picker, 'value'), (self._view, 'background'))
	  |#
	  (let* ((center_button (_make_button_center self))
		(render_button (_show_download_image self))
		(qtconsole_button (_make_button_qtconsole self))
		(center_render_hbox (_make_autofit (make-instance 'cl-jupyter-widgets::hbox :children (vector toggle_button_interpolate center_button render_button qtconsole_button))))
		 (v0_left (make-instance 'cl-jupyter-widgets::vbox :children (vector step_slide delay_text background_color_picker camera_type center_render_hbox))))
	    (setf v0_left (_relayout_master v0_left :width "100%"))
	    (setf (widget_general self) v0_left)
	    widget_general)))))

(defmethod _make_command_box ((self TrajectoryPlayer))
  (let ((widget_text_command (make-instance 'cl-jupyter-widgets::text)))
  (error "only YOU can prevent this error from existing. _make_command_box player.lisp")))
#|    def _make_command_box(self):
        widget_text_command = Text()

        @widget_text_command.on_submit
        def _on_submit_command(_):
            command = widget_text_command.value
            js_utils.execute(command)
            widget_text_command.value = ''
        return widget_text_command

|#

(defmethod _create_all_tabs ((self TrajectoryPlayer))
  (let ((tab (display self))
	(index 0))
    (loop for child across (children tab)
       do
	 (setf (selected_index tab) index)
	 (incf index))
    (setf (widget_extra self) (_make_extra_box self)
	  index 0)
    (loop for child across (children (widget_extra self))
       do
	 (setf (selected_index (widget_extra self)) index)))
  (values))


(defmethod _simplify_repr_control ((self TrajectoryPlayer))
  (loop for widget in (_saved_widgets (widget_repr self))
     do
       (if (not (typep widget 'cl-jupyter-widgets::tab))
	   (setf (display (layout widget)) "none")))
  (setf (display (layout (widget_repr_choices self))) "flex"
	(selected_index (widget_accordion_repr_parameters self)) 0)
  (values))

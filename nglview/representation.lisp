(in-package :nglv)

(defclass RepresentationControl (Box)
  ((parameters :initarg :parameters :accessor parameters
	       :type list
	       :initform ()
	       :metadata (:sync nil
				:json-name "parameters"))
   (name :initarg :name :accessor name
	 :initform nil
	 :metadata (:sync nil
			  :json-name "name"))
   (repr_index :initarg :repr_index :accessor repr_index
	       :type integer
	       :initform 0
	       :metadata (:sync nil
				:json-name "repr_index"))
   (component_index :initarg :component_index :accessor component_index
		    :type integer
		    :initform 0
		    :metadata (:sync nil
				     :json-name "component_index"))
   (_disable_update_parameters :initarg :_disabled_update_parameters :accessor _disabled_update_parameters
			       :type bool
			       :initform :false
			       :metadata (:sync nil
						:json-name "_disable_update_parameters"))
   (view :initarg :view :accessor view
	 :initform nil)
   (children :accessor children
	     :type vector
	     :initform nil)))

(defmethod initialize-instance :after ((self RepresentationControl))
  (setf (children self) (children (_make_widget self))))

(defmethod _on_change_widget_child_value ((self RepresentationControl) change)
  (let ((owner (aref change "owner"))
	(new (aref change "new")))
    (error "Help Implement meeeeeeeee _on_Change_widget_child_value in representation.lisp")))
#|
    def _on_change_widget_child_value(self, change):
        owner = change['owner']
        new = change['new']
        self.parameters = {py_utils._camelize(owner._ngl_description): new}
|#

(defmethod _on_parameters_changed ((self RepresentationControl) change)
  (if (not (_disabled_update_parameters self))
      (let ((parameters (aref change "new")))
	(update_representation (_view self) :component (component_index self) :repr_index (repr_index self) &rest **parameters))))

(defmethod _on_name_changed ((self RepresentationControl) change)
  (let ((new_name (aref change "new")))
    (if (string= new_name "surface")
	(loop for kid in (children self)
	   do
	     (if (string= (_ngl_type kid) "surface")
		 (setf (display (layout kid)) "flex")))
	(loop for kid in (children self)
	   do
	     (if (string= (_ngl_type kid) "surface")
		 (setf (display (layout kid)) "none"))))))

(defmethod _on_repr_index_changed ((self RepresentationControl) change)
  (let ((c_string (concatenate 'string "c" (write-to-string (component_index self))))
	(r_string (write-to-string (aref change "new"))))
    (_update self c_string r_string)))

(defmethod _on_component_index_changed ((self RepresentationControl) change)
  (let ((c_string (concatenate 'string "c" (write-to-string (aref change "new"))))
	(r_string (write-to-string (repr_index self))))
    (_update self c_string r_string)))

(defmethod _update ((self RepresentationControl) c_string r_string)
  (multiple-value-bind (name _repr_dict)
      (_get_name_and_repr_dict self c_string r_string)
    (setf (name self) name (_disable_update_parameters self) t)
    (loop for kid across (children self)
       do
	 (setf desc (_camelize py_utils (_ngl_description kid))))
    (error "Implement me!1 _update from representation.lisp")
    (setf (_disable_update_parameters self) nil)))
#|
    def _update(self, c_string, r_string):
        name, _repr_dict = self._get_name_and_repr_dict(c_string, r_string)
        self.name = name
        self._disable_update_parameters = True
        for kid in self.children:
            desc = py_utils._camelize(kid._ngl_description)
            if desc in _repr_dict:
                kid.value = _repr_dict.get(desc)
        self._disable_update_parameters = False
|#

(defmethod _make_widget ((self RepresentationControl))
  (let ((c_string (concatenate 'string "c" (write-to-string (component_index self))))
	(r_string (write-to-string (repr_index self))))
    (multiple-value-bind (name _repr_dict)
	;; name, _repr_dict = self._get_name_and_repr_dict(c_string, r_string)
	(assembly_list (list "default" "AU" "BU1" "UNITCELL" "SUPERCELL"))
      (surface_types (list "vws" "sas" "ms" "ses"))
      (flet ((func (&key (opacity (get _repr_dict "opacity" 1.))
			 (assembly (get _repr_dict "assembly" "default"))
			 (color_scheme (get _repr_dict "colorScheme" " "))
			 (wireframe (get _repr_dict "wireframe" nil))
			 (probe_radius (get _repr_dict "probeRadius" 1.4))
			 (isolevel (get _repr_dict "isolevel" 2.))
			 (smooth (get _repr_dict "smooth" 2.))
			 (surface_type (get _repr_dict "surfaceType" "ms"))
			 (box_size (get _repr_dict "boxSize" 10))
			 (cutoff (get _repr_dict "cutoff" 0)))))
	(let ((widget (make-instance 'cl-jupyter-widgets::interactive
				     func
				     :opacity '(0. 1. 0.1)
				     :color_scheme *COLOR_SCHEMES*
				     :assembly assembly_list
				     :probe_radius '(0. 5. 0.1)
				     :isolevel '(0. 10. 0.1)
				     :smooth '(0 10 1)
				     :surface_type surface_types
				     :box_size '(0. 100 2)
				     :cutoff '(0. 100 0.1)
				     :continuous_update :false)))
      ;;NOTE: INTERACTIVE IS NOT IMPLEMENTED IN COMMON LISP!!! (find it in python in ipywidgets6/widgets/interaction.py
	  (loop for kid across (children widget)
	     do
	       ;;I think I want to use unwind-protect here
	       (error "finish implementing _make_widget in represenation.lisp"))
	  #|
 try:
                setattr(kid, '_ngl_description', kid.description)
            except AttributeError:
                # ipywidgets.Output does not have `description` attribute
                setattr(kid, '_ngl_description', '')
            if kid._ngl_description in ['probe_radius', 'smooth', 'surface_type', 'box_size', 'cutoff']:
                setattr(kid, '_ngl_type', 'surface')
            else:
                setattr(kid, '_ngl_type', 'basic')
            kid.observe(self._on_change_widget_child_value, 'value')
	  |#
	  widget)))))

(defmethod _get_name_and_repr_dict ((self RepresentationControl) c_string r_string)
  (flet ((read-dict (key dict)
	   (multiple-value-bind (value present-p)
	       (gethash key dict)
	     (if present-p
		 value
		 (return-from _get_name_and_repr_dict
		   (values "" (make-hash-table)))))))
    (let* ((repr-dict (_repr_dict (_view self)))
	   (inner (read-dict r_string (read-dict c_string _repr_dict))))
      (values (read-dict "name" inner)
	      (read-dict "parameters" inner)))))

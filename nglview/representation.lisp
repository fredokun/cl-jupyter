(in-package :nglv)

(defclass RepresentationControl (cljw:Box)
  ((parameters :initarg :parameters :accessor parameters
	       :type list
	       :initform ()
	       :observers (%on-parameters-changed)
	       :metadata (:sync nil
				:json-name "parameters"))
   (name :initarg :name :accessor name
	 :initform nil
	 :observers (%on-name-changed)
	 :metadata (:sync nil
			  :json-name "name"))
   (repr-index :initarg :repr-index :accessor repr-index
	       :type integer
	       :initform 0
	       :observers (%on-repr-index-changed)
	       :metadata (:sync nil
				:json-name "repr_index"))
   (component-index :initarg :component-index :accessor component-index
		    :type integer
		    :initform 0
		    :observers (%on-component-index-changed)
		    :metadata (:sync nil
				     :json-name "component_index"))
   (%disable-update-parameters :initarg :%disabled-update-parameters :accessor %disabled-update-parameters
			       :type bool
			       :initform :false
			       :metadata (:sync nil
						:json-name "_disable_update_parameters"))
   (view :initarg :view :accessor view
	 :initform nil)
   (children :initarg :children :accessor children
	     :type vector
	     :initform nil)))

(defmethod initialize-instance :after ((self RepresentationControl))
  (setf (children self) (children (%make-widget self))))

;Not an observer
(defmethod _on_change_widget_child_value ((self RepresentationControl) change)
  (let ((owner (aref change "owner"))
	(new (aref change "new")))
    (setf (parameters self) (list (cons (camelize (ngl-description owner)) new))))
  (values))

;Observer for parameters
(defmethod %on-parameters-changed (object name new old)
  (unless (%disabled-update-parameters object)
      (setf (parameters object)  new))
  (update-representation (view object) :component (component-index object)
			 :repr-index (repr-index object) &rest (parameters object))
  (values))

;Observer for name
(defmethod %on-name-changed (object name new old)
  (let ((new-name new))
    (if (string= new-name "surface")
	(loop for kid across (children object)
	   do
	     (when (string= (%ngl-type kid) "surface")
	       (setf (display (layout kid)) "flex")))
	(loop for kid across (children object)
	   do
	     (if (string= (%ngl-type kid) "surface")
		 (setf (display (layout kid)) "none")))))
  (values))

;Observer for repr-index
(defmethod %on-repr-index-changed (object name new old)
  (let ((c-string (concatenate 'string "c" (write-to-string (component-index object))))
	(r-string (write-to-string new)))
    (%update object c-string r-string)))

;Observer for component-index
(defmethod %on-component-index-changed (object name new old)
  (let ((c-string (concatenate 'string "c" (write-to-string new)))
	(r-string (write-to-string (repr-index object))))
    (%update self c-string r-string)))

(defmethod %update ((self RepresentationControl) c-string r-string)
  (multiple-value-bind (name %repr-dict)
      (%get-name-and-repr-dict self c-string r-string)
    (setf (name self) name (%disable-update-parameters self) t)
    (loop for kid across (children self)
       do
	 (setf desc (camelize (ngl-description kid))))
    (error "Implement me!1 %update from representation.lisp")
    (setf (%disable-update-parameters self) nil)))
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

(defmethod %make-widget ((self RepresentationControl))
  (let ((c-string (concatenate 'string "c" (write-to-string (component-index self))))
        (r-string (write-to-string (repr-index self))))
    (multiple-value-bind (name %repr-dict)
        (%get-name-and-repr-dict self c_string r_string)
      (let ((assembly-list (list "default" "AU" "BU1" "UNITCELL" "SUPERCELL"))
            (surface_types (list "vws" "sas" "ms" "ses")))
        (flet ((func (&key (opacity (get %repr-dict "opacity" 1.))
                        (assembly (get %repr-dict "assembly" "default"))
                        (color-scheme (get %repr-dict "colorScheme" " "))
                        (wireframe (get %repr-dict "wireframe" nil))
                        (probe-radius (get %repr-dict "probeRadius" 1.4))
                        (isolevel (get %repr-dict "isolevel" 2.))
                        (smooth (get %repr-dict "smooth" 2.))
                        (surface-type (get %repr-dict "surfaceType" "ms"))
                        (box-size (get %repr-dict "boxSize" 10))
                        (cutoff (get %repr-dict "cutoff" 0)))))
          (let ((widget (make-instance 'cl-jupyter-widgets::interactive
                                       func
                                       :opacity '(0. 1. 0.1)
                                       :color-scheme *COLOR-SCHEMES*
                                       :assembly assembly-list
                                       :probe-radius '(0. 5. 0.1)
                                       :isolevel '(0. 10. 0.1)
                                       :smooth '(0 10 1)
                                       :surface-type surface-types
                                       :box-size '(0. 100 2)
                                       :cutoff '(0. 100 0.1)
                                       :continuous-update :false)))
            ;;NOTE: INTERACTIVE IS NOT IMPLEMENTED IN COMMON LISP!!! (find it in python in ipywidgets6/widgets/interaction.py
            (loop for kid across (children widget)
                  do
                     ;;I think I want to use unwind-protect here or handler-case or somethin.
                     (error "finish implementing %make-widget in represenation.lisp"))
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
            widget))))))

(defmethod %get-name-and-repr-dict ((self RepresentationControl) c-string r-string)
  (flet ((read-dict (key dict)
	   (multiple-value-bind (value present-p)
	       (gethash key dict)
	     (if present-p
		 value
		 (return-from %get-name-and-repr-dict
		   (values "" (make-hash-table)))))))
    (let* ((repr-dict (%repr-dict (%view self)))
	   (inner (read-dict r-string (read-dict c-string %repr-dict))))
      (values (read-dict "name" inner)
	      (read-dict "parameters" inner)))))

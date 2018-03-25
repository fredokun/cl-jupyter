(in-package :nglv)

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
  (aref (ngl-component-ids (%view self)) (%index self)))

(defmethod add-representations ((self ComponentViewer) repr-type &optional (selection "all") &rest kwargs &key &allow-other-keys)
  (setf (aref kwargs "component") (%index self))
  (add-representation (%view self) :repr-type repr-type :selection selection kwargs))


(defmethod %borrow-attribute ((self ComponentViewer) view attributes &key (trajectory-atts nil))
  (let ((traj (%get-traj-by-id view (id self))))
    (loop for attname in attributes
       do
         (let ((view-att nil)))))
  (error "Help me!!!"))


(in-package :nglv)
;;;https://github.com/drmeister/spy-ipykernel/blob/master/nglview/layout.py#L8

(defun make-form-item-layout ();Alright I think I know what I'm doing here
  (make-instance 'cl-jupyter-widgets::layout :display "flex" :flex-flow "row"
		 :justify-content "space-between"))

(defun %make-box-layout(&optional (width "100%"))
  (make-instance 'cl-jupyter-widgets::layout :display "flex" :flex-flow "column"
		 :align-items "stretch" :width width))

(defun %relayout (box form-item-layout)
  (let ((form-items ()) (box2 nil))
    (loop for kid across (children box)
	 do
	 (let ((label-value ""))
	   (if (and (description kid) (not (or (typep kid 'button) (typep kid 'toggle-button))))
		(setf label-value (description kid) (description kid) ""))
	   (if (typep kid 'button)
		(setf box2 (make-instance 'cl-jupyter-widgets::Box :children (vector kid) :layout form-item-layout))
		(setf box2 (make-instance 'cl-jupyter-widgets::Box :children (vector (make-instance 'cl-jupyter-widgets::label :value label-value) kid) :layout form-item-layout)))
	   (push box2 form-items)))))

(defun %relayout-master (box &optional (width "20%"))
  (let* ((old-children (;;What does box.children[:]??
			))
	 (form-items (%relayout box (make-form-item-layout)))
	 (form (apply #'make-instance 'cl-jupyter-widgets::Box form-items :layout (%make-box-layout(:width width)))))
    (setf (%ngl-children form) old-children)
    form))

(defun %make-autofit (box)
  (let* ((items-layout (make-instance 'cl-jupyter-widgets::layout :flex "1 1 auto" :width "auto")) ((layout box) items-layout))
    box))

(defun %make-delay-tab (box-factory &optional (selected-index 0))
  (let ((tab (make-instance 'cl-jupyter-widgets::tab
			    :children (loop for (box) in box-factory
					 collect (make-instance 'cl-jupyter-widgets::Box))))
	(i 0))
    
    (loop for (dummy . title) in box-factory
       do
	 (set-title tab i title)
	 (incf i))

    (if (not (children (aref (children tab) selected-index)))
	(setf (selected-index tab) -1))

    (flet ((on-update-selected-index (change)
	     (let ((index (aref change "new")))
	       (if (not (children (aref (children tab) index)))
		   (setf (children (aref (children tab) index)) (error "I don't know what to set it to")))
	       )))
      (observe tab on-update-selected-index :names "selected-index")
      (setf (selected-index tab) selected-index)
      tab)))


    
 #|   """

    Parameters
    ----------
    box_factory : list of (func, tab_name)

    Example of box_factory: [(_make_gen_box, 'General'),
                             (_make_repr_box, 'Representation')]
    """

    tab = Tab([Box() for box, _ in box_factory])
    [tab.set_title(i, title) for i, (_, title) in enumerate(box_factory)]

    # trick
    if not tab.children[selected_index].children:
        tab.selected_index = -1

    def on_update_selected_index(change):
        index = change['new']
        if not tab.children[index].children:
            # make widget on demand
            tab.children[index].children = [box_factory[index][0](),]

    tab.observe(on_update_selected_index, names='selected_index')

    # trigger
    tab.selected_index = selected_index

    return tab
|#

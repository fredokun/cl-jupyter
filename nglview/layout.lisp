(in-package :nglv)
;;;https://github.com/drmeister/spy-ipykernel/blob/master/nglview/layout.py#L8

(defun make_form_item_layout ();Alright I think I know what I'm doing here
  (make-instance 'cl-jupyter-widgets::layout :display "flex" :flex-flow "row"
		 :justify-content "space-between"))

(defun _make_box_layout(&optional (width "100%"))
  (make-instance 'cl-jupyter-widgets::layout :display "flex" :flex-flow "column"
		 :align-items "stretch" :width width))

(defun _relayout (box form_item_layout)
  (let ((form_items ()) (box2 nil))
    (loop for kid across (children box)
	 do
	 (let ((label_value ""))
	   (if (and (description kid) (not (or (typep kid 'button) (typep kid 'toggle-button))))
		(setf label_value (description kid) (description kid) ""))
	   (if (typep kid 'button)
		(setf box2 (make-instance 'cl-jupyter-widgets::Box :children (vector kid) :layout form_item_layout))
		(setf box2 (make-instance 'cl-jupyter-widgets::Box :children (vector (make-instance 'cl-jupyter-widgets::label :value label_value) kid) :layout form_item_layout)))
	   (push box2 form_items)))))

(defun _relayout_master (box &optional (width "20%"))
  (let* ((old_children (;;What does box.children[:]??
			))
	 (form_items (_relayout box (make_form_item_layout)))
	 (form (apply #'make-instance 'cl-jupyter-widgets::Box form_items :layout (_make_box_layout(:width width)))))
    (setf (_ngl_children form) old_children)
    form))

(defun _make_autofit (box)
  (let* ((items_layout (make-instance 'cl-jupyter-widgets::layout :flex "1 1 auto" :width "auto")) ((layout box) items_layout))
    box))

(defun _make_delay_tab (box_factory &optional (selected_index 0))
  (let ((tab (make-instance 'cl-jupyter-widgets::tab
			    :children (loop for (box) in box_factory
					 collect (make-instance 'cl-jupyter-widgets::Box))))
	(i 0))
    
    (loop for (_ . title) in box_factory
       do
	 (set_title tab i title)
	 (incf i))

    (if (not (children (aref (children tab) selected_index)))
	(setf (selected_index tab) -1))

    (flet ((on_update_selected_index (change)
	     (let ((index (aref change "new")))
	       (if (not (children (aref (children tab) index)))
		   (setf (children (aref (children tab) index)) (error "I don't know what to set it to")))
	       )))
      (observe tab on_update_selected_index :names "selected_index")
      (setf (selected_index tab) selected_index)
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

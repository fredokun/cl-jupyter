(in-package :cljw)

(defparameter %context (list (cons "figure" nil)
			     (cons "figure_registry" nil)
			     (cons "scales" nil)
			     (cons "scale_registry" nil)
			     (cons "last_mark" nil)
			     (cons "current_key" nil)))

(defparameter line-style-codes (list (cons ":" "dotted")
				     (cons "-." "dash_dotted")
				     (cons "--" "dashed")
				     (cons "-" "solid")))

(defparameter color-codes (list (cons "b" "blue")
				(cons "g" "green")
				(cons "r" "red")
				(cons "c" "cyan")
				(cons "m" "magenta")
				(cons "y" "yellow")
				(cons "k" "black")))

(defparameter marker-codes (list (cons "o" "circle")
				 (cons "v" "triangle-down")
				 (cons "^" "triangle-up")
				 (cons "s" "square")
				 (cons "d" "diamond")
				 (cons "+" "cross")))

(defun hashtable (data v)
  (warn "How to try data[v]"))

(defun show (&key (key nil) (display-toolbar t))
  (let ((figure nil))
    (if key
	(setf figure (nth key (cdr (assoc "figure_registry" %context :test #'string=))))
      (setf figure (current-figure)))
    (if display-toolbar
	(unless (pyplot figure)
	  (setf (pyplot figure) (make-instance 'toolbar :figure figure)))
	(display (make-instance 'vbox :children (vector figure (pyplot figure))))
      (display figure)))
  (values))

(defun figure (&rest kwargs &key (key nil) (fig nil) &allow-other-keys)
  ;;;We don't want key and fig to actually be in the kwargs plist
  (setf kwargs (remove key kwargs)
        kwargs (remove ':key kwargs)
        kwargs (remove ':fig kwargs)
        kwargs (remove fig kwargs))
  ;;;Now begins the translation of python code.
  (let ((scales-arg (getf kwargs ':scales)))
    ;;Make getf an effective pop of the (:scales value)
    (remove ':scales kwargs)
    (remove scales-arg kwargs)
    (setf (cdr (assoc "current_key" %context :test #'string=)) key)
    (if fig
	(progn
	  (setf (cdr assoc "figure" %context :test #'string=) fig)
	  (when key
	    (setf (cdr (assoc key (cdr (assoc "figure_registry" %context :test #'string=)))) fig))
	  (loop for arg in kwargs));;;Python wants to add slots to the figure class...
      ;;;setattr(%context['figure'], arg, kwargs[arg])
	;;Else clause of the if fig
	(progn
	  (if (not key)
              (setf (cdr (assoc "figure" %context :test #'string=)) (make-instance 'figure kwargs))
              (progn
                (unless (member key (assoc "figure_registry" %context :test #'string=))
                  (unless (getf kwargs :title)
                    (push (concatenate 'string "Figure" " " key) kwargs)
                    (push :title kwargs))
                  (setf (cdr (assoc key (cdr (assoc "figure_registry" %context :test #'string=)))) fig)
                (setf (cdr (assoc "figure" %context :test #'string=)) (cdr (assoc key (cdr (assoc "figure_registry" %context :test #'string=)))))
                (warn "How to Add a slot for each argument in kwargs"))))))
    (scales key :scales scales-arg)
        (loop for arg in kwargs)
    #|
    if(getattr(%context['figure'], 'axist_registry', None) is None):
         setattr(%context['figure'], 'axis_registry', {})
    |#
    ;;;Return the figure in context.
    (cdr (assoc "figure" %context :test #'string=))))
        
(defun close (key)
  (let ((figure-registry (cdr (assoc "figure_registry" %context)))
        (fig nil))
    (unless (member key figure-registry)
      (return-from close))
    (when (eq (cdr (assoc "figure" %context :test #'string=)) (cdr (assoc key figure-registry)))
      (figure))
    (setf fig (cdr (assoc key figure-registry)))
    ;;;if hasattr(fig, 'pyplot')
         ;;;fig.pyplot.close()
    ;;;del figure_registry[key]
    ;;;del _context['scale_registry'][key]
    (values)))

(defun %process-data (&rest kwarg-names &key &allow-other-keys)
  (warn "TODO: Make %process data"))

(defun scales (&key (key nil) (scales nil))
  (warn "TODO: Make scales"))

(defun xlim (low high)
  (set-lim low high "x"))

(defun ylim (low high)
  (set-lim low high "y"))

(defun set-lim (low high name)
  (let ((scale (cdr (assoc (%get-attribute-dimension name) (cdr (assoc "scales" %context :test #'string=))))))
    (setf (min scale) low) (max scale) high)
  scale)

(defun axes (&rest kwargs &key (mark nil) (options nil) &allow-other-keys)
  ;;;Remove mark and options from kwargs
  (setf kwargs (remove mark kwargs)
        kwargs (remove ':mark kwargs)
        kwargs (remove options kwargs)
        kwargs (remove ':options kwargs))
  (unless mark
    (let ((new_mark (cdr (assoc "last_mark" %context :test #'string=))))
      (if new_mark
          (setf mark (cdr (assoc "last_mark" %context :test #'string=)))
          (return-from axes nil))))
  (let ((fig (getf ':figure kwargs)))
    (unless fig
      (setf fig (current-figure)))
    (let ((scales (scales mark))
          (fig-axes (loop for axis in (axes fig) collect axis))
          (axes nil))
      )))
;;;FINISH AXES
    
(defun %set-label (label mark dim &rest kwargs &key &allow-other-keys)
  (unless (or mark (cdr (assoc "last_mark" %context :test #'string=)))
    (return-from %set-label nil))
  (unless mark
    (setf mark (cdr (assoc "last_mark" %context :test #'string=))))
  (let ((fig nil)
        (fig-val (getf ':figure kwargs))
        (scales (scales mark))
        (scale-metadata nil)
        (scale-metadata-val (getf dim (scales-metadata mark)))
        (scale nil))
    (if fig-val
        (setf fig fig-val)
        (setf fig (current-figure)))
    (when scale-metadata-val
      (setf scale-metadata scale-metadata-val))
    (if (getf dim scales)
        (setf scale (getf dim scales))
        (return-from %set-label))
    (let ((dimension nil)
          (val (getf ':dimension scale-metadata))
          (axis nil))
      (if val
          (setf dimension val)
          (setf dimension (cdr (assoc dim scales :test #'string=))))
      (setf axis (%fetch-axis fig dimension (cdr (assoc dim scales :test #'string=))))
      (when axis
        (%apply-properties axis (list (cons "label" label))))))
  (values))

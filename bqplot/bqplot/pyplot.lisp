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
  (remove key kwargs)
  (remove ':key kwargs)
  (remove ':fig kwargs)
  (remove fig kwargs)
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
	    (setf (nth key (cdr (assoc "figure_registry" %context :test #'string=))) fig))
	  (loop for arg in kwargs));;;Python wants to add slots to the figure class...
      ;;;setattr(%context['figure'], arg, kwargs[arg])
	;;Else clause of the if fig
	(progn
	  
      

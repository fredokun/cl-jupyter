(in-package :cljw)

(defclass cyjs (domwidget)
  ((frame-width :accessor frame-width :type integer :initform 400
		:metadata (:sync t :json-name "frameWidth"))
   (frame-height :accessor frame-height :type integer :initform 300
		 :metadata (:sync t :json-name "frameHeight"))
   (msg-from-kernel :accessor msg-from-kernel :type unicode :initform "{}"
		    :metadata (:sync t :json-name "msgFromKernel"))
   (msg-from-browser :accessor msg-from-browser :type unicode :initform "{}"
		     :observers (msg-arrived)
		     :metadata (:sync t :json-name "msgFromBrowser"))
   (incoming-message :accessor incoming-message :type list :initform nil)
   (status :accessor status :type String :initform "initial status message\n")
   (selected-nodes :accessor selected-nodes :type list :initform nil))
  (:default-initargs
   :view-name (unicode "CyjsView")
    :view-module (unicode "cyjs"))
  (:metaclass traitlets:traitlet-class))

(defmethod add-graph ((self cyjs) g)
  (reset-message self)
  (let ((gjson (from-igraph self g)))
    (setf (msg-from-kernel self) (myjson::encode-json-to-string (list (cons "cmd" "addGraph")
								(cons "status" "request")
								(cons "callback" "")
								(cons "payload" gjson)))))
  (values))

(defmethod add-graph-with-layout ((self cyjs) g layout)
  (reset-message self)
  (let ((gjson (from-igraph self g :layout layout)))
    (setf (msg-from-kernel self) (myjson::encode-json-to-string (list (cons "cmd" "addGraph")
								(cons "status" "request")
								(cons "callback" "")
								(cons "payload" gjson)))))
  (values))

(defmethod delete-graph ((self cyjs))
  (reset-message self)
  (setf (msg-from-kernel self) (myjson::encode-json-to-string (list (cons "cmd" "deleteGraph")
								    (cons "status" "request")
								    (cons "callback" "")
								    (cons "payload" ""))))
  (values))

(defmethod from-igraph ((self cyjs) igrpah-network &key (layout nil) (scale 100.0)))

(defmethod set-height ((self cyjs) new-height)
  (setf (frame-height self) new-height)
  (values))

(defmethod fit-selected ((self cyjs) &key (margin 50))
  (setf (status self) (concatenate 'string  (status self) "entering fitSelected (" (write-to-string margin) ")\n"))
  (setf (msg-from-kernel self) (myjson::encode-json-to-string (list (cons "cmd" "fitSelected")
								    (cons "status" "request")
								    (cons "callback" "")
								    (cons "payload" margin))))
  (values))

(defmethod fit ((self cyjs) &key (margin 50))
  (setf (status self) (concatenate 'string (status self) "entering fit (" (write-to-string margin) ")\n"))
  (setf (msg-from-kernel self) (myjson::encode-json-to-string (list (cons "cmd" "fit")
							       (cons "status" "request")
							       (cons "callback" "")
							       (cons "payload" margin))))
  (values))

(defmethod get-selected-nodes ((self cyjs))
  (selected-nodes self))

(defmethod selectNodes ((self cyjs) nodes)
  (setf (msg-from-kernel self) (myjson::encode-json-to-string (list (cons "cmd" "selectNodes")
							      (cons "status" "request")
							      (cons "callback" "")
							      (cons "payload" nodes)))))

(defmethod reset-message ((self cyjs));ensures that any ensuing method is seen as novel in the browser
  (setf (msg-from-kernel self) (myjson::encode-json-to-string (list (cons "cmd" "cleanSlate")
							      (cons "status" "nop")
							      (cons "callback" "")
							      (cons "payload" ""))))
  (values))

(defmethod available-layouts ((self cyjs))
  (list "grid" "null" "random" "cose" "circle" "concentric" "breadthfirst"))

(defmethod set-position ((self cyjs) igraph-layout) ; The somewhat cryptic object created by igraph layouts
  (let ((tbl-pos nil))
    (loop for i upto (length igraph-layout)
       do
	 (let ((x (* (aref igraph-layout i 0) 100))
	       (y (* (aref igraph-layout i 1) 100)))
	 (push (list (cons "id" i)
		     (cons "x" x)
		     (cons "y" y))
	       tbl-pos)));;;In python, tbl-pos is a list which contains a dict. So in Lisp, tbl-pos is a list which contains an alist.
    (reset-message self)
    (setf (msg-from-kernel self) (myjson::encode-json-to-string (list (cons "cmd" "setPosition")
								      (cons "status" "request")
								      (cons "callback" "")
								      (cons "payload" tbl-pos)))))
  (values))

(defmethod load-style-file ((self cyjs) filename)
  (when (open filename)
    (setf (msg-from-kernel self) (myjson::encode-json-to-string (list (cons "cmd" "loadStyleFile")
								      (cons "status" "request")
								      (cons "callback" "")
								      (cons "payload" filename)))))
  (values))

(defmethod layout-cyjs ((self cyjs) name)
  (reset-message self)
  (setf (msg-from-kernel self) (myjson::encode-json-to-string (list (cons "cmd" "layout")
								    (cons "status" "request")
								    (cons "callback" "")
								    (cons "payload" name))))
  (values))

(defmethod clear-selection ((self cyjs))
  (reset-message self)
  (setf (msg-from-kernel self) (myjson::encode-json-to-string (list (cons "cmd" "clearSelection")
								    (cons "status" "request")
								    (cons "callback" "")
								    (cons "payload" ""))))
  (values))

;;;g parameter unused, temporarily establishes symmetry with setEdgeAttributes
(defmethod set-node-attributes ((self cyjs) g attribute-name node-names values)
  (reset-message self)
  (setf (msg-from-kernel self) (myjson::encode-json-to-string (list (cons "cmd" "setNodeAttributes")
								    (cons "status" "request")
								    (cons "callback" "")
								    (cons "payload" (list (cons "attributeName" attribute-name)
											  (cons "nodeNames" node-names)
											  (cons "values" values))))))
  (values))


(defmethod set-edge-attributes ((self cyjs) g attribute-name source-names target-names edge-types values))


;;;Observer for msg-from-browser
(defmethod msg-arrived (object name new old)
  (setf (status self)
	(concatenate 'string (status self) "msg-from-browser has arrived at time" (write-to-string (get-decoded time)) "\n"))
  (let ((raw-message new))
    (setf (incoming-message self) (myjson::parse-json-string raw-message))
    (let ((cmd (cdr (assoc "cmd" (incoming-message self) :test #'string=))))
      (setf (status self) (concatenate 'string (status self) "msg: " cmd "\n"))
      (when (string= cmd "updateSelectedNodes")
	(setf (selected-nodes self) (cdr (assoc "payload" (incoming-message self) :test #'string=))))))
  (values))
	

(defmethod get-response ((self cyjs))
  (cdr (assoc "payload" (incoming-message self) :test #'string=)))

(defmethod get-full-response ((self cyjs))
  (incoming message self))

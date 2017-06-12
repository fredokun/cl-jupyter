(in-package #:cl-jupyter-widgets)

(defvar *kernel* nil
  "Set to this to the current kernel")
(defvar *kernel-start-hook* nil
  "Set this to a function to invoke a callback whenever a kernel is started")
(defvar *kernel-shutdown-hook* nil
  "Set this to a function to invoke a callback whenever a kernel is shutdown")
(defvar *handle-comm-open-hook* nil
  "Set this to a function to invoke a callback whenever an comm_open message is received")
(defvar *handle-comm-msg-hook* nil
  "Set this to a function to invoke a callback whenever an comm_msg message is received")
(defvar *handle-comm-close-hook* nil
  "Set this to a function to invoke a callback whenever an comm_close message is received")



(defun kernel-start-hook (kernel)
  (widget-log "In kernel-start-hook kernel -> ~a~%" kernel)
  (let ((comm-manager (make-instance 'comm-manager :kernel kernel)))
    (setf (gethash kernel *kernel-comm-managers*) comm-manager)))

(defun kernel-shutdown-hook (kernel)
  (widget-log "In kernel-shutdown-hook kernel -> ~a~%" kernel)
  (let ((comm-manager (gethash kernel *kernel-comm-managers*)))
    (if comm-manager
	(remhash kernel *kernel-comm-managers*)
	(warn "The kernel ~a was shutdown but no comm-manager could be found for it" kernel))))


(defun handle-comm-open (shell identities msg buffers)
  (widget-log "[Shell] handling 'comm_open' - parsing message~%")
  (cl-jupyter::send-status-update (cl-jupyter::kernel-iopub (cl-jupyter::shell-kernel shell)) msg "busy" :key (cl-jupyter::kernel-key shell))'
  (widget-log "[Shell] done sending busy~%")
  (unwind-protect
       (progn
	 (widget-log "[Shell] Parsing message~%")
	 (let ((content (myjson:parse-json-from-string (cl-jupyter::message-content msg))))
	   (widget-log "  ==> msg = ~W~%" msg)
	   (widget-log "  ==> comm_open Message content = ~W~%" content)
	   (let* ((kernel (cl-jupyter::shell-kernel shell))
		  (manager (gethash kernel *kernel-comm-managers*)))
	     ;; Should I pass identities for ident??????
	     ;; I have no idea what the stream is
	     (comm-open manager :I-dont-know-what-to-pass-for-stream :i-dont-know-what-to-pass-for-ident content))))
    (widget-log "    Unwounding after parse-json-from-string or comm-open~%"))
  ;; status back to idle
  (cl-jupyter::send-status-update (cl-jupyter::kernel-iopub (cl-jupyter::shell-kernel shell)) msg "idle" :key (cl-jupyter::kernel-key shell)))

(defun handle-comm-msg (shell identities msg buffers)
  (widget-log "[Shell] handling 'comm_msg' - parsing message~%")
  (cl-jupyter::send-status-update (cl-jupyter::kernel-iopub (cl-jupyter::shell-kernel shell)) msg "busy" :key (cl-jupyter::kernel-key shell))'
  (widget-log "[Shell] done sending busy~%")
  (unwind-protect
       (progn
	 (widget-log "[Shell] Parsing message~%")
	 (let ((content (myjson:parse-json-from-string (cl-jupyter::message-content msg))))
	   (widget-log "  ==> msg = ~W~%" msg)
	   (widget-log "  ==> comm_msg Message content = ~W~%" content)
	   (let* ((kernel (cl-jupyter::shell-kernel shell))
		  (manager (gethash kernel *kernel-comm-managers*)))
	     ;; Should I pass identities for ident??????
	     ;; I have no idea what the stream is
	     (comm-msg manager :I-dont-know-what-to-pass-for-stream :i-dont-know-what-to-pass-for-ident content))))
    (widget-log "    Unwounding after parse-json-from-string or comm-msg~%"))
  ;; status back to idle
  (cl-jupyter::send-status-update (cl-jupyter::kernel-iopub (cl-jupyter::shell-kernel shell)) msg "idle" :key (cl-jupyter::kernel-key shell)))

(defun handle-comm-close (shell identities msg buffers)
  (widget-log "[Shell] handling 'comm_close' - parsing message~%")
  (cl-jupyter::send-status-update (cl-jupyter::kernel-iopub (cl-jupyter::shell-kernel shell)) msg "busy" :key (cl-jupyter::kernel-key shell))'
  (widget-log "[Shell] done sending busy~%")
  (unwind-protect
       (progn
	 (widget-log "[Shell] Parsing message~%")
	 (let ((content (myjson:parse-json-from-string (cl-jupyter::message-content msg))))
	   (widget-log "  ==> msg = ~W~%" msg)
	   (widget-log "  ==> comm_close Message content = ~W~%" content)
	   (let* ((kernel (cl-jupyter::shell-kernel shell))
		  (manager (gethash kernel *kernel-comm-managers*)))
	     ;; Should I pass identities for ident??????
	     ;; I have no idea what the stream is
	     (comm-close manager :I-dont-know-what-to-pass-for-stream :i-dont-know-what-to-pass-for-ident content))))
    (widget-log "    Unwinding after parse-json-from-string or comm-close~%"))
  ;; status back to idle
  (cl-jupyter::send-status-update (cl-jupyter::kernel-iopub (cl-jupyter::shell-kernel shell)) msg "idle" :key (cl-jupyter::kernel-key shell)))


(eval-when (:load-toplevel :execute)
  (setf *kernel-start-hook* #'kernel-start-hook)
  (setf *kernel-shutdown-hook* #'kernel-shutdown-hook)
  (setf *handle-comm-open-hook* #'handle-comm-open)
  (setf *handle-comm-msg-hook* #'handle-comm-msg)
  (setf *handle-comm-close-hook* #'handle-comm-close)
  )

(defun send-comm-open (content)
  (let* ((msg (cl-jupyter::make-message cl-jupyter::*parent-msg* "comm_open" nil content))
	 (shell cl-jupyter::*shell*))
    #++(let ((json-str (encode-json-to-string content :indent 4)))
      (format t "Sending comm_open~%")
      (format t "parent-msg -> ~s~%" *parent-msg*)
      (format t "content:   ~s~%" content)
      (format t "json:  ---> ~%")
      (format t "~s~%" json-str))
    (cl-jupyter::message-send
     (cl-jupyter::iopub-socket (cl-jupyter::kernel-iopub (cl-jupyter::shell-kernel shell)))
     msg
     :identities '("comm_open")
     :key (cl-jupyter::kernel-key shell))))

(defun send-comm-msg (content)
  (let* ((msg (cl-jupyter::make-message cl-jupyter::*parent-msg* "comm_msg" nil content))
	 (shell cl-jupyter::*shell*))
    #++(let ((json-str (encode-json-to-string content :indent 4)))
      (format t "Sending comm_msg~%")
      (format t "parent-msg -> ~s~%" *parent-msg*)
      (format t "content:   ~s~%" content)
      (format t "json:  ---> ~%")
      (format t "~s~%" json-str))
    (cl-jupyter::message-send
     (cl-jupyter::iopub-socket (cl-jupyter::kernel-iopub (cl-jupyter::shell-kernel shell)))
     msg
     :identities '("comm_msg")
     :key (cl-jupyter::kernel-key shell))))

(in-package #:cl-jupyter)

#|

# The control router socket #

|#


(defvar *control-parent-msg* nil)
(defvar *control* nil)

(defclass control-channel (shell-channel)
  ())


(defun make-control-channel (kernel)
  (let ((socket (pzmq:socket (kernel-ctx kernel) :router)))
    (let ((control (make-instance 'control-channel
                                :kernel kernel
                                :socket socket)))
      (let ((config (slot-value kernel 'config)))
        (let ((endpoint (format nil "~A://~A:~A"
                                  (config-transport config)
                                  (config-ip config)
                                  (config-control-port config))))
          ;; (jformat t "control endpoint is: ~A~%" endpoint)
          (pzmq:bind socket endpoint)
          control)))))

(defparameter *control-receive-lock* (bordeaux-threads:make-lock "control-receive-lock"))

(defun control-loop (control shell-thread)
  (jformat t "[Control] Starting control-loop~%")
  (logg 2 "[Control] Starting control-loop~%")
  (bordeaux-threads:with-lock-held (*control-receive-lock*)
    (let ((active t))
;;;           (send-status-starting (kernel-iopub (kernel control)) (kernel-session (kernel control)) :key (kernel-key control))
      (while active
             (logg 1 "[Control] top of main loop~%")
             (vbinds (identities sig msg)  (message-recv control)
                     (logg 1 "[Control] Received message ~s~%" msg)
                     (progn
                       (logg 2 "Control Received message:~%")
                       (logg 2 "  | identities: ~A~%" identities)
                       (logg 2 "  | signature: ~W~%" sig)
                       (logg 2 "  | message: ~A~%" (encode-json-to-string (message-header msg)))
                       (logg 2 "  | number of buffers: ~W~%" (length (message-buffers msg))))
                     ;; TODO: check the signature (after that, sig can be forgotten)
                     (let* ((msg-type (header-msg-type (message-header msg))))
                       (let ((*control-parent-msg* msg)
                             (*control* control))
                         (logg 2 "  |  *control-parent-msg* -> ~s~%" *control-parent-msg*)
                         (cond ((equal msg-type "shutdown_request")
                                (setf active (handle-control-shutdown-request control identities msg)))
                               ((equal msg-type "interrupt_request")
                                (handle-control-interrupt-request control identities msg shell-thread))
                               (t (warn "[Control] message type '~A' not (yet ?) supported, skipping... msg: ~s" msg-type msg))))))))))


#|

### Message type: kernel_info_reply ###

|#





(defun handle-control-shutdown-request (control identities msg)
  "Handle inspect_request.  This provides Shift-Tab completion to cl-jupyter.
This should be improved so that if the cursor is inside of a form it returns information
on the function of the form.  Currently it provides information on the function associated
with the symbol to the left of the cursor."
  (send-status-update (kernel-iopub (kernel control)) msg "busy" :key (kernel-key control))
  (logg 2 "[Control] handle-control-shutdown-request~%")
  (jformat t "[Control] handle-control-shutdown-request~%")
  (send-status-update (kernel-iopub (kernel control)) msg "idle" :key (kernel-key control)))


(defun handle-control-interrupt-request (control identities msg shell-thread)
  "Handle inspect_request.  This provides Shift-Tab completion to cl-jupyter.
This should be improved so that if the cursor is inside of a form it returns information
on the function of the form.  Currently it provides information on the function associated
with the symbol to the left of the cursor."
  (send-status-update (kernel-iopub (kernel control)) msg "busy" :key (kernel-key control))
  (logg 2 "[Control] handle-control-interrupt-request thread: ~a~%" shell-thread)
  (jformat t "[Control] handle-control-interrupt-request thread: ~a~%" shell-thread)
  (bordeaux-threads:interrupt-thread shell-thread
                                     (lambda ()
                                       (jformat t "About to throw interrupt-shell~%")
                                       (logg-backtrace "Throwing interrupt-shell backtract~%~a~%"
                                             (with-output-to-string (sout)
                                               (let ((*print-pretty* nil))
                                                 (trivial-backtrace:print-backtrace-to-stream sout))))
                                       (throw 'interrupt-shell nil)))
  (let ((reply (make-message msg "interrupt_reply" nil nil)))
    (message-send control reply :identities identities :key (kernel-key control)))
  (send-status-update (kernel-iopub (kernel control)) msg "idle" :key (kernel-key control)))


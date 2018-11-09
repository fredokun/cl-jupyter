(in-package #:cl-jupyter)

#|

# The IOPUB publish/subscribe channel #

|#

(defclass iopub-channel (shell-channel) ())

(defun make-iopub-channel (kernel)
  (let ((socket (pzmq:socket (kernel-ctx kernel) :pub)))  
    (let ((iopub (make-instance 'iopub-channel
                                :kernel kernel
                                :socket socket)))
      (let ((config (slot-value kernel 'config)))
        (let ((endpoint (format nil "~A://~A:~A"
                                  (config-transport config)
                                  (config-ip config)
                                  (config-iopub-port config))))
          ;;(jformat t "[IOPUB] iopub endpoint is: ~A~%" endpoint)
          (pzmq:bind socket endpoint)
	  (setf (slot-value kernel 'iopub) iopub)
          iopub)))))

(defun send-status-starting (iopub session &key (key nil))
  (let ((status-msg (make-orphan-message session "status" nil
					 `(("execution_state" . "starting")) #())))
    (logg 2 "[iopub] Made orphan message: ~s~%" status-msg)
    #+(or)(jformat t "[iopub] Made orphan message: ~s~%" status-msg)
    (prog1
	(message-send iopub status-msg :identities (list (fredokun-utilities:string-to-octets "status")) :key key)
      (logg 2 "[iopub] Leaving send-status-starting~%"))))

(defun send-status-update (iopub parent-msg status &key (key nil))
  (let ((status-content `((:execution--state . ,status))))
    (let ((status-msg (make-message parent-msg "status" nil
				    `(("execution_state" . ,status)))))
      (message-send iopub status-msg :identities (list (fredokun-utilities:string-to-octets "status")) :key key))))

(defun send-execute-code (iopub parent-msg execution-count code &key (key nil))
  (let ((code-msg (make-message  parent-msg "execute_input" nil
				 `(("code" . ,code)
				   ("execution_count" . ,execution-count)))))
    ;;(jformat t "content to send = ~W~%" (encode-json-to-string (message-content code-msg)))
    (message-send iopub code-msg :identities (list (fredokun-utilities:string-to-octets "execute_input")) :key key)))

(defun send-execute-raw-display-object (iopub parent-msg execution-count display-obj &key (key nil))
  (logg 2 "iopub.lisp::send-execute-raw-display-object    display-obj -> ~s~%" display-obj)
  (let ((result-msg (make-message parent-msg "execute_result" nil
                                  `(("execution_count" . ,execution-count)
                                    ("data" . ,(display-object-data display-obj))
                                    ("metadata" . ())))))
    (message-send iopub result-msg :identities (list (fredokun-utilities:string-to-octets "execute_result")) :key key)))

(defun send-execute-result (iopub parent-msg execution-count result &key (key nil))
  (logg 2 "iopub.lisp::send-execute-result    result -> ~s~%" result)
  (let ((display-obj (display result)))
    (send-execute-raw-display-object iopub parent-msg execution-count display-obj :key key)))


(defun send-stream (iopub parent-msg stream-name data &key (key nil))
  (let ((stream-msg (make-message parent-msg "stream" nil
				  `(("name" . ,stream-name)
				    ("text" . ,data )))))
    (message-send iopub stream-msg :identities (list (fredokun-utilities:string-to-octets (format nil "stream.~W" stream-name))) :key key)))

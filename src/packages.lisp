(defpackage #:fredokun-utilities
  (:nicknames #:fredo-utils)
  (:use #:cl)
  (:export #:[]
           #:[]-contains
           #:current-date-time
           #:*example-enabled*
           #:*example-equal-predicate*
           #:example
           #:example-progn
           #:*logg-enabled*
           #:*logg-level*
           #:logg
           #:vbinds
           #:afetch
	   #:while
	   #:read-file-lines
	   #:read-string-file
	   #:read-binary-file
           #:function-lambda-list
           #:jformat
           #:getpid
	   #:quit))

(defpackage #:myjson
  (:use #:cl #:fredo-utils)
  (:export #:parse-json
	   #:parse-json-from-string
	   #:encode-json
	   #:encode-json-to-string
           #:dumps))

(defpackage #:cl-jupyter
  (:use #:cl #:fredo-utils #:myjson)
  (:export
   #:send-execute-raw-display-object ; cl-jupyter-widgets uses this
   #:make-message
   #:display-object
   #:display-object-data
   #:display
   #:display-plain render-plain
   #:display-html render-html
   #:display-markdown render-markdown
   #:display-latex render-latex
   #:display-png render-png
   #:display-jpeg render-jpeg
   #:display-svg render-svg
   #:display-json render-json
   #:display-javascript render-javascript
   #:header-msg-type
   #:message-send
   #:message-header
   #:message-content
   #:message-buffers
   #:message
   #:socket
   #:kernel
   #:*shell*
   #:*kernel*
   #:*parent-msg*
   #:*default-special-bindings*
   #:*special-variables*
   #:take-history-in
   #:take-history-out
   #:shell-interrupt
   #:history-in
   #:history-out
   #:kernel-start
   #:kernel-session
   #:kernel-iopub
   #:kernel-key
   #:kernel-shell
   #:send-status-update
   #:[]
   #:[]-contains
   #:*started-kernels*
   #:*kernel-start-hook*
   #:*kernel-shutdown-hook*
   #:*sort-encoded-json*
   #:*evaluator*
   #:*handle-comm-open-hook*
   #:*handle-comm-msg-hook*
   #:*handle-comm-close-hook*
   #:*cl-jupyter-widget-display-hook*
   #:logg
   ))

(defpackage #:cl-jupyter-user
  (:use #:cl #:fredo-utils #:cl-jupyter #:common-lisp-user)
  (:export 
   #:display
   #:display-plain render-plain
   #:display-html render-html
   #:display-markdown render-markdown
   #:display-latex render-latex
   #:display-png render-png
   #:display-jpeg render-jpeg
   #:display-svg render-svg
   #:display-json render-json
   #:display-javascript render-javascript
   #:html #:latex #:svg
   #:png-from-file
   #:svg-from-file
   #:quit))

(in-package #:cl-jupyter)

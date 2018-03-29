(cljw:widget-log "nglview  Loading packages.lisp~%")

(defpackage "CLEXT.QUEUE"
  (:use "COMMON-LISP"
        "BORDEAUX-THREADS")
  (:export "QUEUE" "QUEUEP"
           "MAKE-QUEUE"
           "QUEUE-NAME"
           "QUEUE-COUNT"
           "QUEUE-EMPTYP"
           "ENQUEUE"
           "DEQUEUE"
           "TEST-QUEUE")
  (:documentation "Implements a thread-safe message queue."))

(defpackage #:nglv
  (:use #:cl)
  (:shadow #:count #:structure)
  (:export
   #:nglwidget
   #:shape
   #:remote-call-thread-queue
   #:wait-until-finished
   #:show-aggregate
   )
  (:import-from :fredokun-utilities #:[] #:[]-contains))

(defpackage #:pythread
  (:use #:cl)
  (:shadow #:set)
  (:export
   #:remote-call-callback
   #:make-remote-call-callback
   #:callback
   #:method-name
   #:fire-callback
   #:remote-call-add
   #:remote-call-thread-run
   #:event
   #:event-set
   #:clear
   #:is-set
   #:*remote-call-thread*
   #:*remote-call-thread-queue*))

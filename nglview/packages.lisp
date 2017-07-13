(cljw:widget-log "nglview  Loading packages.lisp~%")

(defpackage #:nglv
  (:use #:cl)
  (:shadow #:count #:structure)
  (:export
   #:nglwidget
   #:shape
   #:remote-call-thread-queue
   #:wait-until-finished
   )
  )

(defpackage #:pythread
  (:use #:cl)
  (:shadow #:set)
  (:export
   #:remote-call-add
   #:remote-call-thread-run
   #:event
   #:set
   #:clear
   #:is-set))

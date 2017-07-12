(cljw:widget-log "nglview  Loading packages.lisp~%")

(defpackage #:nglv
  (:use #:cl)
  (:shadow #:count #:structure)
  (:export
   #:nglwidget
   #:shape
   )
  )

(defpackage #:pythread
  (:use #:cl)
  (:shadow #:set)
  (:export
   #:event
   #:set
   #:clear
   #:is-set))

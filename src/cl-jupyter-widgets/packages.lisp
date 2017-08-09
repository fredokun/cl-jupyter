
(defpackage #:cl-jupyter-widgets
  (:nicknames #:cljw)
  (:use #:cl)
  (:shadow #:open #:close #:step #:min #:max)
  (:export
   #:*kernel-start-hook*
   #:*kernel-shutdown-hook*
   #:*handle-comm-open-hook*
   #:*handle-comm-msg-hook*
   #:*handle-comm-close-hook*
   #:*send-updates*
   #:notify-change
   #:do-ipython-display
   #:widget
   #:widget-open
   #:widget-send
   #:widget-close
   #:domwidget
   #:int-slider
   #:image
   #:bool
   #:dict
   #:unicode
   #:cunicode
   #:tuple
   #:color
   #:instance
   #:on-msg
   #:on-displayed
   #:assoc-value
   ))

(defpackage #:traitlets
  (:use #:cl)
  (:export #:traitlet-class #:synced-object)
  (:export #:traitlet-metadata
	   #:effective-traitlet))

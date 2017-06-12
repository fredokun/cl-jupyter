
(in-package #:cl-jupyter-widgets)


(defmacro widget-log (fmt &rest args)
  `(progn
     (format *error-output* ,fmt ,@args)
     (finish-output *error-output*)))

#+(or)
(defmacro widget-log (fmt &rest args)
  nil)



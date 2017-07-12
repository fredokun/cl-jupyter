(in-package :nglv)

(cljw:widget-log "nglview  Loading utils.lisp~%")


(defun get-name (obj &rest args &key (name nil name-p) &allow-other-keys)
  (if name-p
      name
      (let ((raw-name (format nil "~a" obj)))
	raw-name)))


#||
def get_name(obj, kwargs):
    name = kwargs.pop('name', str(obj))
    if name.startswith('<nglview.'):
        name = name.split()[0].strip('<')
    return name
    
||#

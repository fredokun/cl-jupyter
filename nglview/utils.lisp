(in-package :nglv)

(cljw:widget-log "nglview  Loading utils.lisp~%")


(defun get-name (obj &rest args &key (name nil name-p) &allow-other-keys)
  (if name-p
      name
      (let ((raw-name (format nil "~a" obj)))
	raw-name)))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
     :while end))

(defun camelize (snake)
  (let ((split-snake (my-split snake :delimiterp (lambda (c) (char= c #\_)))))
    (with-output-to-string (sout)
      (princ (car split-snake) sout)
      (loop for part in (cdr split-snake)
	   do (princ (string-capitalize part) sout)))))

(defun camelize-dict (alist)
  (mapcar (lambda (x) (cons (camelize (car x)) (cdr x))) alist))
	 

(defun seq-to-string (seq)
  "e.g. convert [1, 3, 5] to \"@1,3,5\"
"
  (cond
    ((stringp seq)
     seq)
    ((vectorp seq)
      (with-output-to-string (sout)
	(princ #\@ sout)
	(loop for x across seq
	   for sep = "" then ","
	   do (princ sep sout)
	   do (princ x sout))))
    (t (error "Handle seq-to-string for ~a" seq))))

#||
def get_name(obj, kwargs):
    name = kwargs.pop('name', str(obj))
    if name.startswith('<nglview.'):
        name = name.split()[0].strip('<')
    return name
    
||#

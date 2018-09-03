
(in-package #:fredokun-utilities)

#+(or)
(declaim (notinline []))
#+(or)
(defun [] (table key &optional (default nil defaultp))
  (format t "table-> ~s key -> ~s defaultp -> ~s~%" table key defaultp)
  (let ((pair (assoc key table :test #'equal)))
    (if pair
        (cdr pair)
        (if defaultp
            default
            (error "Could not find key ~a in dict ~a" key table)))))


(macrolet
    ((define-alist-get (name get-entry get-value-from-entry add doc)
       `(progn
          (declaim (notinline ,name))
          (defun ,name (alist key &optional (default nil defaultp) &aux (test 'string=))
            ,doc
            (let ((entry (,get-entry key alist :test test)))
              (if entry
                  (values (,get-value-from-entry entry) entry)
                  (if defaultp
                      default
                      (error "Could not find key ~a in alist ~a" key alist)))))
          (define-setf-expander ,name (place key &key (test ''string=)
                                       &environment env)
            (multiple-value-bind
                  (temporary-variables initforms newvals setter getter)
                (get-setf-expansion place env)
              (when (cdr newvals)
                (error "~A cannot store multiple values in one place" ',name))
              (let ((new-value (gensym "new-value"))
                    (key-val (gensym "key-value"))
                    (test-val (gensym "test-val"))
                    (alist (gensym "alist"))
                    (entry (gensym "entry")))
                (values
                 (append temporary-variables
                         (list alist
                               key-val
                               test-val
                               entry))
                 (append initforms
                         (list getter
                               key
                               test
                               `(,',get-entry ,key-val ,alist :test ,test-val)))
                 `(,new-value)
                 `(cond
                    (,entry
                     (setf (,',get-value-from-entry ,entry) ,new-value))
                    (t
                     (let ,newvals
                       (setf ,(first newvals) (,',add ,key ,new-value ,alist))
                       ,setter
                       ,new-value)))
                 `(,',get-value-from-entry ,entry))))))))
  (define-alist-get [] assoc cdr acons
    "ASSOC-VALUE is an alist accessor very much like ASSOC, but it can
be used with SETF."))


(defparameter *base-string-to-octets-calls* 0)
(defparameter *wide-string-to-octets-calls* 0)
(defparameter *string-to-octets-calls* 0)
(defun string-to-octets (string &key (encoding babel:*default-character-encoding* encodingp))
  #+clasp
  (cond
    ((and (null encodingp) (core:base-string-p string))
     (incf *base-string-to-octets-calls*)
     (core:base-string-to-octets string))
    ((and (null encodingp) (core:fits-in-base-string string))
     (incf *wide-string-to-octets-calls*)
     (core:character-string-that-fits-in-base-string-to-octets string))
    (t
     (incf *string-to-octets-calls*)
     (babel:string-to-octets string :encoding encoding)))
  #-clasp
  (babel:string-to-octets string :encoding encoding))


#+(or)  
(define-setf-expander [] (table key &optional (default 'nil defaultp) &environment env)
  (multiple-value-bind (temps inits new set get)
      (get-setf-expansion table env)
    (when (cdr new)
      (error "~A cannot store multiple values in one place" '[]))
    (let ((nv (gensym))
          (elem-temp (gensym "ELEM")) (alist-temp (gensym "ALIST"))
          (pair-temp (gensym "PAIR")) (default-temp (gensym "DEFAULT")))
      (values
       (append temps (list elem-temp alist-temp) (when defaultp (list default-temp))
               (list pair-temp))
       (append inits (list key get) (when defaultp (list default))
               (list `(assoc ,elem-temp ,alist-temp :test #'equal)))
       (list nv)
       `(cond ((null ,pair-temp)
               ,(if defaultp
                    `(let ,new
                       ;; progn to make the temp "used" and avoid the style warning.
                       (setf ,(first new) (acons ,elem-temp (progn ,default-temp ,nv) ,alist-temp))
                       ,set
                       ,nv)
                    `(error "Could not find key ~a in dict ~a" ,elem-temp ,alist-temp)))
              (t (setf (cdr ,pair-temp) ,nv)))
       `(cond ((null ,pair-temp)
               ,(if defaultp
                    default
                    `(error "Could not find key ~a in dict ~a" ,elem-temp ,alist-temp)))
              (t (cdr ,pair-temp)))))))

(defun []-contains (table key)
  (let ((pair (assoc key table :test #'equal)))
    pair))

(defun getpid ()
  "Return the pid of the current process"
  #+clasp(core:getpid)
  #+sbcl(sb-posix:getpid))

(defun current-date-time ()
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (format nil "~4d-~02,'0d-~02,'0dT~02,'0d:~02,'0d:~02,'0d" year month date hour minute second)))

#|

# CommonTypes: Utilities #

|#

;; To activate the inline examples
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *example-enabled* t) ;; nil in production / t for self-testing
  (defparameter *example-equal-predicate* #'equal)
  (defparameter *example-with-echo* nil)
 
  )

(defparameter *jformat-lock* (bordeaux-threads:make-lock "format-lock"))

(defmacro jformat (stream fmt &rest args)
  `(bordeaux-threads:with-lock-held (*jformat-lock*)
     (format ,stream ,fmt ,@args)))

(defmacro example (expr arrow expected &key (warn-only nil))
  "Show an evaluation example, useful for documentation and lightweight testing.

   (example `EXPR` => `EXPECTED`) evaluates `EXPR` and compare, wrt. `EQUIV`
 (EQUAL by default) to `EXPECTED` and raise an error if inequal.

  Set `WARN-ONLY` to T for warning instead of error.
"
  (if (not *example-enabled*)
      (progn
        (when *example-with-echo*
          (format t "------------------~%")
          (format t "Example:~%~A~%=?= ~A~%" (format nil "~A" expr) expected)
          (format t "  ===> SKIP~%"))
        (values));; synonymous of nil if disabled
      ;; when enabled
      (let ((result-var (gensym "result-"))
            (expected-var (gensym "expected-"))
            (err-fun-var (gensym "err-fun-"))
            (expr-str (format nil "~A" expr)))
        `(progn
           (when *example-with-echo*
             (format t "------------------~%")
             (format t "Example:~%~A~%=?= ~A~%" ,expr-str ,expected))
           (let ((,err-fun-var (if ,warn-only #'warn #'error))
                 (,result-var ,expr)
                 (,expected-var ,expected))
             (if (not (equal (symbol-name (quote ,arrow)) "=>"))
                 (error "Missing arrow '=>' in example expression"))
             (if (funcall *example-equal-predicate* ,result-var ,expected-var)
                 (progn (if *example-with-echo*
                            (format t "  ===> PASS~%"))
                        t)
                 (funcall ,err-fun-var "Failed example:~%  Expression: ~S~%  ==> expected: ~A~%  ==> evaluated: ~A~%"
                          ,expr-str ,expected-var ,result-var)))))))


(defmacro example-progn (&body body)
  "The toplevel forms of BODY are evaluated only if examples are enabled"
  (if *example-enabled*
      `(progn ,@body)
      (values)))

(defparameter *log-enabled* nil)
(defparameter *log-level* nil)
(defparameter *log-out-stream* nil)

(defparameter *log-file* nil)
(defparameter *log-lock* nil)

;;; Comment out the following eval-when if you want logging fully disabled
#+(or)
(eval-when (:execute :load-toplevel :compile-toplevel)
  (format t "Turning on cl-jupyter logging~%")
  (push :cl-jupyter-log *features*)
  (setf *log-enabled* t)
  (setf *log-level* 1))

#+cl-jupyter-log
(eval-when (:execute :load-toplevel)
  (let* ((log-file-name (make-pathname :name (format nil "cl-jupyter-~a" (getpid))
                                       :type "log"))
         (log-path-name (cond
                          ((probe-file "/home/app/logs/")
                           (merge-pathnames log-file-name #P"/home/app/logs/"))
                          (t (merge-pathnames log-file-name #P"/tmp/")))))
    (setf *log-file* (cl:open log-path-name
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :create))
    (setf *trace-output* *log-file*)
    (format *log-file* "Log started up~%")
    (setf *log-lock* (bordeaux-threads:make-lock "cl-jupyter-log"))
    (format *log-file* "About to start logging cl-jupyter~%")
    (defun always-log (fmt &rest args)
      (let ((msg (apply #'format nil fmt args)))
        (bordeaux-threads:with-lock-held (*log-lock*)
          (if (> (length msg) 1024)
              (progn
                (princ (subseq msg 0 1024) *log-file*)
                (princ "... output too long - terminating" *log-file*)
                (terpri *log-file*))
              (princ msg *log-file*))
          (finish-output *log-file*))))
    (format *log-file* "===================== new run =======================~%")))

#+cl-jupyter-log
(defmacro logg (level fmt &rest args)
  "Log the passed ARGS using the format string FMT and its
 arguments ARGS."
  (if (or (not *log-enabled*)
          (> level *log-level*))
      (values);; disabled
      ;; when enabled
      `(progn (always-log ,fmt ,@args))))

#-cl-jupyter-log
(defmacro logg (level fmt &rest args)
  nil)

(defmacro vbinds (binders expr &body body)
  "An abbreviation for MULTIPLE-VALUE-BIND."
  (labels ((replace-underscores (bs &optional (result nil) (fresh-vars nil) (replaced nil))
             (if (null bs)
                 (let ((nresult (nreverse result))
                       (nfresh (nreverse fresh-vars)))
                   (values replaced nresult nfresh))
                 (if (equal (symbol-name (car bs)) "_")
                     (let ((fresh-var (gensym "underscore-")))
                       (replace-underscores (cdr bs) (cons fresh-var result) (cons fresh-var fresh-vars) t))
                     (replace-underscores (cdr bs) (cons (car bs) result) fresh-vars replaced)))))
    (multiple-value-bind (has-underscore nbinders fresh-vars) (replace-underscores binders)
      (if has-underscore
          `(multiple-value-bind ,nbinders ,expr
             (declare (ignore ,@fresh-vars))
             ,@body)
          `(multiple-value-bind ,binders ,expr ,@body)))))

(example (vbinds (a _ b) (values 1 2 3)
           (cons a b))
         => '(1 . 3)) ;; without a warning

(example (vbinds (a _ b _) (values 1 2 3 4)
           (cons a b))
         => '(1 . 3)) ;; without a warning


(defun afetch (comp alist &key (test #'eql) (default nil defaultp))
  (let ((binding (assoc comp alist :test test)))
    (if binding
        (cdr binding)
        (if defaultp
            default
            (error "No such key: ~A in dict ~S" comp alist)))))

(defmacro while (condition &body body)
  (let ((eval-cond-var (gensym "eval-cond-"))
        (body-val-var (gensym "body-val-")))
    `(flet ((,eval-cond-var () ,`,condition))
       (do ((,body-val-var nil (progn ,@body)))
           ((not (,eval-cond-var))
            ,body-val-var)))))

(example (let ((count 0))
           (while (< count 10)
             ;;(jformat t "~A " count)
             (incf count)
             count))
         => 10)

(defun read-file-lines (filename)
  (with-open-file (input filename)
    (loop
       for line = (read-line input nil 'eof)
       until (eq line 'eof)
       collect line)))

(defun read-binary-file (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      bytes)))

(defun read-string-file (filename)
  (with-open-file (stream filename)
    (let ((str (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer str) (read-sequence str stream))
      str)))


;; Taken from Rob Warnock's post "How to programmatically exit?"
(defun quit (&optional code)
      ;; This group from "clocc-port/ext.lisp"
      #+allegro (excl:exit code)
      #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
      #+cmu (ext:quit code)
      #+cormanlisp (win32:exitprocess code)
      #+gcl (lisp:bye code)                     ; XXX Or is it LISP::QUIT?
      #+lispworks (lw:quit :status code)
      #+lucid (lcl:quit code)
      #+sbcl (sb-ext:exit :code code)
      ;; This group from Maxima
      #+kcl (lisp::bye)                         ; XXX Does this take an arg?
      #+scl (ext:quit code)                     ; XXX Pretty sure this *does*.
      #+(or openmcl mcl) (ccl::quit)
      #+abcl (cl-user::quit)
      #+(or ecl clasp) (si:quit)
      ;; This group from <hebi...@math.uni.wroc.pl>
      #+poplog (poplog::bye)                    ; XXX Does this take an arg?
      #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
            kcl scl openmcl mcl abcl ecl clasp)
      (error 'not-implemented :proc (list 'quit code))) 


(defmacro with-handling-errors (&body body)
  `(block error-handler
     (handler-bind
         ((warning
            #'(lambda (wrn)
                (format *error-output* "~&~A~%" wrn)
                (muffle-warning)
                (return-from error-handler)))
          (serious-condition
            #'(lambda (err)
                #+(or)(progn
                  (logg 0 "~a~%" (with-output-to-string (sout) (format sout "~&~A~%" err)))
                  (logg 0 "~a~%" (with-output-to-string (sout)
                                                        (let ((*print-pretty* nil))
                                   (trivial-backtrace:print-backtrace-to-stream sout)))))
                (format *error-output* "~&An error occurred of type: ~A: ~%  ~A~%~%"
                        (class-name (class-of err)) err)
                (format *error-output* "serious-condition backtrace:~%~A~%"
                        (with-output-to-string (sout)
                                               (let ((*print-pretty* nil))
                                                 (trivial-backtrace:print-backtrace-to-stream sout))))
                (return-from error-handler))))
       (progn ,@body))))

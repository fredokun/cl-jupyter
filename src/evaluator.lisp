(in-package #:cl-jupyter)

#|

# Evaluator #

The evaluator is where the "interesting stuff" takes place :
 user expressions are evaluated here.

The history of evaluations is also saved by the evaluator.

|#



(defclass output-stream (gray:fundamental-character-output-stream)
  ((contents :initform (make-array 256 :element-type 'base-char :adjustable t :fill-pointer 0) :accessor contents)
   (iopub :initarg :iopub :accessor iopub)
   (key :initarg :key :accessor key)
   (stream-name :initarg :stream-name :accessor stream-name)
   (parent-msg :initarg :parent-msg :accessor parent-msg))
  (:documentation "A stream that handles sending output to the jupyter when finish-output is called"))

(defmethod gray:stream-write-char ((s output-stream) c)
  (vector-push-extend c (contents s)))

(defmethod gray:stream-finish-output ((s output-stream))
  (with-standard-io-syntax
    (let ((*print-readably* nil)) ; turn this off so that log printing #<xxx> doesn't crash
      (send-stream (iopub s)
                   (parent-msg s)
                   (stream-name s)
                   (contents s)
                   :key (key s))))
  (setf (fill-pointer (contents s)) 0))

(defclass evaluator ()
  ((kernel :initarg :kernel :reader evaluator-kernel)
   (history-in :initform (make-array 64 :fill-pointer 0 :adjustable t)
	       :reader evaluator-history-in)
   (history-out :initform (make-array 64 :fill-pointer 0 :adjustable t)
		:reader evaluator-history-out)))

(defvar *evaluator* nil)

(defun take-history-in (hist-ref)
  (let ((history-in (slot-value cl-jupyter:*evaluator* 'cl-jupyter:history-in)))
    (let ((href (if (< hist-ref 0)
                    (+ (+ (length history-in) 1) hist-ref)
                  hist-ref)))
      (if (and (>= href 1)
               (<= href (length history-in)))
          (aref history-in (- href 1))
        nil))))

(defun take-history-out (hist-ref &optional value-ref)
  (let ((history-out (slot-value cl-jupyter:*evaluator* 'cl-jupyter:history-out)))
    (let ((href (if (< hist-ref 0)
                    (+ (+ (length history-out) 1) hist-ref)
                    hist-ref)))
      (when (and (>= href 1)
                 (<= href (length history-out)))
        (let ((out-values  (aref history-out (- href 1))))
          (if value-ref
              (when (and (>= value-ref 1)
                         (<= value-ref (length out-values)))
                (elt out-values (- value-ref 1)))
              (values-list out-values)))))))

(defun make-evaluator (kernel)
  (let ((evaluator (make-instance 'evaluator
                                  :kernel kernel)))
    (setf (slot-value kernel 'evaluator) evaluator)
    evaluator))

(defmacro handling-errors (&body body)
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

(defun evaluate-code (evaluator code &key iopub parent-msg key)
  ;; (jformat t "[Evaluator] Code to evaluate: ~W~%" code)
  (logg 2 "[Evaluator] Code to evaluate: ~W  cl-jupyter:*kernel*->~a~%"
        code cl-jupyter:*kernel*)
  (let ((execution-count (+ (length (evaluator-history-in evaluator)) 1)))
    (let ((code-to-eval (handler-case
                            (read-from-string (format nil "(progn ~A~%)" code))
                          (END-OF-FILE (err) (list :read-error-eof (format nil "~A" (class-name (class-of err)))))
                          #+clasp(condition (err)
                                   (list :read-error (format nil "~A (condition of type ~A)" err (class-name (class-of err)))))
                          #+sbcl (SB-INT:SIMPLE-READER-ERROR (err)
                                   (list :read-error (format nil "~A (condition of type ~A)" err (class-name (class-of err))))))))
      ;;(jformat t "code-to-eval = ~A~%" code-to-eval)
      (cond
        ((and (consp code-to-eval)
              (eq (car code-to-eval) :read-error-eof))
         (values execution-count nil ""
                 (format nil "Reader error: incomplete input (condition of type: ~A)~%" (cadr code-to-eval))))
        ((and (consp code-to-eval)
              (eq (car code-to-eval) :read-error))
         (values execution-count nil ""
                 (format nil "Reader error: ~A~%" (cadr code-to-eval))))
        ((equal code-to-eval '(progn))
         (values execution-count nil "" (format nil "Warning: no evaluable input~%")))
        ((and (consp code-to-eval)
              (eql (car code-to-eval) 'quicklisp-client:quickload)
              (stringp (cadr code-to-eval)))
         ;; quicklisp hook
         (let ((results (multiple-value-list (ql:quickload (cadr code-to-eval)))))
           (values execution-count results "" "")))
        (t
         ;; else "normal" evaluation
         ;;(jformat t "[Evaluator] Code to evaluate: ~W~%" code-to-eval)
         (let* ((stdout (make-instance 'output-stream
                                       :iopub iopub
                                       :stream-name "stdout"
                                       :key key
                                       :parent-msg parent-msg))
                (stderr (make-instance 'output-stream
                                       :iopub iopub
                                       :stream-name "stderr"
                                       :key key
                                       :parent-msg parent-msg)))
           (let ((results (let ((*standard-output* stdout)
                                (*error-output* stderr))
                            (handling-errors
                                        ;(if (and (consp code-to-eval)
                                        ;       (eql (car code-to-eval) 'quicklisp-client:quickload)
                                        ;       (stringp (cadr code-to-eval)))
                             ;; quicklisp hook
                                        ;  (multiple-value-list (ql:quickload (cadr code-to-eval)))
                             ;; normal evaluation
                             (let ((*evaluator* evaluator))
                               (let ((* (take-history-out -1))
                                     (** (take-history-out -2))
                                     (*** (take-history-out -3)))
                                 ;; put the evaluator in the environment
                                 (logg 2 "In evaluator.lisp:133  cl-jupyter:*kernel* -> ~a~%" cl-jupyter:*kernel*)
                                 (multiple-value-list (eval code-to-eval))))))))
             ;;(jformat t "[Evaluator] : results = ~W~%" results)
             (logg 2 "[Evaluator] : results = ~W~%" results)
             (let ((in-code (format nil "~A" code-to-eval)))
               (vector-push-extend (subseq in-code 7 (1- (length in-code)))
                                   (evaluator-history-in evaluator)))
             (vector-push-extend results (evaluator-history-out evaluator))
             (values execution-count results (contents stdout) (contents stderr)))))))))

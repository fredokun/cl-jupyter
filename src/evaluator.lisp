(in-package #:cl-jupyter)

#|

# Evaluator #

The evaluator is where the "interesting stuff" takes place :
 user expressions are evaluated here. 

The history of evaluations is also saved by the evaluator.

|#


(defclass evaluator ()
  ((kernel :initarg :kernel :reader evaluator-kernel)
   (history-in :initform (make-array 64 :fill-pointer 0 :adjustable t)
	       :reader evaluator-history-in)
   (history-out :initform (make-array 64 :fill-pointer 0 :adjustable t)
		:reader evaluator-history-out)))

(defun make-evaluator (kernel)
  (let ((evaluator (make-instance 'evaluator
				  :kernel kernel)))
    (setf (slot-value kernel 'evaluator) evaluator)
    evaluator))

;;; macro taken from: http://www.cliki.net/REPL
;;; modified to handle warnings correctly
(defmacro handling-errors (&body body)
  `(handler-case
       (handler-bind ((simple-warning
		       #'(lambda (wrn) 
			   (format *error-output* "~&~A: ~%" (class-name (class-of wrn)))
			   (apply (function format) *error-output*
				  (simple-condition-format-control   wrn)
				  (simple-condition-format-arguments wrn))
			   (format *error-output* "~&")
			   (muffle-warning)))
		      (warning 
		       #'(lambda (wrn) 
			   (format *error-output* "~&~A: ~%  ~S~%"
				   (class-name (class-of wrn)) wrn)
			   (muffle-warning))))
	 (progn ,@body))
     (simple-condition (err)
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&"))
     (condition (err)
       (format *error-output* "~&~A: ~%  ~S~%"
               (class-name (class-of err)) err))))

(defun evaluate-code (evaluator code)
  ;;(format t "[Evaluator] Code to evaluate: ~W~%" code)
  (vector-push-extend code (evaluator-history-in evaluator))
  (let ((execution-count (length (evaluator-history-in evaluator))))
 
    (let ((code-to-eval (handler-case
                            (read-from-string (format nil "(progn ~A)" code))
                          (END-OF-FILE (err) :read-error))))
      ;; (format t "code-to-eval = ~A~%" code-to-eval)
      (cond
        ((eq code-to-eval :read-error) (values execution-count nil "" "Incomplete input (END-OF-FILE condition)"))
        ((and (consp code-to-eval)
              (eql (car code-to-eval) 'quicklisp-client:quickload)
              (stringp (cadr code-to-eval)))
         ;; quicklisp hook
         (let ((results (multiple-value-list (ql:quickload (cadr code-to-eval)))))
           (values execution-count results "" "")))
        (t
         ;; else "normal" evaluation
         ;;(format t "[Evaluator] Code to evaluate: ~W~%" code-to-eval)
         (let* ((stdout-str (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
                (stderr-str (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
           (let ((results (with-output-to-string (stdout stdout-str)
                            (with-output-to-string (stderr stderr-str)
                              (let ((*standard-output* stdout)
                                    (*error-output* stderr))
                                (handling-errors
                                        ;(if (and (consp code-to-eval)
                                        ;	(eql (car code-to-eval) 'quicklisp-client:quickload)
                                        ;	(stringp (cadr code-to-eval)))
                                 ;; quicklisp hook
                                        ;  (multiple-value-list (ql:quickload (cadr code-to-eval)))
                                 ;; normal evaluation
                                 (multiple-value-list (eval code-to-eval))))))));)
             ;;(format t "[Evaluator] : results = ~W~%" results)
             (vector-push-extend results (evaluator-history-out evaluator))
             (values execution-count results stdout-str stderr-str))))))))


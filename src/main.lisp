(in-package #:fishbowl)

#|

# Fishbowl-repl entry point(s)

There are two complementary ways to start Fishbowl.

 - start an `ipython` frontend and let it spawn and manage the `Fishbowl-repl` kernel

 - start the `Fishbowl-repl` kernel and let it spawn and manage the `ipython` frontend.

There is not "best" way to start the REPL, there are pros and cons in both cases.
 The first solution makes the ``Fishbowl-repl` kernel unaware of the used frontend, and
 thus it can  handle any frontend (as long as the protocol is implemented). The Ipython frontend
can also launch multiple instances of the kernel, and restart it in case of trouble. Also, the Unix/Posix
 features provided by Python are probably more robust than in the Lisp case. In the second
 case, the advantage is that everything is controlled from the Lisp side, which makes
it compatible with e.g. the Quicklisp way of doing things.

Note that the dependencies are exactly the same in both cases.

|#

(defun check-implementation ()
  #+sbcl  :sbcl
  #+clozure :ccl
  #-(or sbcl clozure)
  (error "Sorry, at this time only SBCL and Clozuze CL are supported."))

(defun check-native-threads ()
  #+sbcl (if (not (member :sb-thread *features*))
             (exit-with-error "Native thread not supported by this SBCL build.")
             t)
  #+clozure (if (not (member :openmcl-native-threads *features*))
                (exit-with-error "Native thread not supported by this CCL build.")
                t)
  #-(or sbcl clozure) (error "Implementation unsupported."))
  
(defun get-argv ()
  ;; Borrowed from apply-argv, command-line-arguments.  Temporary solution (?)
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+clozure CCL:*UNPROCESSED-COMMAND-LINE-ARGUMENTS*  ;(ccl::command-line-arguments)
  #+gcl si:*command-args*
  #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
  #+cmu extensions:*command-line-strings*
  #+allegro (sys:command-line-arguments)
  #+lispworks sys:*line-arguments-list*
  #+clisp ext:*args*
  #-(or sbcl clozure gcl ecl cmu allegro lispworks clisp)
  (error "get-argv not supported for your implementation"))

(defun banner ()
  (write-line "")
  (format t "~A: an enhanced interactive Common Lisp Shell~%" +KERNEL-IMPLEMENTATION-NAME+)
  (format t "(Version ~A - Ipython protocol v.~A)~%"
          +KERNEL-IMPLEMENTATION-VERSION+
          +KERNEL-PROTOCOL-VERSION+)
  (format t "--> (C) 2014-2015 Frederic Peschanski (cf. LICENSE)~%")
  (write-line ""))


(defun run-program-to-string (program-path args)
  (let ((outstr (make-array '(0) :element-type 'base-char
                            :fill-pointer 0 :adjustable t)))
    (handler-case
        (with-output-to-string (output outstr)
          #+sbcl (sb-ext:run-program program-path args :search t :output output)
          #+clozure (let ((proc (run-program program-path args :silently-ignore-catastrophic-failures nil :output output)))
                      ;; XXX: the (:silently-ignore-catastrophic-failures nil) option does not
                      ;; seem to work ... so the following is a workaround.
                      (let ((results (multiple-value-list 
                                      (external-process-status proc))))
                        (when (and (eql (car results) :exited)
                                   (not (eql (cadr results) 0)))
                          (return-from run-program-to-string nil))))
          #-(or sbcl clozure) nil
          )
      (error (se)
        (declare (ignore se))
        (format t "here")
        (return-from run-program-to-string nil)))
    #-(or sbcl clozure)
    (error "Cannot run program: Lisp implementation unsupported (please report).")
    (if (eql (aref outstr (- (length outstr) 1))
             #\Newline)
        (progn
          (vector-pop outstr)
          outstr)
        outstr)))

;; (run-program-to-string "ipython" '("--version"))

(defun check-ipython-version (program-path &key (required-version 2))
  (let ((version-str (run-program-to-string program-path '("--version"))))
    (when (null version-str)
      (error "Cannot run: ~A" program-path))
    (when (not (= (digit-char-p (aref version-str 0)) required-version))
      (error "IPython major version ~A required (current '~A' is v.~A)" required-version program-path (aref version-str 0)))
    t))

;; (check-ipython-version "ipython")

(defun fetch-ipython-default-profile-dir (program-path profile-name)
  (let ((profile-path-str (run-program-to-string program-path `("locate" "profile" ,profile-name))))
    (when (null profile-path-str)
        ;; create the profile
      (setf profile-path-str (run-program-to-string program-path `("profile" "create" ,profile-name))))
    (truename profile-path-str)))

;; (fetch-ipython-default-profile-dir "ipython" "fishbowl")

(defun fetch-ipython-custom-profile-dir (program-path profile-dir)
  (let ((profile-path-str
         (run-program-to-string program-path `("locate" "profile" ,(format nil "--profile-dir=~W" (namestring profile-dir))))))
    (if (null profile-path-str)
        nil
        (truename profile-path-str))))

;;(fetch-ipython-custom-profile-dir "ipython" (truename "~/Cours/LI101P/Supports/Exercices/config/profile"))


;; (truename "~/Cours/LI101P/Supports/Exercices/config/profile")


(defun install-ipython-fishbowl-custom-files (profile-dir)
  (let ((custom-dir (truename (concatenate 'string (namestring profile-dir) "static/custom/"))))                                       
    (error "Not yet implemented")))

(defun kernel-launch (notebook-file &key
                                      (profile-name "fishbowl")
                                      (profile-dir nil)
                                      (profile-overwrite nil)
                                      (shell-port 0)
                                      (iopub-port 0)
                                      (hb-port 0)
                                      (ipython-program "ipython")
                                      (ipython-extra-opts ""))
  (banner)
  (check-implementation)
  (check-native-threads)
  (check-ipython-version ipython-program)
  (let ((ipython-profile-dir (if profile-dir
                                 (fetch-ipython-custom-profile-dir ipython-program profile-dir)
                                 (fetch-ipython-default-profile-dir ipython-program profile-name))))
    (when (null ipython-profile-dir)
      (error "Cannot initialize IPython profile directory."))        
    (error "Not yet implemented.")))


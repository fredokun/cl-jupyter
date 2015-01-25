(in-package #:fishbowl)

#|

# Fishbowl-repl entry point(s)

There are two complementary ways to start Fishbowl.

 - start an `ipython` frontend and let it spawn and manage the `Fishbowl-repl` kernel

 - start the `Fishbowl-repl` kernel and let it spawn and manage the `ipython` frontend.

There is not "best" way to start the REPL, there are pros and cons in both cases.
 The first solution makes the ``Fishbowl-repl` kernel unaware of the used frontend, and
 thus it can  handle any frontend (as long as the protocol is implemented). Also, the Unix/Posix
 features provided by Python are probably more robust than in the Lisp case. In the second
 case, the advantage is that everything is controlled from the Lisp side, which makes
it compatible with e.g. the Quicklisp way of doing things.

Note that the dependencies are exactly the same in both cases.

|#


(defun check-implementation ()
  #+sbcl  :sbcl
  #+clozure :ccl
  #-(or sbcl clozure)
  (error "Sorry, at this time only SBCL and CCL (clozure) are supported."))


(defun check-native-threads (impl)
  #+sbcl (if (not (member :sb-thread *features*))
             (error "Native thread not supported by this SBCL build.")
             t)
  #+clozure
  


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


(defun parse-command-line ()
  (let ((cmd-args (get-argv)))
    ;; TODO
    (error "Not implemented")))

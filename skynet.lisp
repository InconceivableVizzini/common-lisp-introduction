;;;; skynet.lisp

(in-package #:skynet)

(defun asdf-version ()
  "Returns the version of :asdf in this environment"
  (when (find-package :asdf)
    (let ((ver (symbol-value
		(or (find-symbol (string :*asdf-version*) :asdf)
		    (find-symbol (string :*asdf-version*) :asdf)))))
      (etypecase ver
	(string ver)
	(cons (with-output-to-string (s)
		(loop for (n . m) on ver
		   do (princ n s)
		     (when m (princ "." s)))))
	(null "1.0")))))

(defun argv ()
  "Returns the arguments passed to the program"
  (or
   #+CLISP *args*
   #+SBCL sb-ext:*posix-argv*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(defun main ()
  "The entry point to our lisp application"

  ; Print the ASDF version we're using and echo any
  ; program arguments.
  (format t "ASDF version: ~D ~%" (asdf-version))
  (format t "~D ~a" (length (rest (argv)))
	  (cond ((= 1 (length (rest (argv)))) "argument")
		(t "arguments")))
  (mapcar #'(lambda (x) (format t " ~a " x)) (rest (argv)))
  (format t "~%"))

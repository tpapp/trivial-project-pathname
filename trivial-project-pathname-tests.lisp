;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(cl:defpackage #:trivial-project-pathname-tests
  (:use #:cl #:alexandria #:anaphora #:clunit #:let-plus)
  (:export
   #:run-suite))

(in-package #:trivial-project-pathname-tests)

(defsuite tests ())

(defun run (&optional interactive?)
  "Run the unit tests."
  (run-suite 'tests :use-debugger interactive?))

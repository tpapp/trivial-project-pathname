;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(asdf:defsystem #:trivial-project-pathname
  :serial t
  :description "Simple Common Lisp library for project pathname management."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:anaphora
               #:cl-fad
               #:let-plus)
  :components ((:file "trivial-project-pathname")))

(asdf:defsystem #:trivial-project-pathname-tests
  :description "Unit tests for TRIVIAL-PROJECT-PATHNAME."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :depends-on (#:trivial-project-pathname
               #:clunit)
  :serial t
  :components ((:file "trivial-project-pathname-tests")))

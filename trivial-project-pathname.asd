;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(asdf:defsystem #:trivial-project-pathname
  :serial t
  :description "Simple Common Lisp library for project pathname management."
  :author "Tamas K. Papp <tkpapp@gmail.com>"
  :license "MIT"
  :depends-on (#:anaphora
               #:cl-fad
               #:let-plus)
  :components ((:file "trivial-project-pathname")))

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(defpackage #:trivial-project-pathname
  (:use #:cl #:alexandria #:anaphora)
  (:export
   #:define-project-pathname
   #:project-base-directory))

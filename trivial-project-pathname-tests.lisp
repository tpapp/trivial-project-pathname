;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(cl:defpackage #:trivial-project-pathname-tests
  (:use #:cl
        #:alexandria
        #:anaphora
        #:clunit
        #:let-plus)
  (:export
   #:run-suite))

(in-package #:trivial-project-pathname-tests)

(defsuite tests ())

(defun run (&optional interactive?)
  "Run the unit tests."
  (run-suite 'tests :use-debugger interactive?))



(project-pathname:define project-pathname1 (:asdf "trivial-project-pathname"))

(defun system-relative-truename (name)
  (truename (asdf:system-relative-pathname "trivial-project-pathname" name)))

(deftest test1 (tests)
  (assert-equal (system-relative-truename "test-files/a")
      (truename (project-pathname1 "test-files/a"))))

(project-pathname:define (project-pathname2 :load-time-value? t)
    (:directory-of (system-relative-truename "trivial-project-pathname.lisp"))
  (test "test-files/"))

(project-pathname:define (project-pathname3 :load-time-value? t)
    (:directory (system-relative-truename "")))

(deftest test2 (tests)
  (assert-equal (truename (project-pathname2 "test-files/b"))
      (truename (project-pathname3 "test-files/b")))
  (assert-equal (truename (project-pathname2 "b" 'test))
      (truename (project-pathname3 "test-files/b"))))

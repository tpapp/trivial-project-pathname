;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(cl:defpackage #:trivial-project-pathname
  (:use #:cl #:alexandria #:anaphora)
  (:export
   #:define-project-pathname
   #:project-base-directory))

(cl:in-package #:trivial-project-pathname)

(defgeneric project-base-directory% (base-directory)
  (:documentation "The base directory from a specification.  Used internally,
see PROJECT-BASE-DIRECTORY.")
  (:method ((system symbol))
    (asdf:system-source-directory system))
  (:method ((system-or-directory string))
    (aif (cl-fad:directory-exists-p system-or-directory)
         it
         (asdf:find-system system-or-directory nil))))

(defun project-base-directory (base-directory relative)
  "The base directory, combined with RELATIVE."
  (let ((base-directory (project-base-directory% base-directory)))
    (if relative
        (merge-pathnames (cl-fad:pathname-as-directory relative)
                         base-directory)
        base-directory)))

(defmacro define-project-pathname (function (base-directory &optional relative)
                                   &body clauses)
  "Define FUNCTION that resolves project file names relative to a base
directory, which is computed from BASE-DIRECTORY (a symbol or string
designating an ASDF system, or a pathname designator for a directory),
combined with RELATIVE (a relative directory, optional).

FIXME finish documentation
"
  (with-unique-names (base-directory%)
    `(let ((,base-directory% (project-base-directory ,base-directory ,relative)))
       (assert (cl-fad:directory-pathname-p ,base-directory%) ()
               "The resulting base directory ~A is not a directory."
               (namestring ,base-directory%))
       ,(if clauses
            `(defun ,function (kind filename)
               (merge-pathnames
                (let ((relative-directory (cl-fad:pathname-as-directory
                                           (ecase kind
                                             ,@clauses))))
                  (aif relative-directory
                       (merge-pathnames filename it)
                       filename))
                ,base-directory%))
            `(defun ,function (filename)
               (merge-pathnames filename ,base-directory%))))))

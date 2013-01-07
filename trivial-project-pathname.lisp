;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(cl:defpackage #:trivial-project-pathname
  (:use #:cl
        #:anaphora
        #:let-plus)
  (:export
   #:project-base-directory
   #:project-relative-directory
   #:define-project-pathname))

(cl:in-package #:trivial-project-pathname)

(defun project-base-directory (type designator)
  "Return a directory (path).  Valid arguments are:

  :asdf system-designator
    Source directory of an ASDF system designated by SYSTEM-DESIGNATOR.  See ASDF:SYSTEM-SOURCE-DIRECTORY.

  :directory PATHSPEC
    Path specification.  Does not have to exist.  Checked for being a directory.

  :as-directory PATHSPEC
    Converted using CL-FAD:PATHNAME-AS-DIRECTORY.

  :directory-of PATHSPEC
    Directory of PATHSPEC, first converted using CL-FAD:PATHNAME-AS-FILE."
  (ecase type
    (:asdf (asdf:system-source-directory designator))
    (:directory (aprog1 (pathname designator)
                  (assert (cl-fad:directory-pathname-p it) ()
                          "~A is not a directory."
                          (namestring it))))
    (:as-directory (cl-fad:pathname-as-directory designator))
    (:directory-of (make-pathname :name nil :type nil
                                  :directory  (cl-fad:pathname-as-file designator)))))

(defun project-relative-directory (type designator relative-path)
  "When RELATIVE-PATH is not NIL, combine it with the base directory obtained from TYPE and DESIGNATOR (see PROJECT-BASE-DIRECTORY).  Otherwise just return the latter."
  (let ((base-directory (project-base-directory type designator)))
    (aif relative-path
         (merge-pathnames (cl-fad:pathname-as-directory it)
                          base-directory)
         base-directory)))

(defmacro define-project-pathname (function-specification directory-specification
                                   &body subdirectories)
  "

Arguments:

  function-specification := function-name | (function-name &key load-time-value?)

  directory-specification is passed on to project-relative-directory

  subdirectories := (types pathspec)*

  types := SYMBOL | (SYMBOL+)

Description:

Defines (FUNCTION-NAME PATHSPEC &optional TYPE) that resolves path specifications relative to a base directory that is obtained by passing DIRECTORY-SPECIFICATION to PROJECT-RELATIVE-DIRECTORY (see its documentation for acceptable forms), then, if TYPE is given, merges the corresponding pathspec, then the pathspec in the argument.  SUBDIRECTORIES are passed on to CASE.

Example (using ASDF):

  (define-project-pathname project-pathname1 (:asdf :trivial-project-pathname))

  (project-pathname1 \"trivial-project-pathname.lisp\")
      ; => the path to the source file of this function

Example (using read-time evaluation, subdirectories)

  (define-project-pathname project-pathname2
    (:directory-of #.(or *compile-file-truename* *load-truename*)
                   '(:relative :up))
"
  (let+ (((function &key load-time-value?) (ensure-list function-specification))
         (directory-form `(project-relative-directory ,@directory-specification)))
    `(defun ,function (filename &optional type)
       (let ((directory ,(if load-time-value?
                             `(load-time-value ,directory-form t)
                             directory-form))
             (directory (let ((subdirectory
                                (ecase type
                                  ,@subdirectories
                                  ((nil) nil))))
                          (if subdirectory
                              (progn
                                (assert (cl-fad:directory-pathname-p subdirectory))
                                (merge-pathnames subdirectory directory))
                              directory))))
         (merge-pathnames filename directory)))))

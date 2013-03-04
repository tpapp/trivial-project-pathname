;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(cl:defpackage #:trivial-project-pathname
  (:nicknames #:project-pathname)
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export
   #:base-directory
   #:relative-directory
   #:define
   #:define-load-directory))

(cl:in-package #:trivial-project-pathname)

(defun base-directory (type designator)
  "Return a directory (path).  Valid arguments are:

  :asdf SYSTEM-DESIGNATOR
    Source directory of an ASDF system designated by SYSTEM-DESIGNATOR.  See ASDF:SYSTEM-SOURCE-DIRECTORY.  Only available when ASDF is.  Check that the system is found.

  :directory PATHSPEC
    Path specification.  Does not have to exist.  Checked for being a directory.

  :as-directory PATHSPEC
    Converted using CL-FAD:PATHNAME-AS-DIRECTORY.

  :directory-of PATHSPEC
    Directory of PATHSPEC interpreted as a file."
  (ecase type
    #+asdf2 (:asdf (aprog1 (asdf:system-source-directory designator)
                     (assert it () "System ~A not found." designator)))
    (:directory  (aprog1 (pathname designator)
                   (assert (cl-fad:directory-pathname-p it) ()
                           "~A is not a directory." (namestring it))))
    (:as-directory (cl-fad:pathname-as-directory designator))
    (:directory-of (make-pathname :name nil
                                  :type nil
                                  :directory (pathname-directory
                                              (cl-fad:pathname-as-file designator))))))

(defun make-relative-directory (relative-directory)
  "Create a pathname representing a relative directory from a list of directories (may contain :UP, etc)."
  (make-pathname :directory (cons :relative relative-directory)))

(defun relative-directory (type designator &rest relative-path)
  "When RELATIVE-PATH is not NIL, combine it with the base directory obtained from TYPE and DESIGNATOR (see BASE-DIRECTORY).  Otherwise just return the latter."
  (let ((base-directory (base-directory type designator)))
    (aif relative-path
         (merge-pathnames (make-relative-directory it) base-directory)
         base-directory)))

(defmacro define (function-specification directory-specification &body subdirectories)
  "Define a function that resolves pathnames in a project.

Arguments:

  FUNCTION-SPECIFICATION := FUNCTION-NAME | (FUNCTION-NAME &key LOAD-TIME-VALUE?)

  DIRECTORY-SPECIFICATION is passed on to RELATIVE-DIRECTORY

  SUBDIRECTORIES := (types . pathspec)*

  types := SYMBOL | (SYMBOL+)

  pathspec := DIRECTORY+

Description:

Defines (FUNCTION-NAME PATHSPEC &optional TYPE) that resolves path specifications relative to a base directory that is obtained by passing DIRECTORY-SPECIFICATION to RELATIVE-DIRECTORY (see its documentation for acceptable forms), then, if TYPE is given, merges the corresponding pathspec, then the pathspec in the argument.  SUBDIRECTORIES are passed on to CASE, forming a :RELATIVE pathname from a list of directories.

Example (using ASDF):

  (define-project-pathname project-pathname1 (:asdf :trivial-project-pathname))

  (project-pathname1 \"trivial-project-pathname.lisp\")
      ; => the path to the source file of this function

Example (using ASDF, subdirectories):

  (define-project-pathname project-pathname2
      (:asdf :my-project-that-has-code-in-a-subdirectory :up))
    (:plots \"plots/\")
    (:data \"data\"))
"
  (let+ (((function &key load-time-value?) (ensure-list function-specification))
         (directory-form `(relative-directory ,@directory-specification))
         ((&values subdirectories declarations docstring)
          (if (and (not (cdr subdirectories)) (stringp (car subdirectories)))
              (values nil nil (car subdirectories))
              (parse-body subdirectories :documentation t))))
    `(defun ,function (filename &optional type)
       ,@(when docstring `(,docstring))
       (let* ((directory ,(if load-time-value?
                              `(load-time-value ,directory-form t)
                              directory-form))
              (directory (let ((subdirectory
                                 ,@declarations
                                 (ecase type
                                   ,@(loop for (key . directory) in subdirectories
                                           collect (list key
                                                         (make-relative-directory
                                                          directory)))
                                   ((nil) nil))))
                           (if subdirectory
                               (progn
                                 (assert (cl-fad:directory-pathname-p subdirectory))
                                 (merge-pathnames subdirectory directory))
                               directory))))
         (merge-pathnames filename directory)))))

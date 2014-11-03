(cl:in-package :cl-case-control-asd)

(defpackage :cl-case-control
  (:nicknames :case-control :cl-casectl :casectl)
  (:use :cl #+clisp :ext)
  #+clisp (:import-from :ext #:char-invertcase #:string-invertcase)
  ;; predicates and utilities for unify codes in both "standard"
  ;; case-insensitive environment and "extended" case-sensitive environment.
  (:export #:case-sensitive-p #:case-sensitive-package-p)
  (:export #:adj-case #:case-selective-intern #:case-selective-defpackage)
  ;; general purpose type, predicate, and utilities
  (:export #:string-designator)
  (:export #:string-designator-p #:lowercase-char-p #:uppercase-char-p)
  (:export #:char-invertcase #:string-invertcase) )

(in-package :cl-case-control)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when
    #+clisp (find-package :cs-cl)
    #+(and allegro acl-case-sensitive) t
    #+(and allegro (not acl-case-sensitive)) nil
    #-(or clisp allegro) nil
    (pushnew :case-sensitiveness *features*) ))


(defun string-designator-p (obj)
  (typep obj 'string-designator) )

(defun case-sensitive-p ()
  #+case-sensitiveness t
  #-case-sensitiveness nil )

(defun case-sensitive-package-p (&optional (pkg *package*))
  (declare (ignorable pkg))
  #+clisp (package-case-sensitive-p pkg)
  #+(and allegro (not acl-case-sensitive)) nil
  #+(and allegro acl-case-sensitive)
  (eq (readtable-case *readtable*) :preserve)
  #-(or clisp allegro) nil )

(defun case-inverted-package-p (&optional (pkg *package*))
  (declare (ignorable pkg))
  #+clisp (package-case-inverted-p pkg)
  #+(and allegro (not acl-case-sensitive)) nil
  #+(and allegro acl-case-sensitive)
  (case (readtable-case *readtable*) ((:downcase :invert) t))
  #-(or clisp allegro) nil )

(defun lowercase-char-p (c)
  "Predicate for lowercase character.
Note: LOWERCASE-CHAR-P function DOES NOT assume given character is an ASCII
character."
  (and (alpha-char-p c) (char= (char-downcase c) c)) )

(defun uppercase-char-p (c)
  "Predicate for uppercase character.
Note: UPPERCASE-CHAR-P function DOES NOT assume given character is an ASCII
character."
  (and (alpha-char-p c) (char= (char-upcase c) c)) )

#-clisp
(defun char-invertcase (c)
  "Invert case of given character.
Note: CHAR-INVERTCASE function DOES NOT assume given character is an ASCII
character."
  (unless (characterp c)
    (error 'type-error :datum c :expected-type 'character) )
  (cond ((lowercase-char-p c) (char-upcase c))
        ((uppercase-char-p c) (char-downcase c))
        (t c) ))

#-clisp
(defun string-invertcase (obj)
  (let ((str (typecase obj
               (string obj)
               ((or character symbol) (string obj))
               (otherwise
                 ;; For expected-type parameter, in order to generate
                 ;; an intelligible error message, use not
                 ;; asdf:string-designator but (or string character symbol).
                 (error 'type-error
                        :datum obj
                        :expected-type '(or string character symbol) )))))
    (with-output-to-string (sos)
      (with-input-from-string (sis str)
        (loop with eof = (gensym)
              for c = (read-char sis nil eof)
              until (eq c eof)
              do (write-char (char-invertcase c) sos) )))))

(defun adj-case (str &optional (pkg *package*))
  (funcall (if (case-sensitive-package-p pkg)
             (if (case-inverted-package-p pkg)
               #'string
               #'string-invertcase )
             #'string-upcase )
           str ))

(defun case-selective-intern (str &optional (pkg *package*))
  "CASE-SELECTIVE-INTERN is a alternative INTERN for CL implementations
which have \"modern\" case-sensitivess feature.
 CASE-SELECTIVE-INTERN interns given string into given package (or current
package) with or without case conversion.

 When given package is a case-insensitive package, CASE-SELECTIVE-INTERN
interns given string with STRING-UPCASE.
 When given package is a case-sensitive package, CASE-SELECTIVE-INTERN
interns given string. Case of characters on the given string is left as is.

 Note: you should use CL:INTERN if you want to intern into case-insensitive
package without case conversion."
  (unless (stringp str)
    (error 'type-error :datum str :expected-type 'string) )
  (unless (or (packagep pkg)
              (and (string-designator-p pkg) (packagep (find-package pkg))) )
    (error 'type-error :datum pkg :expected-type 'package) )
  #-case-sensitiveness (intern (string-upcase str) pkg)
  #+case-sensitiveness
  (cond ((case-sensitive-package-p pkg)
         #+clisp
         (cs-cl:intern
           (if (case-inverted-package-p pkg) str (string-invertcase str))
           pkg )
         #+allegro
         (intern str pkg) )
        (t (cl:intern (string-upcase str) pkg)) ))

(defmacro case-selective-defpackage (pname case-p &rest params)
  (declare (ignorable case-p))
  `(defpackage ,pname
     #+case-sensitiveness (:modern ,case-p)
     #+(and case-sensitiveness clisp) (:use :cs-cl)
     #+(and case-sensitiveness (not clisp)) (:use :cl)
     #-case-sensitiveness (:use :cl)
     ,@params ))


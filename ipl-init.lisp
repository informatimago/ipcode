;;;; -*- coding: utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               ipl-init.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             None
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;
;;;;    This file loads the IPL packages.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2014-10-10 <PJB> Generalized for ccl (and perhaps other implementations)
;;;;    2005-11-04 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2003 - 2014
;;;;    mailto:pjb@informatimago.com
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************
(in-package "COMMON-LISP-USER")

;;; We load quicklisp:
(let ((setups (list (merge-pathnames
                     (make-pathname :directory '(:relative "QUICKLISP")
                                    :name "SETUP"
                                    :type "LISP"
                                    :version :newest
                                    :case :common
                                    :defaults (user-homedir-pathname))
                     (user-homedir-pathname)
                     nil)

                    ;; Some implementations (eg. ccl) don't
                    ;; translate case common to lower case
                    ;; customary in POSIX file systems.
                    
                    (merge-pathnames
                     (make-pathname :directory '(:relative "quicklisp")
                                    :name "setup"
                                    :type "lisp"
                                    :version :newest
                                    :case :common
                                    :defaults (user-homedir-pathname))
                     (user-homedir-pathname)
                     nil))))
  (unless (dolist (quicklisp setups)
            (when (probe-file quicklisp)
              (load quicklisp)
              (return t)))
    (error "Please install quicklisp.  I expect it in ~S" (namestring (first setups)))))


;;; Load the CLX library, to access the X11 server:

#+clisp (require "clx")
#-clisp (ql:quickload "clx")
(ql:quickload "babel")


;;; Let's define an IPL package, which will export the Common Lisp
;;; symbol we will use.

(defpackage "IPL"
  (:use "COMMON-LISP")
  ;; For now, we re-export all the COMMON-LISP symbols:
  (:export . #.(let ((symlist '()))
                 (do-external-symbols (sym "COMMON-LISP" symlist)
                   (push (symbol-name sym) symlist))))
  (:documentation "The IPL package exports the Common Lisp symbols
we will use."))
(in-package "IPL")


;;; We set up a logical host:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (logical-pathname-translations "IPL") nil)
  (setf (logical-pathname-translations "IPL")
        (let* ((path (or *compile-file-pathname* *load-pathname*))
               ;; (path #P "~/works/ipl/ipcode/ipl-init.lisp")
               (dir  (truename (make-pathname :name nil :type nil :version nil :defaults path))))
          ;; DIR is the current directory, where this file is loaded from.
          ;;
          ;; We build a map of all the lisp files in this directory,
          ;; translating logical pathnames such as: #P"IPL:FILE.LISP"
          ;; to the corresponding physical pathname.
          ;; This is required, because some implementation don't map
          ;; uppercase (from conforming logical pathname), to
          ;; lowercase (as it is customary for files on POSIX file
          ;; systems).
          ;;
          ;; Note: we don't take into account compiled files in those
          ;; translations.  We also map the logical pathnames without
          ;; types to the same lisp source files.
          (append
           (mapcan (lambda (file)
                     (let ((path (enough-namestring file dir)))
                       (list (list (format nil "~:@(IPL:~{~A;~}~A.~A~)"
                                           (rest (pathname-directory path))
                                           (pathname-name path)
                                           (pathname-type path))
                                   file)
                             (list (format nil "~:@(IPL:~{~A;~}~A~)"
                                           (rest (pathname-directory path))
                                           (pathname-name path))
                                   file))))
                   (directory (make-pathname :name :wild :type "lisp" :version nil :defaults path)))
           (list (list "IPL:**;*.*"
                       (merge-pathnames
                        (make-pathname :directory '(:relative :wild-inferiors)
                                       :name :wild :type :wild :version nil
                                       :defaults path)
                        path nil))
                 (list "IPL:**;*"
                       (merge-pathnames
                        (make-pathname :directory '(:relative :wild-inferiors)
                                       :name :wild :type nil :version nil
                                       :defaults path)
                        path nil)))))))

;;; Now we can load the X library and the turtles:

(load "IPL:IPL-CLX")
(load "IPL:IPL-TURTLES")


;;; Finally, we make an IPL-USER package to work with those graphics
;;; libraries:

(defpackage "IPL-USER"
  (:use "IPL" "IPL-CLX" "IPL-TURTLES")
  (:documentation "This is a sand-box package for the user
to play with IPL and graphics stuff."))
(in-package "IPL-USER")

(import '(#+clisp ext:quit
          #+ccl   ccl:quit
          #+sbcl  sb-ext:quit))

;;; THE END ;;;

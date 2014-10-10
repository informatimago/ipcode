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
;;;;    2005-11-04 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2003 - 2004
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

(IN-PACKAGE "COMMON-LISP-USER")
(require "clx")

(DEFPACKAGE "IPL"
  (:USE "COMMON-LISP"))
(IN-PACKAGE "IPL")

;; For now, we re-export all the COMMON-LISP symbols:
(LET ((SYMLIST '()))
  (DO-EXTERNAL-SYMBOLS (SYM "COMMON-LISP") (PUSH SYM SYMLIST))
  (EXPORT SYMLIST #.(FIND-PACKAGE "IPL")))

(setf (logical-pathname-translations "IPL") nil)
(setf (logical-pathname-translations "IPL")
      (list (list "IPL:**;*.*"
                  (merge-pathnames
                   (make-pathname :directory '(:relative :wild-inferiors)
                                  :name :wild :type :wild :version nil
                                  :defaults *load-pathname*)
                   *load-pathname* nil))
            (list "IPL:**;*"
                  (merge-pathnames
                   (make-pathname :directory '(:relative :wild-inferiors)
                                  :name :wild :type nil :version nil
                                  :defaults *load-pathname*)
                   *load-pathname* nil))))

(LOAD "IPL:IPL-CLX")
(LOAD "IPL:IPL-TURTLES")

(DEFPACKAGE "IPL-USER"
  (:USE "IPL" "IPL-CLX" "IPL-TURTLES"))

(IN-PACKAGE "IPL-USER")

(import '(ext:quit))

;;; The End ;;;



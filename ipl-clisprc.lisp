;;;; -*- coding: utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               ipl-clisprc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             None
;;;;USER-INTERFACE:     None
;;;;DESCRIPTION
;;;;
;;;;    This file contains initializations common to clisp, cmucl, sbcl, and
;;;;    possibly to other Common-Lisp too.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon
;;;;MODIFICATIONS
;;;;    2004-11-23 <PJB> Added LIST-ALL-SYMBOLS and LIST-EXTERNAL-SYMBOLS.
;;;;    2003-05-02 <PJB> Created.
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

(in-package "COMMON-LISP-USER")
(defpackage "IPL"
  (:use "COMMON-LISP")) ; Export à la fin.
(in-package "IPL")

(let ((symlist '()))
  (do-external-symbols (sym "COMMON-LISP") (push sym symlist))
  (export symlist #.(find-package "IPL")))

;;; ----------------------------------------------------------------------
;;; -- logical hosts -- the Common-Lisp way to PATH --
;;; --------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *logical-hosts* '()))
(export '*logical-hosts*)

(defun def-lp-trans (host path &optional (subpath ""))
  (pushnew host *logical-hosts* :test (function string-equal))
  ;; If the HOST is already defined we don't change it.
  (unless (handler-case (logical-pathname-translations host) (error () nil))
    (let ((directory (append (pathname-directory path)
                             (cdr (pathname-directory subpath))
                             '( :wild-inferiors ))))
      (setf (logical-pathname-translations host)
            (list
             (list "**;*"     (make-pathname :directory directory
                                             :name :wild))
             (list "**;*.*"   (make-pathname :directory directory
                                             :name :wild :type :wild))
             (list "**;*.*.*" (make-pathname :directory directory
                                             :name :wild :type :wild
                                             :version :wild)) )))))

(defmacro mp (pathname &optional
                       (directory nil directory-p)
                       (name      nil name-p)
                       (type      nil type-p)
                       (version   nil version-p))
  `(merge-pathnames
    (make-pathname,@(when directory-p `(:directory '(:relative ,@directory)))
                  ,@(when name-p      `(:name      ,name))
                  ,@(when type-p      `(:type      ,type))
                  ,@(when version-p   `(:version   ,version))
                  :defaults ,pathname)
    ,pathname))


(defparameter +share-lisp+ "/usr/local/share/lisp/")
(def-lp-trans "SHARE-LISP" +share-lisp+)
(def-lp-trans "CMU-AI"     +share-lisp+ "ai/")
(def-lp-trans "CL-PDF"     +share-lisp+ "cl-pdf/")
(def-lp-trans "UFFI"       +share-lisp+ "uffi/")
(def-lp-trans "PACKAGES"   +share-lisp+ "packages/")
(def-lp-trans "CLOCC"      +share-lisp+ "packages/net/sourceforge/clocc/clocc/")
(def-lp-trans "CCLAN"      +share-lisp+ "packages/net/sourceforge/cclan/")
(def-lp-trans "DEFSYSTEM"  +share-lisp+
  ;; We must go thru a translation for defsystem-3.x isn't a valid logical name!
  "packages/net/sourceforge/clocc/clocc/src/defsystem-3.x/")

(defparameter +pjb-comm+ (mp (user-homedir-pathname) ("src" "public" "common")))
(defparameter +pjb-lisp+ (mp (user-homedir-pathname) ("src" "lisp")))

(def-lp-trans "HOME"     (user-homedir-pathname)   "")
(def-lp-trans "LOADERS"  +pjb-comm+        "cl-loaders/")
(def-lp-trans "NORVIG"   +pjb-lisp+        "norvig/")


;;;----------------------------------------------------------------------
#-sbcl (load "PACKAGES:NET;SOURCEFORGE;CCLAN;ASDF;ASDF.LISP")
#+sbcl (require 'asdf)
#+sbcl (require 'asdf-install)
;;;(LOAD "PACKAGES:NET;SOURCEFORGE;CCLAN;ASDF;ASDF-INSTALL.LISP")

(defparameter *original-asdf-registry* asdf:*central-registry*)

(defun asdf-rescan-packages ()
  (format *trace-output* "~&;; Scanning ASDF packages...~%")
  (prog1
      (sort 
       (delete-duplicates 
        (mapcar
         (lambda (p) (make-pathname :name nil :type nil :version nil :defaults p))
         (directory "PACKAGES:**;*.ASD")) 
        :test (function equal))
       (lambda (a b) (if (= (length a) (length b))
                  (string< a b)
                  (< (length a) (length b))))
       :key (function namestring))
    (format *trace-output* "~&;; Done.~%")))

(defun update-asdf-registry ()
  (setf asdf:*central-registry*
        (nconc (asdf-rescan-packages)
               ;;(list CCLAN-GET::*CCLAN-ASDF-REGISTRY*)
               *original-asdf-registry*)))

(export 'update-asdf-registry)
(update-asdf-registry)

(in-package "ASDF")
(handler-bind ((warning (function muffle-warning)))
  ;; TODO: we should keep the error message in a string
  ;;       and check it's only the warnings.
  (flet ((output-files (c)
           (flet ((implementation-id ()
                    (flet ((first-word (text)
                             (let ((pos (position (character " ") text)))
                               (remove (character ".")
                                       (if pos (subseq text 0 pos) text)))))
                      (format nil
                        "~A-~A-~A"
                        (first-word (lisp-implementation-type))
                        (first-word (lisp-implementation-version))
                        (first-word (machine-type))))))
             (let* ((object
                      (compile-file-pathname (asdf:component-pathname c)))
                    (path
                     (merge-pathnames
                      (make-pathname :directory
                                     (list :relative
                                           (format nil
                                             "OBJ-~:@(~A~)"
                                             (implementation-id)))
                                     :name
                                     (pathname-name object)
                                     :type
                                     (pathname-type object))
                      object)))
               (ensure-directories-exist path)
               (list path)))))
    (defmethod output-files ((operation compile-op) (c cl-source-file))
      (output-files c))
    (defmethod output-files
        ((operation load-op) (c cl-source-file))
      (output-files c))))

(in-package "IPL")

;;;----------------------------------------------------------------------
;;; (WHEN *LOAD-VERBOSE*
;;;   (FORMAT T "~& (LOAD \"LOADER:CCLAN.LISP\") ~%")
;;;   (FORMAT T "~& (LOAD \"DEFSYSTEM:DEFSYSTEM.LISP\") ~%"))



(unless (block :done
          (dolist (file (list (mp +share-lisp+ ("common-lisp") "package")
                              (mp +share-lisp+ ("common-lisp") "package" "lisp")
                              (mp +pjb-comm+ ("common-lisp") "package")
                              (mp +pjb-comm+ ("common-lisp") "package" "lisp")))
            (handler-case (progn (load file) (return-from :done t)) (error ())))
          nil)
  (error "Cannot find COM.INFORMATIMAGO.COMMON-LISP.PACKAGE"))

(push (function package:package-system-definition)
      asdf:*system-definition-search-functions*)

(import '(com.informatimago.common-lisp.package:define-package
          com.informatimago.common-lisp.package:list-all-symbols
          com.informatimago.common-lisp.package:list-external-symbols))
(export '(com.informatimago.common-lisp.package:define-package
          com.informatimago.common-lisp.package:list-all-symbols
          com.informatimago.common-lisp.package:list-external-symbols))

(when (fboundp 'common-lisp-user::post-process-logical-pathname-translations)
  (map nil
    (function common-lisp-user::post-process-logical-pathname-translations)
    *logical-hosts*))
  

;;; (setf COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:*package-verbose* t)

;;;----------------------------------------------------------------------
;;; Some personal functions
;;; -----------------------


(defun show (&rest args) (format t "~&~{;; --> ~S~%~}" args))
(export 'show)


(let ((alternate (find-package "COMMON-LISP-USER")))
  (defun pswitch (&optional package)
    (when package (setf alternate (find-package package)))
    (unless (fboundp (intern "PSWITCH" alternate))
      (import '(pswitch)  alternate))
    (setf common-lisp:*package*  (if (eq alternate common-lisp:*package*)
                       (find-package "COMMON-LISP-USER")
                       alternate))))
(export 'pswitch)


(package:load-package "COM.INFORMATIMAGO.COMMON-LISP.BROWSER")
(use-package          "COM.INFORMATIMAGO.COMMON-LISP.BROWSER")
(export '(browse cd pwd pushd popd ls cat less more))


(defmethod documentation ((package t) (doc-type (eql 'exports)))
  (format t "~:{----------------------------------------~%~A~2%~A~2%~}"
          (mapcar (lambda (sym) (list sym (documentation sym 'function)))
                  (delete-if (lambda (sym) (null (documentation sym 'function)))
                             (list-external-symbols package))))
  (format t "Undocumented: ~{~A~^ ~}~%"
          (delete-if (lambda (sym)  (documentation sym 'function))
                     (list-external-symbols package))))


(defun diff-package (p1 p2)
  (let ((*print-readably* t))
    (format t "~2%Symbols exported from ~A not exported from ~A:~%~{  ~S~%~}~%"
            p1 p2
            (set-difference (list-external-symbols p1) (list-external-symbols p2)
                            :test (function eq)))))
(export 'diff-package)  


(defun lspack (&rest arguments)
  (let ((package nil) (exports (member :exports arguments)))
    (when (car arguments) (setf package (car arguments)))
    (flet
      ((lpac (title plist)
             (when plist
               (setf plist (mapcar
                            (lambda (name) (if (string= "" name) "<empty>" name))
                            (sort (mapcar (lambda (item)
                                            (etypecase item
                                              (string  item)
                                              (symbol  (string item))
                                              (package (package-name item))))
                                          plist)
                                  (function string<))))
               (let ((out (format nil "~{~A ~}" plist)))
                 (if (< (length out) 60)
                   (format t "   ~14A ~A~%" title out)
                   (format t "   ~14A~{ ~<~%                  ~1:;~A~>~^~}~%"
                           title plist))))))
      (let* ((packlist
              (if package
                (list package)
                (sort (copy-list (list-all-packages))
                      (function string<) :key (function package-name))))
             (name-width
              (loop for p in packlist
                    maximize (length (package-name p))))
             (numb-width
              (loop for p in packlist
                    maximize (truncate
                              (1+ (log (max (length (list-external-symbols p))
                                            (length (list-all-symbols p)) 3)
                                       10))))))
        (dolist (package packlist)
          (format t "~%~A~%   ~14A ~VD exported, ~VD total.~%"
                  (package-name package)
                  "Symbols:"
                  numb-width (length (list-external-symbols package))
                  numb-width (length (list-all-symbols package)))
          (lpac "Nicknames:" (package-nicknames package))
          (lpac "Uses:"      (package-use-list package))
          (lpac "Used by:"   (package-used-by-list package))
          (when exports
            (lpac "Exported:" (list-external-symbols package))))
        (values)))))

(export 'lspack)


(defun lschar (&key (start 0) (end #x11000) name)
  (if name
      (loop for code from start below end
         when
           #+clisp (regexp:match name (char-name (code-char code)))
           #-clisp (search name (char-name (code-char code))  
                           :test (function equalp))
         do (format t "#x~5,'0X  ~:*~6D  ~C  ~S~%"
                    code (code-char code) (char-name (code-char code))))
      (loop for code from start below end
         do (format t "#x~5,'0X  ~:*~6D  ~C  ~S~%"
                    code (code-char code) (char-name (code-char code))))))
(export 'lschar)


;;;----------------------------------------------------------------------
;;; EDIT --
;;; -------

;;; editor-name is redefined in config.lisp to be:
;;; (defun editor-name () (or (getenv "EDITOR") *editor*))

(defun get-first-word (string)
  "
RETURN:     The first word of the string, or the empty string.
"
  (do ((i 0)
       (j 0)
       (found nil)
       (done nil))
      (done (if found (subseq string i  j) ""))
    (if  (<= (length string) i)
      (setf done t found nil)
      (if (<= j i)
        (if (alpha-char-p (char string i))
          (setf j (1+ i))
          (incf i))
        (if (<= (length string) j)
          (setf done t found t)
          (if (alpha-char-p (char string j))
            (incf j)
            (setf done t found t)))))))


(defvar *editor* (if (fboundp 'ed)
                   (function ed)
                   (lambda (&rest args)
                     (declare (ignore args))
                     (error "This implementation doesn't have an ED")))
  "The editor function provided by the implementation.")


;;; (defvar *edit-log-path* (make-pathname :name "edit-log" :type "lisp")
;;;   "The path to the file where edits of functions are appended.")


(defun edit (&optional item &key (wait t wait-p))
  "
DO:         Create FILE if it doesn't exist, and
            Calls  with the FILE argument.
"
  (setf item (or item
                 (make-pathname :directory '(:absolute "tmp")
                                :name "scratch" :type "lisp")))
  (flet ((doedit (item)
          (cond
           ((null *editor*) (warn "There's no editor (null *editor*)"))
           ((eq *editor* (function ed)) (funcall *editor* item))
           (wait-p (handler-case (funcall *editor* item :wait wait)
                     ((or simple-keyword-error simple-program-error
                          simple-source-program-error)
                      () (funcall *editor* item))))
           (t (funcall *editor* item)))))
    (cond
     ((or (functionp item)
          (and (or (pathnamep item) (stringp item)) (probe-file item)))
      (doedit item))
     ((symbolp item)
      (if (symbol-package item)
        (let ((*package* (symbol-package item)))
          (doedit item))
        (doedit item)))
     (t (loop
         (format *query-io*
           "File ~S does not exist. Should I create it? " item)
         (finish-output *query-io*)
         (let ((line (string-upcase ; small optimization to avoid STRING-EQUAL.
                      (get-first-word (let ((*read-eval* nil))
                                        (read-line *query-io* nil :no))))))
           (cond
            ((member line '("YES" "Y" "JA" "J" "SI" "S" "OUI" "O" "T")
                     :test (function string=))
             (close (open item :direction :output))
             (return-from edit (doedit  (truename item))))
            ((member line '("NO" "N" "NON" "NEIN" "NIL")
                     :test (function string=))
             (format *error-output* "EDIT OF ~S CANCELED." item)
             (finish-output *error-output*)
             (return-from edit nil)))))))))

(export '(edit *editor*))


(defun print-bug-report-info ()
  (format t "~2%~{~28A ~S~%~}~2%"
          (list "LISP-IMPLEMENTATION-TYPE"    (lisp-implementation-type)
                "LISP-IMPLEMENTATION-VERSION" (lisp-implementation-version)
                "SOFTWARE-TYPE"               (software-type)
                "SOFTWARE-VERSION"            (software-version)
                "MACHINE-INSTANCE"            (machine-instance)
                "MACHINE-TYPE"                (machine-type)
                "MACHINE-VERSION"             (machine-version)
                "*FEATURES*"                  *features*))
  (values))

(export 'print-bug-report-info)



;;;---------------------------------------------------------------------
;;; Initialisations spécicique à clisp
;;;---------------------------------------------------------------------

(load "ipl-clx")
(load "ipl-turtles")

(defpackage "IPL-USER"
  (:use "IPL" "IPL-CLX" "IPL-TURTLES"))

(in-package "IPL-USER")

;;; /usr/local/bin/clisp -ansi -q -K full -m 32M -I -Efile ISO-8859-15 -Epathname ISO-8859-15 -Eterminal UTF-8 -Emisc UTF-8 -Eforeign  ISO-8859-15 -L french

;;;----------------------------------------------------------------------
;;; Setting environment -- clisp part --
;;; ------------------------------------

(defun post-process-logical-pathname-translations (host)
  (flet ((pstring (x) (if (pathnamep x) (namestring x) (string x))))
    (setf (logical-pathname-translations host)
          (sort (remove-if
                 (lambda (p) (let ((p (pstring p)))
                          (and (< 5 (length p))
                               (string= "*.*.*" p :start2 (- (length p) 5)))))
                 (logical-pathname-translations host)
                 :key (function first))
                (lambda (a b) (> (length (pstring (first a)))
                            (length (pstring (first b)))))))))

;;;----------------------------------------------------------------------
;;; Setting environment -- COMMON-LISP part --
;;; ------------------------------------------

 ;; (LOAD (MERGE-PATHNAMES
 ;;        (MAKE-PATHNAME :NAME ".common" :TYPE "lisp") (USER-HOMEDIR-PATHNAME)))


;;;----------------------------------------------------------------------
;;; Setting environment -- CLISP specific --
;;; ----------------------------------------
(setf *load-verbose* nil)

(setf custom:*load-paths*     '(#p"")
      custom:*inspect-length* 80
      custom:*editor*         "/usr/local/emacs-multitty/bin/emacsclient"
      custom:*browser*        nil #+(or):mozilla-remote
      custom:*clhs-root-default*
      "http://thalassa.informatimago.com/local/lisp/HyperSpec/"
      custom:*current-language*
      (cons 'i18n:français "/usr/local/languages/clisp/share/locale/"))

(setf *editor* 
      (lambda (arg &key (wait t))
        (if (or (functionp arg) (symbolp arg))
          (ed arg)
          (ext:shell (format nil
                       "/usr/local/emacs-multitty/bin/emacsclient ~
                       ~:[-n~;~] ~A" wait arg)))))


;;; We'll use clocc xlib\clx\clue
;;; (WHEN (FIND-PACKAGE "XLIB") (DELETE-PACKAGE (FIND-PACKAGE "XLIB")))

;;;----------------------------------------------------------------------
;;; Awfull trick for com.informatimago.clisp.script:is-running:

(defun executable-reader (a b c) (sys::unix-executable-reader a b c))
(set-dispatch-macro-character #\# #\! (function executable-reader))

(defun quit () (ext:quit))
(export 'quit)

(push (function ext:cd)
      com.informatimago.common-lisp.browser:*change-directory-hook*)
(cd (ext:cd))     



(defun sh (command)
  (let ((args (delete "" (split-string command " "))))
    (with-open-stream (in (ext:run-program (first args)
                            :arguments (cdr args) :output :stream))
      (loop for line = (read-line in nil nil)
            while line do (princ line) (terpri)))));;SH

#+(and unix macos)
 (setf custom:*default-file-encoding* charset:iso-8859-15
       #+ffi system::foreign-encoding #+ffi charset:iso-8859-15
       custom:*misc-encoding*         charset:utf-8 ; best same as terminal
       custom:*terminal-encoding*     charset:utf-8 
       custom:*pathname-encoding*     charset:utf-8)


(defmacro define-encoding-macro (name symbol-macro)
  `(defmacro ,name (encoding &body body)
    (with-gensyms (saved-encoding)
      `(let ((,saved-encoding ,',symbol-macro))
         (unwind-protect (progn (setf ,',symbol-macro ,encoding)
                                ,@body)
           (setf ,',symbol-macro  ,saved-encoding))))))

(define-encoding-macro with-file-encoding     custom:*default-file-encoding*)
(define-encoding-macro with-pathname-encoding custom:*pathname-encoding*)
(define-encoding-macro with-terminal-encoding custom:*terminal-encoding*)
(define-encoding-macro with-misc-encoding     custom:*misc-encoding*)
#+ffi (define-encoding-macro with-foreign-encoding  system::*foreign-encoding*)

(defun lsencod ()
  (format t "~%~{~32A ~A~%~}~%"
          (list
           'custom:*default-file-encoding* custom:*default-file-encoding*
           #+ffi 'custom:*foreign-encoding* #+ffi custom:*foreign-encoding*
           'custom:*misc-encoding*         custom:*misc-encoding*
           'custom:*pathname-encoding*     custom:*pathname-encoding*
           'custom:*terminal-encoding*     custom:*terminal-encoding*
           'system::*http-encoding*        system::*http-encoding*))
  (values))
(export 'lsencod)


;;;; The End ;;;;

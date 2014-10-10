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


(IN-PACKAGE "COMMON-LISP-USER")

(DEFPACKAGE "IPL"
  (:USE "COMMON-LISP")) ; Export à la fin.

(IN-PACKAGE "IPL")

(LET ((SYMLIST '()))
  (DO-EXTERNAL-SYMBOLS (SYM "COMMON-LISP") (PUSH SYM SYMLIST))
  (EXPORT SYMLIST #.(FIND-PACKAGE "IPL")))

;;; ----------------------------------------------------------------------
;;; -- logical hosts -- the Common-Lisp way to PATH --
;;; --------------------------------------------------

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (DEFVAR *LOGICAL-HOSTS* '()))
(EXPORT '*LOGICAL-HOSTS*)

(DEFUN DEF-LP-TRANS (HOST PATH &OPTIONAL (SUBPATH ""))
  (PUSHNEW HOST *LOGICAL-HOSTS* :TEST (FUNCTION STRING-EQUAL))
  ;; If the HOST is already defined we don't change it.
  (UNLESS (HANDLER-CASE (LOGICAL-PATHNAME-TRANSLATIONS HOST) (ERROR () NIL))
    (LET ((DIRECTORY (APPEND (PATHNAME-DIRECTORY PATH)
                             (CDR (PATHNAME-DIRECTORY SUBPATH))
                             '( :WILD-INFERIORS ))))
      (SETF (LOGICAL-PATHNAME-TRANSLATIONS HOST)
            (LIST
             (LIST "**;*"     (MAKE-PATHNAME :DIRECTORY DIRECTORY
                                             :NAME :WILD))
             (LIST "**;*.*"   (MAKE-PATHNAME :DIRECTORY DIRECTORY
                                             :NAME :WILD :TYPE :WILD))
             (LIST "**;*.*.*" (MAKE-PATHNAME :DIRECTORY DIRECTORY
                                             :NAME :WILD :TYPE :WILD
                                             :VERSION :WILD)) )))))

(DEFMACRO MP (PATHNAME &OPTIONAL
                       (DIRECTORY NIL DIRECTORY-P)
                       (NAME      NIL NAME-P)
                       (TYPE      NIL TYPE-P)
                       (VERSION   NIL VERSION-P))
  `(MERGE-PATHNAMES
    (MAKE-PATHNAME,@(WHEN DIRECTORY-P `(:DIRECTORY '(:RELATIVE ,@DIRECTORY)))
                  ,@(WHEN NAME-P      `(:NAME      ,NAME))
                  ,@(WHEN TYPE-P      `(:TYPE      ,TYPE))
                  ,@(WHEN VERSION-P   `(:VERSION   ,VERSION))
                  :DEFAULTS ,PATHNAME)
    ,PATHNAME))


(DEFPARAMETER +SHARE-LISP+ "/usr/local/share/lisp/")
(DEF-LP-TRANS "SHARE-LISP" +SHARE-LISP+)
(DEF-LP-TRANS "CMU-AI"     +SHARE-LISP+ "ai/")
(DEF-LP-TRANS "CL-PDF"     +SHARE-LISP+ "cl-pdf/")
(DEF-LP-TRANS "UFFI"       +SHARE-LISP+ "uffi/")
(DEF-LP-TRANS "PACKAGES"   +SHARE-LISP+ "packages/")
(DEF-LP-TRANS "CLOCC"      +SHARE-LISP+ "packages/net/sourceforge/clocc/clocc/")
(DEF-LP-TRANS "CCLAN"      +SHARE-LISP+ "packages/net/sourceforge/cclan/")
(DEF-LP-TRANS "DEFSYSTEM"  +SHARE-LISP+
  ;; We must go thru a translation for defsystem-3.x isn't a valid logical name!
  "packages/net/sourceforge/clocc/clocc/src/defsystem-3.x/")

(DEFPARAMETER +PJB-COMM+ (MP (USER-HOMEDIR-PATHNAME) ("src" "public" "common")))
(DEFPARAMETER +PJB-LISP+ (MP (USER-HOMEDIR-PATHNAME) ("src" "lisp")))

(DEF-LP-TRANS "HOME"     (USER-HOMEDIR-PATHNAME)   "")
(DEF-LP-TRANS "LOADERS"  +PJB-COMM+        "cl-loaders/")
(DEF-LP-TRANS "NORVIG"   +PJB-LISP+        "norvig/")


;;;----------------------------------------------------------------------
#-SBCL (LOAD "PACKAGES:NET;SOURCEFORGE;CCLAN;ASDF;ASDF.LISP")
#+SBCL (REQUIRE 'ASDF)
#+SBCL (REQUIRE 'ASDF-INSTALL)
;;;(LOAD "PACKAGES:NET;SOURCEFORGE;CCLAN;ASDF;ASDF-INSTALL.LISP")

(DEFPARAMETER *ORIGINAL-ASDF-REGISTRY* ASDF:*CENTRAL-REGISTRY*)

(DEFUN ASDF-RESCAN-PACKAGES ()
  (FORMAT *TRACE-OUTPUT* "~&;; Scanning ASDF packages...~%")
  (PROG1
      (SORT 
       (DELETE-DUPLICATES 
        (MAPCAR
         (LAMBDA (P) (MAKE-PATHNAME :NAME NIL :TYPE NIL :VERSION NIL :DEFAULTS P))
         (DIRECTORY "PACKAGES:**;*.ASD")) 
        :TEST (FUNCTION EQUAL))
       (LAMBDA (A B) (IF (= (LENGTH A) (LENGTH B))
                  (STRING< A B)
                  (< (LENGTH A) (LENGTH B))))
       :KEY (FUNCTION NAMESTRING))
    (FORMAT *TRACE-OUTPUT* "~&;; Done.~%")))

(DEFUN UPDATE-ASDF-REGISTRY ()
  (SETF ASDF:*CENTRAL-REGISTRY*
        (NCONC (ASDF-RESCAN-PACKAGES)
               ;;(list CCLAN-GET::*CCLAN-ASDF-REGISTRY*)
               *ORIGINAL-ASDF-REGISTRY*)))

(EXPORT 'UPDATE-ASDF-REGISTRY)
(UPDATE-ASDF-REGISTRY)

(IN-PACKAGE "ASDF")
(HANDLER-BIND ((WARNING (FUNCTION MUFFLE-WARNING)))
  ;; TODO: we should keep the error message in a string
  ;;       and check it's only the warnings.
  (FLET ((OUTPUT-FILES (C)
           (FLET ((IMPLEMENTATION-ID ()
                    (FLET ((FIRST-WORD (TEXT)
                             (LET ((POS (POSITION (CHARACTER " ") TEXT)))
                               (REMOVE (CHARACTER ".")
                                       (IF POS (SUBSEQ TEXT 0 POS) TEXT)))))
                      (FORMAT NIL
                        "~A-~A-~A"
                        (FIRST-WORD (LISP-IMPLEMENTATION-TYPE))
                        (FIRST-WORD (LISP-IMPLEMENTATION-VERSION))
                        (FIRST-WORD (MACHINE-TYPE))))))
             (LET* ((OBJECT
                      (COMPILE-FILE-PATHNAME (ASDF:COMPONENT-PATHNAME C)))
                    (PATH
                     (MERGE-PATHNAMES
                      (MAKE-PATHNAME :DIRECTORY
                                     (LIST :RELATIVE
                                           (FORMAT NIL
                                             "OBJ-~:@(~A~)"
                                             (IMPLEMENTATION-ID)))
                                     :NAME
                                     (PATHNAME-NAME OBJECT)
                                     :TYPE
                                     (PATHNAME-TYPE OBJECT))
                      OBJECT)))
               (ENSURE-DIRECTORIES-EXIST PATH)
               (LIST PATH)))))
    (DEFMETHOD OUTPUT-FILES ((OPERATION COMPILE-OP) (C CL-SOURCE-FILE))
      (OUTPUT-FILES C))
    (DEFMETHOD OUTPUT-FILES
        ((OPERATION LOAD-OP) (C CL-SOURCE-FILE))
      (OUTPUT-FILES C))))

(IN-PACKAGE "IPL")

;;;----------------------------------------------------------------------
;;; (WHEN *LOAD-VERBOSE*
;;;   (FORMAT T "~& (LOAD \"LOADER:CCLAN.LISP\") ~%")
;;;   (FORMAT T "~& (LOAD \"DEFSYSTEM:DEFSYSTEM.LISP\") ~%"))



(UNLESS (BLOCK :DONE
          (DOLIST (FILE (LIST (MP +SHARE-LISP+ ("common-lisp") "package")
                              (MP +SHARE-LISP+ ("common-lisp") "package" "lisp")
                              (MP +PJB-COMM+ ("common-lisp") "package")
                              (MP +PJB-COMM+ ("common-lisp") "package" "lisp")))
            (HANDLER-CASE (PROGN (LOAD FILE) (RETURN-FROM :DONE T)) (ERROR ())))
          NIL)
  (ERROR "Cannot find COM.INFORMATIMAGO.COMMON-LISP.PACKAGE"))

(PUSH (FUNCTION PACKAGE:PACKAGE-SYSTEM-DEFINITION)
      ASDF:*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*)

(IMPORT '(COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:DEFINE-PACKAGE
          COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:LIST-ALL-SYMBOLS
          COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:LIST-EXTERNAL-SYMBOLS))
(EXPORT '(COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:DEFINE-PACKAGE
          COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:LIST-ALL-SYMBOLS
          COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:LIST-EXTERNAL-SYMBOLS))

(WHEN (FBOUNDP 'COMMON-LISP-USER::POST-PROCESS-LOGICAL-PATHNAME-TRANSLATIONS)
  (MAP NIL
    (FUNCTION COMMON-LISP-USER::POST-PROCESS-LOGICAL-PATHNAME-TRANSLATIONS)
    *LOGICAL-HOSTS*))
  

;;; (setf COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:*package-verbose* t)

;;;----------------------------------------------------------------------
;;; Some personal functions
;;; -----------------------


(DEFUN SHOW (&REST ARGS) (FORMAT T "~&~{;; --> ~S~%~}" ARGS))
(EXPORT 'SHOW)


(LET ((ALTERNATE (FIND-PACKAGE "COMMON-LISP-USER")))
  (DEFUN PSWITCH (&OPTIONAL PACKAGE)
    (WHEN PACKAGE (SETF ALTERNATE (FIND-PACKAGE PACKAGE)))
    (UNLESS (FBOUNDP (INTERN "PSWITCH" ALTERNATE))
      (IMPORT '(PSWITCH)  ALTERNATE))
    (SETF COMMON-LISP:*PACKAGE*  (IF (EQ ALTERNATE COMMON-LISP:*PACKAGE*)
                       (FIND-PACKAGE "COMMON-LISP-USER")
                       ALTERNATE))))
(EXPORT 'PSWITCH)


(PACKAGE:LOAD-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.BROWSER")
(USE-PACKAGE          "COM.INFORMATIMAGO.COMMON-LISP.BROWSER")
(EXPORT '(BROWSE CD PWD PUSHD POPD LS CAT LESS MORE))


(DEFMETHOD DOCUMENTATION ((PACKAGE T) (DOC-TYPE (EQL 'EXPORTS)))
  (FORMAT T "~:{----------------------------------------~%~A~2%~A~2%~}"
          (MAPCAR (LAMBDA (SYM) (LIST SYM (DOCUMENTATION SYM 'FUNCTION)))
                  (DELETE-IF (LAMBDA (SYM) (NULL (DOCUMENTATION SYM 'FUNCTION)))
                             (LIST-EXTERNAL-SYMBOLS PACKAGE))))
  (FORMAT T "Undocumented: ~{~A~^ ~}~%"
          (DELETE-IF (LAMBDA (SYM)  (DOCUMENTATION SYM 'FUNCTION))
                     (LIST-EXTERNAL-SYMBOLS PACKAGE))))


(DEFUN DIFF-PACKAGE (P1 P2)
  (LET ((*PRINT-READABLY* T))
  (FORMAT T "~2%Symbols exported from ~A not exported from ~A:~%~{  ~S~%~}~%"
          P1 P2
          (SET-DIFFERENCE (LIST-EXTERNAL-SYMBOLS P1) (LIST-EXTERNAL-SYMBOLS P2)
                          :TEST (FUNCTION EQ)))))
(EXPORT 'DIFF-PACKAGE)  


(DEFUN LSPACK (&REST ARGUMENTS)
  (LET ((PACKAGE NIL) (EXPORTS (MEMBER :EXPORTS ARGUMENTS)))
    (WHEN (CAR ARGUMENTS) (SETF PACKAGE (CAR ARGUMENTS)))
    (FLET
      ((LPAC (TITLE PLIST)
             (WHEN PLIST
               (SETF PLIST (MAPCAR
                            (LAMBDA (NAME) (IF (STRING= "" NAME) "<empty>" NAME))
                            (SORT (MAPCAR (LAMBDA (ITEM)
                                            (ETYPECASE ITEM
                                              (STRING  ITEM)
                                              (SYMBOL  (STRING ITEM))
                                              (PACKAGE (PACKAGE-NAME ITEM))))
                                          PLIST)
                                  (FUNCTION STRING<))))
               (LET ((OUT (FORMAT NIL "~{~A ~}" PLIST)))
                 (IF (< (LENGTH OUT) 60)
                   (FORMAT T "   ~14A ~A~%" TITLE OUT)
                   (FORMAT T "   ~14A~{ ~<~%                  ~1:;~A~>~^~}~%"
                           TITLE PLIST))))))
      (LET* ((PACKLIST
              (IF PACKAGE
                (LIST PACKAGE)
                (SORT (COPY-LIST (LIST-ALL-PACKAGES))
                      (FUNCTION STRING<) :KEY (FUNCTION PACKAGE-NAME))))
             (NAME-WIDTH
              (LOOP FOR P IN PACKLIST
                    MAXIMIZE (LENGTH (PACKAGE-NAME P))))
             (NUMB-WIDTH
              (LOOP FOR P IN PACKLIST
                    MAXIMIZE (TRUNCATE
                              (1+ (LOG (MAX (LENGTH (LIST-EXTERNAL-SYMBOLS P))
                                            (LENGTH (LIST-ALL-SYMBOLS P)) 3)
                                       10))))))
        (DOLIST (PACKAGE PACKLIST)
          (FORMAT T "~%~A~%   ~14A ~VD exported, ~VD total.~%"
                  (PACKAGE-NAME PACKAGE)
                  "Symbols:"
                  NUMB-WIDTH (LENGTH (LIST-EXTERNAL-SYMBOLS PACKAGE))
                  NUMB-WIDTH (LENGTH (LIST-ALL-SYMBOLS PACKAGE)))
          (LPAC "Nicknames:" (PACKAGE-NICKNAMES PACKAGE))
          (LPAC "Uses:"      (PACKAGE-USE-LIST PACKAGE))
          (LPAC "Used by:"   (PACKAGE-USED-BY-LIST PACKAGE))
          (WHEN EXPORTS
            (LPAC "Exported:" (LIST-EXTERNAL-SYMBOLS PACKAGE))))
        (VALUES)))))

(EXPORT 'LSPACK)


(DEFUN LSCHAR (&KEY (START 0) (END #X11000) NAME)
  (IF NAME
      (LOOP FOR CODE FROM START BELOW END
         WHEN
           #+CLISP (REGEXP:MATCH NAME (CHAR-NAME (CODE-CHAR CODE)))
           #-CLISP (SEARCH NAME (CHAR-NAME (CODE-CHAR CODE))  
                           :TEST (FUNCTION EQUALP))
         DO (FORMAT T "#x~5,'0X  ~:*~6D  ~C  ~S~%"
                    CODE (CODE-CHAR CODE) (CHAR-NAME (CODE-CHAR CODE))))
      (LOOP FOR CODE FROM START BELOW END
         DO (FORMAT T "#x~5,'0X  ~:*~6D  ~C  ~S~%"
                    CODE (CODE-CHAR CODE) (CHAR-NAME (CODE-CHAR CODE))))))
(EXPORT 'LSCHAR)


;;;----------------------------------------------------------------------
;;; EDIT --
;;; -------

;;; editor-name is redefined in config.lisp to be:
;;; (defun editor-name () (or (getenv "EDITOR") *editor*))

(DEFUN GET-FIRST-WORD (STRING)
  "
RETURN:     The first word of the string, or the empty string.
"
  (DO ((I 0)
       (J 0)
       (FOUND NIL)
       (DONE NIL))
      (DONE (IF FOUND (SUBSEQ STRING I  J) ""))
    (IF  (<= (LENGTH STRING) I)
      (SETF DONE T FOUND NIL)
      (IF (<= J I)
        (IF (ALPHA-CHAR-P (CHAR STRING I))
          (SETF J (1+ I))
          (INCF I))
        (IF (<= (LENGTH STRING) J)
          (SETF DONE T FOUND T)
          (IF (ALPHA-CHAR-P (CHAR STRING J))
            (INCF J)
            (SETF DONE T FOUND T)))))))


(DEFVAR *EDITOR* (IF (FBOUNDP 'ED)
                   (FUNCTION ED)
                   (LAMBDA (&REST ARGS)
                     (DECLARE (IGNORE ARGS))
                     (ERROR "This implementation doesn't have an ED")))
  "The editor function provided by the implementation.")


;;; (defvar *edit-log-path* (make-pathname :name "edit-log" :type "lisp")
;;;   "The path to the file where edits of functions are appended.")


(DEFUN EDIT (&OPTIONAL ITEM &KEY (WAIT T WAIT-P))
  "
DO:         Create FILE if it doesn't exist, and
            Calls  with the FILE argument.
"
  (SETF ITEM (OR ITEM
                 (MAKE-PATHNAME :DIRECTORY '(:ABSOLUTE "tmp")
                                :NAME "scratch" :TYPE "lisp")))
  (FLET ((DOEDIT (ITEM)
          (COND
           ((NULL *EDITOR*) (WARN "There's no editor (null *editor*)"))
           ((EQ *EDITOR* (FUNCTION ED)) (FUNCALL *EDITOR* ITEM))
           (WAIT-P (HANDLER-CASE (FUNCALL *EDITOR* ITEM :WAIT WAIT)
                     ((OR SIMPLE-KEYWORD-ERROR SIMPLE-PROGRAM-ERROR
                          SIMPLE-SOURCE-PROGRAM-ERROR)
                      () (FUNCALL *EDITOR* ITEM))))
           (T (FUNCALL *EDITOR* ITEM)))))
    (COND
     ((OR (FUNCTIONP ITEM)
          (AND (OR (PATHNAMEP ITEM) (STRINGP ITEM)) (PROBE-FILE ITEM)))
      (DOEDIT ITEM))
     ((SYMBOLP ITEM)
      (IF (SYMBOL-PACKAGE ITEM)
        (LET ((*PACKAGE* (SYMBOL-PACKAGE ITEM)))
          (DOEDIT ITEM))
        (DOEDIT ITEM)))
     (T (LOOP
         (FORMAT *QUERY-IO*
           "File ~S does not exist. Should I create it? " ITEM)
         (FINISH-OUTPUT *QUERY-IO*)
         (LET ((LINE (STRING-UPCASE ; small optimization to avoid STRING-EQUAL.
                      (GET-FIRST-WORD (LET ((*READ-EVAL* NIL))
                                        (READ-LINE *QUERY-IO* NIL :NO))))))
           (COND
            ((MEMBER LINE '("YES" "Y" "JA" "J" "SI" "S" "OUI" "O" "T")
                     :TEST (FUNCTION STRING=))
             (CLOSE (OPEN ITEM :DIRECTION :OUTPUT))
             (RETURN-FROM EDIT (DOEDIT  (TRUENAME ITEM))))
            ((MEMBER LINE '("NO" "N" "NON" "NEIN" "NIL")
                     :TEST (FUNCTION STRING=))
             (FORMAT *ERROR-OUTPUT* "EDIT OF ~S CANCELED." ITEM)
             (FINISH-OUTPUT *ERROR-OUTPUT*)
             (RETURN-FROM EDIT NIL)))))))))

(EXPORT '(EDIT *EDITOR*))


(DEFUN PRINT-BUG-REPORT-INFO ()
  (FORMAT T "~2%~{~28A ~S~%~}~2%"
          (LIST "LISP-IMPLEMENTATION-TYPE"    (LISP-IMPLEMENTATION-TYPE)
                "LISP-IMPLEMENTATION-VERSION" (LISP-IMPLEMENTATION-VERSION)
                "SOFTWARE-TYPE"               (SOFTWARE-TYPE)
                "SOFTWARE-VERSION"            (SOFTWARE-VERSION)
                "MACHINE-INSTANCE"            (MACHINE-INSTANCE)
                "MACHINE-TYPE"                (MACHINE-TYPE)
                "MACHINE-VERSION"             (MACHINE-VERSION)
                "*FEATURES*"                  *FEATURES*))
  (VALUES))

(EXPORT 'PRINT-BUG-REPORT-INFO)



;;;---------------------------------------------------------------------
;;; Initialisations spécicique à clisp
;;;---------------------------------------------------------------------

(LOAD "ipl-clx")
(LOAD "ipl-turtles")

(DEFPACKAGE "IPL-USER"
  (:USE "IPL" "IPL-CLX" "IPL-TURTLES"))

(IN-PACKAGE "IPL-USER")

;;; /usr/local/bin/clisp -ansi -q -K full -m 32M -I -Efile ISO-8859-15 -Epathname ISO-8859-15 -Eterminal UTF-8 -Emisc UTF-8 -Eforeign  ISO-8859-15 -L french

;;;----------------------------------------------------------------------
;;; Setting environment -- clisp part --
;;; ------------------------------------

(DEFUN POST-PROCESS-LOGICAL-PATHNAME-TRANSLATIONS (HOST)
  (FLET ((PSTRING (X) (IF (PATHNAMEP X) (NAMESTRING X) (STRING X))))
    (SETF (LOGICAL-PATHNAME-TRANSLATIONS HOST)
          (SORT (REMOVE-IF
                 (LAMBDA (P) (LET ((P (PSTRING P)))
                          (AND (< 5 (LENGTH P))
                               (STRING= "*.*.*" P :START2 (- (LENGTH P) 5)))))
                 (LOGICAL-PATHNAME-TRANSLATIONS HOST)
                 :KEY (FUNCTION FIRST))
                (LAMBDA (A B) (> (LENGTH (PSTRING (FIRST A)))
                            (LENGTH (PSTRING (FIRST B)))))))))

;;;----------------------------------------------------------------------
;;; Setting environment -- COMMON-LISP part --
;;; ------------------------------------------

 ;; (LOAD (MERGE-PATHNAMES
 ;;        (MAKE-PATHNAME :NAME ".common" :TYPE "lisp") (USER-HOMEDIR-PATHNAME)))


;;;----------------------------------------------------------------------
;;; Setting environment -- CLISP specific --
;;; ----------------------------------------
(SETF *LOAD-VERBOSE* NIL)

(SETF CUSTOM:*LOAD-PATHS*     '(#P"")
      CUSTOM:*INSPECT-LENGTH* 80
      CUSTOM:*EDITOR*         "/usr/local/emacs-multitty/bin/emacsclient"
      CUSTOM:*BROWSER*        NIL #+(OR):MOZILLA-REMOTE
      CUSTOM:*CLHS-ROOT-DEFAULT*
      "http://thalassa.informatimago.com/local/lisp/HyperSpec/"
      CUSTOM:*CURRENT-LANGUAGE*
      (CONS 'I18N:FRANÇAIS "/usr/local/languages/clisp/share/locale/"))

(SETF *EDITOR* 
      (LAMBDA (ARG &KEY (WAIT T))
        (IF (OR (FUNCTIONP ARG) (SYMBOLP ARG))
          (ED ARG)
          (EXT:SHELL (FORMAT NIL
                       "/usr/local/emacs-multitty/bin/emacsclient ~
                       ~:[-n~;~] ~A" WAIT ARG)))))


;;; We'll use clocc xlib\clx\clue
;;; (WHEN (FIND-PACKAGE "XLIB") (DELETE-PACKAGE (FIND-PACKAGE "XLIB")))

;;;----------------------------------------------------------------------
;;; Awfull trick for com.informatimago.clisp.script:is-running:

(DEFUN EXECUTABLE-READER (A B C) (SYS::UNIX-EXECUTABLE-READER A B C))
(SET-DISPATCH-MACRO-CHARACTER #\# #\! (FUNCTION EXECUTABLE-READER))

(DEFUN QUIT () (EXT:QUIT))
(EXPORT 'QUIT)

(PUSH (FUNCTION EXT:CD)
      COM.INFORMATIMAGO.COMMON-LISP.BROWSER:*CHANGE-DIRECTORY-HOOK*)
(CD (EXT:CD))     



(DEFUN SH (COMMAND)
  (LET ((ARGS (DELETE "" (SPLIT-STRING COMMAND " "))))
    (WITH-OPEN-STREAM (IN (EXT:RUN-PROGRAM (FIRST ARGS)
                            :ARGUMENTS (CDR ARGS) :OUTPUT :STREAM))
      (LOOP FOR LINE = (READ-LINE IN NIL NIL)
            WHILE LINE DO (PRINC LINE) (TERPRI)))));;SH

#+(AND UNIX MACOS)
 (SETF CUSTOM:*DEFAULT-FILE-ENCODING* CHARSET:ISO-8859-15
       #+FFI SYSTEM::FOREIGN-ENCODING #+FFI CHARSET:ISO-8859-15
       CUSTOM:*MISC-ENCODING*         CHARSET:UTF-8 ; best same as terminal
       CUSTOM:*TERMINAL-ENCODING*     CHARSET:UTF-8 
       CUSTOM:*PATHNAME-ENCODING*     CHARSET:UTF-8)


(DEFMACRO DEFINE-ENCODING-MACRO (NAME SYMBOL-MACRO)
  `(DEFMACRO ,NAME (ENCODING &BODY BODY)
    (WITH-GENSYMS (SAVED-ENCODING)
      `(LET ((,SAVED-ENCODING ,',SYMBOL-MACRO))
         (UNWIND-PROTECT (PROGN (SETF ,',SYMBOL-MACRO ,ENCODING)
                                ,@BODY)
           (SETF ,',SYMBOL-MACRO  ,SAVED-ENCODING))))))

(DEFINE-ENCODING-MACRO WITH-FILE-ENCODING     CUSTOM:*DEFAULT-FILE-ENCODING*)
(DEFINE-ENCODING-MACRO WITH-PATHNAME-ENCODING CUSTOM:*PATHNAME-ENCODING*)
(DEFINE-ENCODING-MACRO WITH-TERMINAL-ENCODING CUSTOM:*TERMINAL-ENCODING*)
(DEFINE-ENCODING-MACRO WITH-MISC-ENCODING     CUSTOM:*MISC-ENCODING*)
#+FFI (DEFINE-ENCODING-MACRO WITH-FOREIGN-ENCODING  SYSTEM::*FOREIGN-ENCODING*)

(DEFUN LSENCOD ()
  (FORMAT T "~%~{~32A ~A~%~}~%"
          (LIST
           'CUSTOM:*DEFAULT-FILE-ENCODING* CUSTOM:*DEFAULT-FILE-ENCODING*
           #+FFI 'CUSTOM:*FOREIGN-ENCODING* #+FFI CUSTOM:*FOREIGN-ENCODING*
           'CUSTOM:*MISC-ENCODING*         CUSTOM:*MISC-ENCODING*
           'CUSTOM:*PATHNAME-ENCODING*     CUSTOM:*PATHNAME-ENCODING*
           'CUSTOM:*TERMINAL-ENCODING*     CUSTOM:*TERMINAL-ENCODING*
           'SYSTEM::*HTTP-ENCODING*        SYSTEM::*HTTP-ENCODING*))
  (VALUES))
(EXPORT 'LSENCOD)


;;; The End ;;;



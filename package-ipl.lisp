(defpackage "IPL"
  (:use "COMMON-LISP")
  ;; For now, we re-export all the COMMON-LISP symbols:
  (:export . #.(let ((symlist '()))
                 (do-external-symbols (sym "COMMON-LISP" symlist)
                   (push (symbol-name sym) symlist))))
  (:import-from "UIOP" "QUIT")
  (:export "QUIT")
  (:documentation
   "The IPL package exports the Common Lisp symbols we will use."))



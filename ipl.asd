(asdf:defsystem "ipl"
  ;; system attributes:
  :description "Initiation to Programming with Lisp."
  :long-description "

This system provides some easy graphics primitives to have fun
learning programming with Lisp.

"
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.0.0"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Automn 2015")
               ((#:albert #:output-dir)          . "../documentation/ipl/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on ("clx" "babel")
  :components ((:file "package-ipl")
               (:file "ipl-clx"           :depends-on ("package-ipl"))
               (:file "ipl-turtles"       :depends-on ("package-ipl" "ipl-clx"))
               (:file "package-ipl-user"  :depends-on ("package-ipl" "ipl-clx" "ipl-turtles"))
               (:file "ipl-ex-graphic"    :depends-on ("package-ipl-user"))
               (:file "fill-triangle"     :depends-on ("package-ipl-user"))
               (:file "lambda-tess"       :depends-on ("package-ipl-user"))
               (:file "artilerie"         :depends-on ("package-ipl-user"))
               (:file "genphrase")
               (:file "hangman")
               (:file "pendu"))

  #+asdf-unicode :encoding #+asdf-unicode :utf-8)

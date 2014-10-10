;;;; -*- coding: utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ipl-clx.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Ce module defini la gestion d'une fenêtre X dans laquelle
;;;;    on peut dessiner.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-04-19 <PJB> Added documentation strings.
;;;;    2005-10-31 <PJB> Créé.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2005
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
;;;;**************************************************************************

(IN-PACKAGE "IPL")

(DEFPACKAGE "IPL-CLX"
  (:USE "IPL")
  (:EXPORT "X" "Y" "MAKE-POINT" "COPY-POINT" "RETURN-FROM-DO-WINDOW"
           "XENV" "*XENV*" "DEFAULT-XENV" "OPEN-WINDOW" "CLOSE-WINDOW"
           "*WINDOW-MANAGER-ENCODING*" "WM-CLASS" "WINDOW-TITLE"
           "WINDOW-WIDTH" "WINDOW-HEIGHT"
           "PROCESS-EVENTS" "WITH-WINDOW" "DO-WINDOW" "REDRAW-WINDOW"
           "CLEAR-WINDOW"  "SET-RGB-COLOR"  "SET-COLOR"  "POINT" "DRAW-POINT-AT"
           "DRAW-POINT" "DRAW-POINTS" "DRAW-LINE" "DRAW-POLYGON"
           "DRAW-RECTANGLE" "DRAW-ARC" "DRAW-TEXT"
           "*WHITE*" "*LIGHT-GRAY*" "*DARK-GRAY*" "*BLACK*"))

(IN-PACKAGE "IPL-CLX")

;;; (ext:without-package-lock (:posix)
;;;   (defun posix::hostent-addr-type (&rest args)
;;;     (apply (function posix::hostent-addrtype) args)))

(defparameter *system-font-name*
  "-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1")
(defparameter *system-bold-font-name*
  "-adobe-helvetica-medium-r-bold--12-120-75-75-p-67-iso8859-1")
(defparameter *user-font-name*
  "-adobe-times-medium-r-normal--12-120-75-75-p-64-iso8859-1")


(DEFSTRUCT XENV
  "Un environnement X window"
  DISPLAY                               ; le serveur X
  WINDOW                                ; la fenêtre X
  WCONTEXT                     ; le contexte graphique pour la fenêtre
  BACKUP                ; une pixmap contenant une copie de la fenêtre
  BCONTEXT)                      ; le contexte graphique pour la copie

(DEFVAR *XENV* NIL
  "L'environnement X window pris par défaut.")

(DEFUN DEFAULT-XENV ()
  "Retourne l'environnement X window par défaut.
S'il n'existe pas, alors on le crée."
  (WHEN (OR (NULL *XENV*) (NULL (XENV-WINDOW *XENV*)))
    (SETF *XENV* (OPEN-WINDOW)))
  *XENV*)

(DEFUN GETENV (VARIABLE)
  "Retourne la valeur de la variable d'environnement."
  #+CLISP (EXT:GETENV VARIABLE)
  #+SBCL (CDR (ASSOC VARIABLE EXT:*ENVIRONMENT-LIST* :TEST #'STRING=))
  #-(OR CLISP SBCL)
  (ERROR "~A: comment obtenir la valeur d'une variable ~
               d'environnement POSIX ?" 'GETENV))

(DEFUN PARSE-DISPLAY-VARIABLE (S)
  "Analyse le nom du display X et
retourne le nom du serveur, et le numéro du display."
  (LET* ((COLON (POSITION #\: S))
         (DOT (POSITION #\. S :START COLON))
         (HOST-NAME (IF (ZEROP COLON) "localhost" (SUBSEQ S 0 COLON)))
         (DISPLAY-NUMBER (PARSE-INTEGER S :START (1+ COLON) :END DOT)))
    (VALUES HOST-NAME DISPLAY-NUMBER)))


(DEFUN OPEN-WINDOW (&KEY DISPLAY-NAME HOST DISPLAY-NUMBER
                    (X 0) (Y 0) (WIDTH 512) (HEIGHT 342) TITLE
                    (WM-CLASS '("lisp" "LISP")))
  "Ouvre une nouvelle fenêtre sur le display DISPLAY-NUMBER du serveur X HOST.
Au lieu de spécifier HSOT e DISPLAY-NUMBER, on peut spécifier DISPLAY-NAME
avec la syntaxe \"host:display-number[.screen-number]\".
Si on ne spécifie ni l'un ni l'autre alors la variable d'environnement DISPLAY
est utilisée, ou sinon \":0.0\".
Retourne une structure xenv.
"
  (UNLESS (AND HOST DISPLAY-NUMBER)
    (MULTIPLE-VALUE-SETQ (HOST DISPLAY-NUMBER)
      (PARSE-DISPLAY-VARIABLE (OR DISPLAY-NAME (GETENV "DISPLAY") ":0.0"))))
  (LET* ((DISPLAY  (XLIB:OPEN-DISPLAY HOST :DISPLAY DISPLAY-NUMBER))
         (SCREEN   (XLIB:DISPLAY-DEFAULT-SCREEN DISPLAY))
         (BLACK    (XLIB:SCREEN-BLACK-PIXEL SCREEN))
         (WHITE    (XLIB:SCREEN-WHITE-PIXEL SCREEN))
         (WINDOW   (XLIB:CREATE-WINDOW
                    :PARENT (XLIB:SCREEN-ROOT SCREEN)
                    :BACKGROUND WHITE
                    :EVENT-MASK (XLIB:MAKE-EVENT-MASK :BUTTON-RELEASE
                                                      :KEY-PRESS
                                                      :KEY-RELEASE
                                                      :BUTTON-MOTION
                                                      :RESIZE-REDIRECT
                                                      :EXPOSURE
                                                      :VISIBILITY-CHANGE
                                                      :STRUCTURE-NOTIFY )
                    :BIT-GRAVITY   :SOUTH-WEST
                    :X X :Y Y :WIDTH WIDTH :HEIGHT HEIGHT))
         (BACKUP   (XLIB:CREATE-PIXMAP
                    :WIDTH WIDTH :HEIGHT HEIGHT
                    :DEPTH (XLIB:SCREEN-ROOT-DEPTH SCREEN)
                    :DRAWABLE WINDOW))
         (font     (xlib:open-font display *system-font-name*))
         (WCONTEXT (XLIB:CREATE-GCONTEXT :DRAWABLE WINDOW
                                         :BACKGROUND WHITE
                                         :FOREGROUND BLACK
                                         :font font))
         (BCONTEXT (XLIB:CREATE-GCONTEXT :DRAWABLE BACKUP
                                         :BACKGROUND WHITE
                                         :FOREGROUND BLACK
                                         :font font)))
    (LET ((XENV (MAKE-XENV :DISPLAY  DISPLAY
                           :WINDOW   WINDOW
                           :WCONTEXT WCONTEXT
                           :BACKUP   BACKUP
                           :BCONTEXT BCONTEXT)))
      (SETF (WM-CLASS XENV) WM-CLASS)
      (XLIB:MAP-WINDOW WINDOW)
      (XLIB:DISPLAY-FORCE-OUTPUT DISPLAY)
      (CLEAR-WINDOW XENV)
      (WHEN TITLE (SETF (WINDOW-TITLE XENV) TITLE))
      XENV)))


(DEFUN CLOSE-WINDOW (&OPTIONAL (*XENV* (DEFAULT-XENV)))
  "Ferme la fenêtre et le serveur spécifiée par la structure *xenv*."
  (WHEN (XENV-DISPLAY *XENV*)
    (XLIB:FREE-GCONTEXT  (XENV-WCONTEXT *XENV*))
    (XLIB:FREE-GCONTEXT  (XENV-BCONTEXT *XENV*))
    (XLIB:DESTROY-WINDOW (XENV-WINDOW   *XENV*))
    (XLIB:FREE-PIXMAP    (XENV-BACKUP   *XENV*))
    (XLIB:DISPLAY-FORCE-OUTPUT (XENV-DISPLAY  *XENV*))
    (XLIB:CLOSE-DISPLAY        (XENV-DISPLAY  *XENV*))
    (SETF (XENV-BCONTEXT *XENV*) NIL
          (XENV-BACKUP   *XENV*) NIL
          (XENV-WCONTEXT *XENV*) NIL
          (XENV-WINDOW   *XENV*) NIL
          (XENV-DISPLAY  *XENV*) NIL))
  NIL)

(DEFVAR *WINDOW-MANAGER-ENCODING* CHARSET:ISO-8859-1)

(DEFUN (SETF WM-CLASS) (WM-CLASS &OPTIONAL (*XENV* (DEFAULT-XENV)))
  (LET ((BYTES (CONCATENATE 'VECTOR
                 (EXT:CONVERT-STRING-TO-BYTES
                  (FIRST  WM-CLASS) *WINDOW-MANAGER-ENCODING*) #(0)
                 (EXT:CONVERT-STRING-TO-BYTES
                  (SECOND WM-CLASS) *WINDOW-MANAGER-ENCODING*) #(0))))
    (XLIB:CHANGE-PROPERTY (XENV-WINDOW *XENV*) :WM_CLASS     BYTES :STRING 8))
  WM-CLASS)

(DEFUN WM-CLASS (&OPTIONAL (*XENV* (DEFAULT-XENV)))
  (LET ((BYTES (XLIB:GET-PROPERTY (XENV-WINDOW *XENV*) :WM_CLASS :TYPE :STRING
                                  :RESULT-TYPE 'VECTOR)))
    (WHEN BYTES
      (LET ((FIRST-ZERO  (POSITION 0 BYTES)))
        (LIST (EXT:CONVERT-STRING-FROM-BYTES
               BYTES *WINDOW-MANAGER-ENCODING*
               :END FIRST-ZERO)
              (EXT:CONVERT-STRING-FROM-BYTES
               BYTES *WINDOW-MANAGER-ENCODING*
               :START (1+ FIRST-ZERO)
               :END   (POSITION 0 BYTES :START (1+ FIRST-ZERO))))))))


(DEFUN (SETF WINDOW-TITLE) (TITLE &OPTIONAL (*XENV* (DEFAULT-XENV)))
  "Change le titre de la fenêtre."
  (LET ((BYTES (EXT:CONVERT-STRING-TO-BYTES TITLE *WINDOW-MANAGER-ENCODING*)))
    (XLIB:CHANGE-PROPERTY (XENV-WINDOW *XENV*) :WM_NAME      BYTES :STRING 8)
    (XLIB:CHANGE-PROPERTY (XENV-WINDOW *XENV*) :WM_ICON_NAME BYTES :STRING 8))
  TITLE)

(DEFUN WINDOW-TITLE (&OPTIONAL (*XENV* (DEFAULT-XENV)))
  "Retourne le titre de la fenêtre."
  (LET ((BYTES (XLIB:GET-PROPERTY (XENV-WINDOW *XENV*) :WM_NAME :TYPE :STRING
                                  :RESULT-TYPE 'VECTOR)))
    (WHEN BYTES
      (EXT:CONVERT-STRING-FROM-BYTES BYTES *WINDOW-MANAGER-ENCODING*))))


(DEFUN WINDOW-WIDTH (&OPTIONAL (*XENV* (DEFAULT-XENV)))
"Retourne la largeur de la fenêtre."
  (XLIB:DRAWABLE-WIDTH (XENV-WINDOW *XENV*)))

(DEFUN WINDOW-HEIGHT (&OPTIONAL (*XENV* (DEFAULT-XENV)))
"Retourne la hauteur de la fenêtre."
  (XLIB:DRAWABLE-HEIGHT (XENV-WINDOW *XENV*)))


(DEFUN PROCESS-EVENTS (XENV &KEY TIMEOUT)
  "Traite un évènement X pour l'environnement X XENV. 
Retourne lorsqu'un évènement est traité, ou si TIMEOUT est non NIL, 
quand le nombre de secondes indiqué est écoulé."
  (XLIB:EVENT-CASE ((XENV-DISPLAY XENV) :TIMEOUT TIMEOUT)
    (:EXPOSURE (COUNT) ; Discard all but final :exposure then display the menu
     (WHEN (ZEROP COUNT) (REDRAW-WINDOW XENV))
     T)
    (OTHERWISE ()                  ; Ignore and discard any other event
     T)))


(DEFMACRO WITH-WINDOW ((&KEY XENV DISPLAY-NAME HOST DISPLAY-NUMBER
                             (X 0) (Y 0) (WIDTH 512) (HEIGHT 342)
                             (TITLE) (WM-CLASS '("lisp""LISP")))
                       &BODY BODY)
  "Si un display est spécifié, alors ouvre une nouvelle fenêtre, sinon utilise
la structure xenv, ou *xenv*.
Si une nouvelle fenêtre a été ouverte, alors elle est fermée à la fin.
BODY doit appeler PROCESS-EVENTS périodiquement."
  (IF (OR (NULL XENV) DISPLAY-NAME HOST DISPLAY-NUMBER)
      ;; nouvelle fenêtre
      `(LET ((*XENV* (OPEN-WINDOW :DISPLAY-NAME ,DISPLAY-NAME
                                  :HOST ,HOST :DISPLAY-NUMBER ,DISPLAY-NUMBER
                                  :X ,X :Y ,Y :WIDTH ,WIDTH :HEIGHT ,HEIGHT)))
         (UNWIND-PROTECT (PROGN ,@BODY) (CLOSE-WINDOW *XENV*)))
      ;; xenv existant
      `(LET ((*XENV* (OR ,XENV (DEFAULT-XENV)))) ,@BODY)))


(DEFMACRO DO-WINDOW ((&KEY XENV DISPLAY-NAME HOST DISPLAY-NUMBER
                           (X 0) (Y 0) (WIDTH 512) (HEIGHT 342)
                           (TITLE) (WM-CLASS '("lisp""LISP"))
                           (TIMEOUT NIL))
                     &BODY BODY)
  "Si un display est spécifié, alors ouvre une nouvelle fenêtre, sinon utilise
la structure xenv, ou *xenv*.
Si une nouvelle fenêtre a été ouverte, alors elle est fermée à la fin.
BODY est exécuté, puis PROCESS-EVENT est appelé, avec le TIMEOUT specifié.
RETURN-FROM-DO-WINDOW doit être appelé pour sortir de la boucle."
  (LET ((NAME (GENSYM)))
    (IF (OR (NULL XENV) DISPLAY-NAME HOST DISPLAY-NUMBER)
        ;; nouvelle fenêtre
        `(LET ((*XENV* (OPEN-WINDOW :DISPLAY-NAME ,DISPLAY-NAME
                                    :HOST ,HOST :DISPLAY-NUMBER ,DISPLAY-NUMBER
                                    :X ,X :Y ,Y :WIDTH ,WIDTH :HEIGHT ,HEIGHT)))
           (UNWIND-PROTECT
                (LOOP :NAMED ,NAME :DO
                   (FLET ((RETURN-FROM-DO-WINDOW (&OPTIONAL RESULT)
                            (RETURN-FROM ,NAME RESULT)))
                     ,@BODY)
                   (PROCESS-EVENTS *XENV* :TIMEOUT ,TIMEOUT))
             (CLOSE-WINDOW *XENV*)))
        ;; xenv existant
        `(LET ((*XENV* (OR ,XENV (DEFAULT-XENV))))
           (LOOP :NAMED ,NAME :DO
                   (FLET ((RETURN-FROM-DO-WINDOW (&OPTIONAL RESULT)
                            (RETURN-FROM ,NAME RESULT)))
                     ,@BODY)
                   (PROCESS-EVENTS *XENV* :TIMEOUT ,TIMEOUT))))))


;;;--------------------
;;; Redraw
;;;--------------------

(DEFUN REDRAW-WINDOW (&OPTIONAL (*XENV* (DEFAULT-XENV)))
  "Rafraichi la fenêtre."
  (LET ((DISPLAY  (XENV-DISPLAY  *XENV*))
        (WINDOW   (XENV-WINDOW   *XENV*))
        (WCONTEXT (XENV-WCONTEXT *XENV*))
        (BACKUP   (XENV-BACKUP   *XENV*)))
    (XLIB:COPY-AREA BACKUP WCONTEXT 0 0
                    (XLIB:DRAWABLE-WIDTH  WINDOW)
                    (XLIB:DRAWABLE-HEIGHT WINDOW)
                    WINDOW 0 0)
    (XLIB:DISPLAY-FORCE-OUTPUT DISPLAY))
  (VALUES))


(DEFUN CLEAR-WINDOW (&OPTIONAL (*XENV* (DEFAULT-XENV)))
  "Efface le contenu de la fenêtre."
  (LET* ((DISPLAY  (XENV-DISPLAY  *XENV*))
         (SCREEN   (XLIB:DISPLAY-DEFAULT-SCREEN DISPLAY))
         (WHITE    (XLIB:SCREEN-WHITE-PIXEL SCREEN))
         (BACKUP   (XENV-BACKUP   *XENV*))
         (BCONTEXT (XENV-BCONTEXT *XENV*))
         (OLDFORE  (XLIB:GCONTEXT-FOREGROUND BCONTEXT))
         (WINDOW   (XENV-WINDOW   *XENV*)))
    (UNWIND-PROTECT
         (PROGN
           (SETF (XLIB:GCONTEXT-FOREGROUND BCONTEXT) WHITE)
           (XLIB:DRAW-RECTANGLE BACKUP BCONTEXT 0 0
                                (XLIB:DRAWABLE-WIDTH  BACKUP)
                                (XLIB:DRAWABLE-HEIGHT BACKUP) T))
      (SETF (XLIB:GCONTEXT-FOREGROUND BCONTEXT) OLDFORE))
    (XLIB:CLEAR-AREA WINDOW)
    (XLIB:DISPLAY-FORCE-OUTPUT DISPLAY))
  (VALUES))


;;;--------------------
;;; Colors
;;;--------------------

(DEFUN RGB (R G B)
  "Retourne une couleur spécifiée par ses composantes rouge, verte et bleue."
  (DPB R  (BYTE 8 16) (DPB G (BYTE 8 8) (LDB (BYTE 8 0) B))))

(defparameter *white*      (rgb #xff #xff #xff) "Couleur: blanc")
(defparameter *light-gray* (rgb #xAA #xAA #xAA) "Couleur: gris clair")
(defparameter *dark-gray*  (rgb #x55 #x55 #x55) "Couleur: gris foncé")
(defparameter *black*      (rgb #x00 #x00 #x00) "Couleur: noir")

(DEFUN SET-FOREGROUND-COLOR (WINDOW CONTEXT COLOR)
  "Change la couleur du pinceau du contexte."
  (DECLARE (IGNORE WINDOW))
  (SETF (XLIB:GCONTEXT-FOREGROUND CONTEXT) COLOR))

(DEFUN SET-RGB-COLOR (R G B &KEY (XENV (DEFAULT-XENV)))
  "Change la couleur spécifiée par ses composantes rouge, verte et bleue,
  du pinceau du contexte."
  (SET-COLOR (RGB R G B) :XENV XENV))

(DEFUN SET-COLOR (COLOR &KEY (XENV (DEFAULT-XENV)))
  "Change la couleur du pinceau."
  (SETF (XLIB:GCONTEXT-FOREGROUND (XENV-WCONTEXT XENV)) COLOR)
  (SETF (XLIB:GCONTEXT-FOREGROUND (XENV-BCONTEXT XENV)) COLOR))


;;;--------------------
;;; Drawing
;;;--------------------

(DEFSTRUCT (POINT (:TYPE LIST) (:CONC-NAME NIL)) X Y)
(DEFUN POINT (X Y) "Retourne un nouveau point 2D." (MAKE-POINT :X X :Y Y))


(DEFUN DRAW-POINT-AT (X Y  &KEY (XENV (DEFAULT-XENV)))
  "Dessine un point aux coordonnées X,Y indiquées."
  (LET* ((HEIGHT (XLIB:DRAWABLE-HEIGHT (XENV-WINDOW XENV)))
         (X      (ROUND X))
         (Y      (- HEIGHT (ROUND Y))))
    (XLIB:DRAW-POINT (XENV-BACKUP XENV) (XENV-BCONTEXT XENV) X Y)
    (XLIB:DRAW-POINT (XENV-WINDOW XENV) (XENV-WCONTEXT XENV) X Y)
    (XLIB:DISPLAY-FORCE-OUTPUT (XENV-DISPLAY XENV)))
  (VALUES))


(DEFUN DRAW-POINT (POINT  &KEY (XENV (DEFAULT-XENV)))
  "Dessine le POINT."
  (DRAW-POINT-AT (X POINT) (Y POINT) XENV))


(DEFUN DRAW-POINTS (POINTS  &KEY (XENV (DEFAULT-XENV)))
  "Dessine une list de POINTS."
  (LET* ((HEIGHT (XLIB:DRAWABLE-HEIGHT (XENV-WINDOW XENV)))
         (POINTS (MAPCAR (LAMBDA (PT) (LIST (ROUND (X PT)) (- HEIGHT (ROUND (Y PT)))))
                         (CONS (CAR (LAST POINTS)) POINTS))))
    (XLIB:DRAW-POINTS (XENV-BACKUP XENV) (XENV-BCONTEXT XENV) POINTS)
    (XLIB:DRAW-POINTS (XENV-WINDOW XENV) (XENV-WCONTEXT XENV) POINTS)
    (XLIB:DISPLAY-FORCE-OUTPUT (XENV-DISPLAY XENV)))
  (VALUES))


(DEFUN DRAW-LINE (X1 Y1 X2 Y2 &KEY (XENV (DEFAULT-XENV)))
  "Dessine une ligne entre les points X1,Y1 et X2,Y2."
  (LET* ((HEIGHT (XLIB:DRAWABLE-HEIGHT (XENV-WINDOW XENV)))
         (X1 (ROUND X1))
         (X2 (ROUND X2))
         (Y1 (- HEIGHT (ROUND Y1)))
         (Y2 (- HEIGHT (ROUND Y2))))
    (XLIB:DRAW-LINE (XENV-BACKUP XENV) (XENV-BCONTEXT XENV) X1 Y1 X2 Y2)
    (XLIB:DRAW-LINE (XENV-WINDOW XENV) (XENV-WCONTEXT XENV) X1 Y1 X2 Y2)
    (XLIB:DISPLAY-FORCE-OUTPUT (XENV-DISPLAY XENV)))
  (VALUES))


(DEFUN DRAW-POLYGON (POINTS FILLP &KEY (XENV (DEFAULT-XENV)))
  "Dessine un polygone défini par ses sommets POINTS. 
Si FILLP est vrai, alors le rempli."
  (LET* ((HEIGHT (XLIB:DRAWABLE-HEIGHT (XENV-WINDOW XENV)))
         (POINTS (MAPCAN (LAMBDA (PT) (LIST (ROUND (X PT)) (- HEIGHT (ROUND (Y PT)))))
                           (CONS (CAR (LAST POINTS)) POINTS))))
    (XLIB:DRAW-LINES (XENV-BACKUP XENV) (XENV-BCONTEXT XENV)
                     POINTS :FILL-P FILLP)
    (XLIB:DRAW-LINES (XENV-WINDOW XENV) (XENV-WCONTEXT XENV)
                     POINTS :FILL-P FILLP)
    (XLIB:DISPLAY-FORCE-OUTPUT (XENV-DISPLAY XENV)))
  (VALUES))


(DEFUN DRAW-RECTANGLE (X Y WIDTH HEIGHT FILLP &KEY (XENV (DEFAULT-XENV)))

  "Dessine un rectangle parallèle aux coordonnées, dont le point en
bas à gauche est X,Y, et de largeur WIDTH et de hauteur HEIGHT.
Si FILLP est vrai, alors le rempli."
  (LET* ((WHEIGHT (XLIB:DRAWABLE-HEIGHT (XENV-WINDOW XENV)))
         (WIDTH  (ROUND WIDTH))
         (HEIGHT (ROUND HEIGHT))
         (X      (ROUND X))
         (Y      (- WHEIGHT (ROUND Y) HEIGHT)))
    (XLIB:DRAW-RECTANGLE (XENV-BACKUP XENV) (XENV-BCONTEXT XENV)
                         X Y WIDTH HEIGHT FILLP)
    (XLIB:DRAW-RECTANGLE (XENV-WINDOW XENV) (XENV-WCONTEXT XENV)
                         X Y WIDTH HEIGHT FILLP)
    (XLIB:DISPLAY-FORCE-OUTPUT (XENV-DISPLAY XENV)))
  (VALUES))


(DEFUN DRAW-ARC (X Y WIDTH HEIGHT angle-1 angle-2 FILLP
                 &KEY (XENV (DEFAULT-XENV)))

  "Dessine un arc parallèle aux coordonnées, dont le point en
bas à gauche est X,Y, et de largeur WIDTH et de hauteur HEIGHT.
Si FILLP est vrai, alors le rempli.
Les angles sont donnés en radian comptés sur l'axe Ox"
  (LET* ((WHEIGHT (XLIB:DRAWABLE-HEIGHT (XENV-WINDOW XENV)))
         (WIDTH  (ROUND WIDTH))
         (HEIGHT (ROUND HEIGHT))
         (X      (ROUND X))
         (Y      (- WHEIGHT (ROUND Y) HEIGHT)))
    (XLIB:DRAW-ARC (XENV-BACKUP XENV) (XENV-BCONTEXT XENV)
                   X Y WIDTH HEIGHT angle-1 angle-2 fillp)
    (XLIB:DRAW-ARC (XENV-WINDOW XENV) (XENV-BCONTEXT XENV)
                   X Y WIDTH HEIGHT angle-1 angle-2 fillp)
    (XLIB:DISPLAY-FORCE-OUTPUT (XENV-DISPLAY XENV)))
  (VALUES))


(defun draw-text (x y string &KEY (fillp t) (XENV (DEFAULT-XENV)))
  "Dessine le texte STRING aux coordonnées X,Y."
  (LET* ((HEIGHT (XLIB:DRAWABLE-HEIGHT (XENV-WINDOW XENV)))
         (X      (ROUND X))
         (Y      (- HEIGHT (ROUND Y))))
    (if fillp
        (progn
          (xlib:draw-image-glyphs (xenv-backup xenv) (xenv-bcontext xenv)
                                  x y string)
          (xlib:draw-image-glyphs (xenv-window xenv) (xenv-wcontext xenv)
                                  x y string))
        (progn
          (xlib:draw-glyphs (xenv-backup xenv) (xenv-bcontext xenv) x y string)
          (xlib:draw-glyphs (xenv-window xenv) (xenv-wcontext xenv) x y string)))
    (XLIB:DISPLAY-FORCE-OUTPUT (XENV-DISPLAY XENV)))
  (values))

;;;-------------------------------------------
;;; Une boucle REPL traitant les évènements X
;;;-------------------------------------------



;;;--------------------
;;; Tests
;;;--------------------

(DEFUN TEST-1 ()
  (LET ((TIME (GET-UNIVERSAL-TIME))) 
    (DO-WINDOW (:TIMEOUT 1)
      (WHEN (<= 1 (- (GET-UNIVERSAL-TIME) TIME))
        (SETF TIME (GET-UNIVERSAL-TIME))
        (SET-RGB-COLOR (RANDOM 256)  (RANDOM 256)  (RANDOM 256))
        (DRAW-RECTANGLE (RANDOM 500) (RANDOM 300)
                        (RANDOM 500) (RANDOM 300) T)))))


(DEFUN TEST-PROPERTIES ()
  (WITH-WINDOW ()
    (PRINT (WINDOW-TITLE))
    (SLEEP 1)
    (PRINT (SETF (WINDOW-TITLE) "¿Donde esta la casa de mi amigo?"))
    (SLEEP 1)
    (PRINT (WINDOW-TITLE))
    (SLEEP 10)))



(defun test-nx ()
  
  (defclass view ()
    ((left   :accessor left   :initarg :left)
     (bottom :accessor bottom :initarg :bottom)
     (width  :accessor width  :initarg :width)
     (height :accessor height :initarg :height)))

  (defmethod top   ((self view)) (+ (bottom self) (height self)))
  (defmethod right ((self view)) (+ (left self) (width self)))

  (defclass button (view)
    ((title :accessor title :initarg :title)))

  (defmethod draw ((self button))
    (set-color *light-gray*)
    (draw-rectangle (left self) (bottom self) (width self) (height self) t)
    (set-color *black*)
    (draw-rectangle (left self) (bottom self) (width self) (height self) nil)
    (set-color *white*)
    (draw-line (left self) (bottom self) (left  self) (top self))
    (draw-line (left self) (top    self) (right self) (top self))
    (set-color *dark-gray*)
    (draw-point-at (left self) (bottom self))
    (draw-point-at (right self) (top self))
    (when (slot-boundp self 'title)
      (set-color *black*)
      (draw-text (+ 4 (left self)) (+ 5 (bottom self))
                 (if (stringp (title self))
                     (title self)
                     (format nil "~A" (title self))))))

      
                                        ;(progn (close-window)(setf *xenv* nil))
  (set-color *light-gray*)
  (draw-rectangle 0 0 (window-width) (window-height) t)
  (draw (make-instance 'button
          :left 10 :bottom 40 :width 70 :height 20 :title "OK")))





;;; Local Variables:
;;; eval: (cl-indent 'xlib:event-case '((&whole 6 1 1 1 1 1 1) &rest (&whole 2 1 1 1 1 1 1 1 1 1 1 1)))
;;; End:


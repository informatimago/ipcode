;;;; -*- mode:lisp; coding:utf-8 -*-
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
;;;;    Copyright Pascal Bourguignon 2005 - 2014
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
(in-package "IPL")
(defpackage "IPL-CLX"
  (:use "IPL" "BABEL")
  (:export "X" "Y" "MAKE-POINT" "COPY-POINT" "RETURN-FROM-DO-WINDOW"
           "XENV" "*XENV*" "DEFAULT-XENV" "OPEN-WINDOW" "CLOSE-WINDOW"
           "*WINDOW-MANAGER-ENCODING*" "WM-CLASS" "WINDOW-TITLE"
           "WINDOW-WIDTH" "WINDOW-HEIGHT"
           "PROCESS-EVENTS" "WITH-WINDOW" "DO-WINDOW" "REDRAW-WINDOW"
           "CLEAR-WINDOW"  "SET-RGB-COLOR"  "SET-COLOR"  "POINT" "DRAW-POINT-AT"
           "DRAW-POINT" "DRAW-POINTS" "DRAW-LINE" "DRAW-POLYGON"
           "DRAW-RECTANGLE" "DRAW-ARC" "DRAW-TEXT"
           "*WHITE*" "*LIGHT-GRAY*" "*DARK-GRAY*" "*BLACK*")
  (:documentation "The IPL-CLX package exports a simplified set of graphics primitives."))
(in-package "IPL-CLX")

;;; (ext:without-package-lock (:posix)
;;;   (defun posix::hostent-addr-type (&rest args)
;;;     (apply (function posix::hostent-addrtype) args)))

(defparameter *system-font-name*
  "-adobe-helvetica-medium-r-normal--12-120-75-75-p-67-iso8859-1")
(defparameter *system-bold-font-name*
  "-adobe-helvetica-medium-r-bold--12-120-75-75-p-67-iso8859-1")
(defparameter *user-font-name*
  "-adobe-times-medium-r-normal--12-120-75-75-p-64-iso8859-1")


(defstruct xenv
  "Un environnement X window"
  display                               ; le serveur X
  window                                ; la fenêtre X
  wcontext                     ; le contexte graphique pour la fenêtre
  backup                ; une pixmap contenant une copie de la fenêtre
  bcontext)                      ; le contexte graphique pour la copie

(defvar *xenv* nil
  "L'environnement X window pris par défaut.")

(defun default-xenv ()
  "Retourne l'environnement X window par défaut.
S'il n'existe pas, alors on le crée."
  (when (or (null *xenv*) (null (xenv-window *xenv*)))
    (setf *xenv* (open-window)))
  *xenv*)

(defun getenv (variable)
  "Retourne la valeur de la variable d'environnement."
  #+clisp (ext:getenv variable)
  #+ccl   (ccl:getenv variable)
  #+sbcl  (cdr (assoc variable ext:*environment-list* :test #'string=))
  #-(or clisp ccl sbcl)
  (error "~A: comment obtenir la valeur d'une variable ~
               d'environnement POSIX ?" 'getenv))


(defun parse-display-variable (s)
  "Analyse le nom du display X et
retourne le nom du serveur, et le numéro du display."
  (let* ((colon (position #\: s))
         (dot (position #\. s :start colon))
         (host-name (if (zerop colon) "localhost" (subseq s 0 colon)))
         (display-number (parse-integer s :start (1+ colon) :end dot)))
    (values host-name display-number)))


(defun open-window (&key display-name host display-number
                      (x 0) (y 0) (width 512) (height 342) 
                      (title "IPL LISP") (wm-class '("lisp" "LISP")))
  "Ouvre une nouvelle fenêtre sur le display DISPLAY-NUMBER du serveur X HOST.
Au lieu de spécifier HSOT e DISPLAY-NUMBER, on peut spécifier DISPLAY-NAME
avec la syntaxe \"host:display-number[.screen-number]\".
Si on ne spécifie ni l'un ni l'autre alors la variable d'environnement DISPLAY
est utilisée, ou sinon \":0.0\".
Retourne une structure xenv.
"
  (unless (and host display-number)
    (multiple-value-setq (host display-number)
      (parse-display-variable (or display-name (getenv "DISPLAY") ":0.0"))))
  (let* ((display  (xlib:open-display host :display display-number))
         (screen   (xlib:display-default-screen display))
         (black    (xlib:screen-black-pixel screen))
         (white    (xlib:screen-white-pixel screen))
         (window   (xlib:create-window
                    :parent (xlib:screen-root screen)
                    :background white
                    :event-mask (xlib:make-event-mask :button-release
                                                      :key-press
                                                      :key-release
                                                      :button-motion
                                                      :resize-redirect
                                                      :exposure
                                                      :visibility-change
                                                      :structure-notify )
                    :bit-gravity   :south-west
                    :x x :y y :width width :height height))
         (backup   (xlib:create-pixmap
                    :width width :height height
                    :depth (xlib:screen-root-depth screen)
                    :drawable window))
         (font     (xlib:open-font display *system-font-name*))
         (wcontext (xlib:create-gcontext :drawable window
                                         :background white
                                         :foreground black
                                         :font font))
         (bcontext (xlib:create-gcontext :drawable backup
                                         :background white
                                         :foreground black
                                         :font font)))
    (let ((xenv (make-xenv :display  display
                           :window   window
                           :wcontext wcontext
                           :backup   backup
                           :bcontext bcontext)))
      (setf (wm-class xenv) wm-class)
      (xlib:map-window window)
      (xlib:display-force-output display)
      (clear-window xenv)
      (when title (setf (window-title xenv) title))
      xenv)))


(defun close-window (&optional (*xenv* (default-xenv)))
  "Ferme la fenêtre et le serveur spécifiée par la structure *xenv*."
  (when (xenv-display *xenv*)
    (xlib:free-gcontext  (xenv-wcontext *xenv*))
    (xlib:free-gcontext  (xenv-bcontext *xenv*))
    (xlib:destroy-window (xenv-window   *xenv*))
    (xlib:free-pixmap    (xenv-backup   *xenv*))
    (xlib:display-force-output (xenv-display  *xenv*))
    (xlib:close-display        (xenv-display  *xenv*))
    (setf (xenv-bcontext *xenv*) nil
          (xenv-backup   *xenv*) nil
          (xenv-wcontext *xenv*) nil
          (xenv-window   *xenv*) nil
          (xenv-display  *xenv*) nil))
  nil)


(defvar *window-manager-encoding* :iso-8859-1)
(defun encode-string (string &key (start 0) (end (length string)))
  (string-to-octets string
                    :encoding *window-manager-encoding*
                    :start start :end end))
(defun decode-string (octets &key (start 0) (end (length octets)))
  (octets-to-string octets
                    :encoding *window-manager-encoding*
                    :start start :end end))



(defun (setf wm-class) (wm-class &optional (*xenv* (default-xenv)))
  (let ((bytes (concatenate 'vector
                            (encode-string (first  wm-class)) #(0)
                            (encode-string (second wm-class)) #(0))))
    (xlib:change-property (xenv-window *xenv*) :wm_class bytes :string 8))
  wm-class)


(defun wm-class (&optional (*xenv* (default-xenv)))
  (let ((bytes (xlib:get-property (xenv-window *xenv*) :wm_class :type :string
                                                                 :result-type 'vector)))
    (when bytes
      (let ((first-zero (position 0 bytes)))
        (list (decode-string bytes :end first-zero)
              (decode-string bytes
                             :start (1+ first-zero)
                             :end   (position 0 bytes :start (1+ first-zero))))))))


(defun (setf window-title) (title &optional (*xenv* (default-xenv)))
  "Change le titre de la fenêtre."
  (let ((bytes (encode-string title)))
    (xlib:change-property (xenv-window *xenv*) :wm_name      bytes :string 8)
    (xlib:change-property (xenv-window *xenv*) :wm_icon_name bytes :string 8))
  title)

(defun window-title (&optional (*xenv* (default-xenv)))
  "Retourne le titre de la fenêtre."
  (let ((bytes (xlib:get-property (xenv-window *xenv*) :wm_name :type :string
                                                                :result-type 'vector)))
    (when bytes
      (decode-string bytes))))


(defun window-width (&optional (*xenv* (default-xenv)))
  "Retourne la largeur de la fenêtre."
  (xlib:drawable-width (xenv-window *xenv*)))

(defun window-height (&optional (*xenv* (default-xenv)))
  "Retourne la hauteur de la fenêtre."
  (xlib:drawable-height (xenv-window *xenv*)))


(defun process-events (xenv &key timeout)
  "Traite un évènement X pour l'environnement X XENV. 
Retourne lorsqu'un évènement est traité, ou si TIMEOUT est non NIL, 
quand le nombre de secondes indiqué est écoulé."
  (xlib:event-case ((xenv-display xenv) :timeout timeout)
    (:exposure (count) ; Discard all but final :exposure then display the menu
     (when (zerop count) (redraw-window xenv))
     t)
    (otherwise ()                  ; Ignore and discard any other event
     t)))


(defmacro with-window ((&key xenv display-name host display-number
                          (x 0) (y 0) (width 512) (height 342)
                          (title "IPL LISP") (wm-class '("lisp" "LISP")))
                       &body body)
  "Si un display est spécifié, alors ouvre une nouvelle fenêtre, sinon utilise
la structure xenv, ou *xenv*.
Si une nouvelle fenêtre a été ouverte, alors elle est fermée à la fin.
BODY doit appeler PROCESS-EVENTS périodiquement."
  (if (or (null xenv) display-name host display-number)
      ;; nouvelle fenêtre
      `(let ((*xenv* (open-window :display-name ,display-name
                                  :host ,host :display-number ,display-number
                                  :x ,x :y ,y :width ,width :height ,height
                                  :wm-class ',wm-class
                                  :title ,title)))
         (unwind-protect (progn ,@body) (close-window *xenv*)))
      ;; xenv existant
      `(let ((*xenv* (or ,xenv (default-xenv)))) ,@body)))


(defmacro do-window ((&key xenv display-name host display-number
                        (x 0) (y 0) (width 512) (height 342)
                        (title "IPL LISP") (wm-class '("lisp" "LISP"))
                        (timeout nil))
                     &body body)
  "Si un display est spécifié, alors ouvre une nouvelle fenêtre, sinon utilise
la structure xenv, ou *xenv*.
Si une nouvelle fenêtre a été ouverte, alors elle est fermée à la fin.
BODY est exécuté, puis PROCESS-EVENT est appelé, avec le TIMEOUT specifié.
RETURN-FROM-DO-WINDOW doit être appelé pour sortir de la boucle."
  (let ((name (gensym)))
    (if (or (null xenv) display-name host display-number)
        ;; nouvelle fenêtre
        `(let ((*xenv* (open-window :display-name ,display-name
                                    :host ,host :display-number ,display-number
                                    :x ,x :y ,y :width ,width :height ,height
                                    :title ,title :wm-class ',wm-class)))
           (unwind-protect
                (loop :named ,name :do
                  (flet ((return-from-do-window (&optional result)
                           (return-from ,name result)))
                    ,@body)
                  (process-events *xenv* :timeout ,timeout))
             (close-window *xenv*)))
        ;; xenv existant
        `(let ((*xenv* (or ,xenv (default-xenv))))
           (loop :named ,name :do
             (flet ((return-from-do-window (&optional result)
                      (return-from ,name result)))
               ,@body)
             (process-events *xenv* :timeout ,timeout))))))


;;;--------------------
;;; Redraw
;;;--------------------

(defun redraw-window (&optional (*xenv* (default-xenv)))
  "Rafraichi la fenêtre."
  (let ((display  (xenv-display  *xenv*))
        (window   (xenv-window   *xenv*))
        (wcontext (xenv-wcontext *xenv*))
        (backup   (xenv-backup   *xenv*)))
    (xlib:copy-area backup wcontext 0 0
                    (xlib:drawable-width  window)
                    (xlib:drawable-height window)
                    window 0 0)
    (xlib:display-force-output display))
  (values))


(defun clear-window (&optional (*xenv* (default-xenv)))
  "Efface le contenu de la fenêtre."
  (let* ((display  (xenv-display  *xenv*))
         (screen   (xlib:display-default-screen display))
         (white    (xlib:screen-white-pixel screen))
         (backup   (xenv-backup   *xenv*))
         (bcontext (xenv-bcontext *xenv*))
         (oldfore  (xlib:gcontext-foreground bcontext))
         (window   (xenv-window   *xenv*)))
    (unwind-protect
         (progn
           (setf (xlib:gcontext-foreground bcontext) white)
           (xlib:draw-rectangle backup bcontext 0 0
                                (xlib:drawable-width  backup)
                                (xlib:drawable-height backup) t))
      (setf (xlib:gcontext-foreground bcontext) oldfore))
    (xlib:clear-area window)
    (xlib:display-force-output display))
  (values))


;;;--------------------
;;; Colors
;;;--------------------

(defun rgb (r g b)
  "Retourne une couleur spécifiée par ses composantes rouge, verte et bleue."
  (dpb r  (byte 8 16) (dpb g (byte 8 8) (ldb (byte 8 0) b))))

(defparameter *white*      (rgb #xff #xff #xff) "Couleur: blanc")
(defparameter *light-gray* (rgb #xaa #xaa #xaa) "Couleur: gris clair")
(defparameter *dark-gray*  (rgb #x55 #x55 #x55) "Couleur: gris foncé")
(defparameter *black*      (rgb #x00 #x00 #x00) "Couleur: noir")

(defun set-foreground-color (window context color)
  "Change la couleur du pinceau du contexte."
  (declare (ignore window))
  (setf (xlib:gcontext-foreground context) color))

(defun set-rgb-color (r g b &key (xenv (default-xenv)))
  "Change la couleur spécifiée par ses composantes rouge, verte et bleue,
  du pinceau du contexte."
  (set-color (rgb r g b) :xenv xenv))

(defun set-color (color &key (xenv (default-xenv)))
  "Change la couleur du pinceau."
  (setf (xlib:gcontext-foreground (xenv-wcontext xenv)) color)
  (setf (xlib:gcontext-foreground (xenv-bcontext xenv)) color))


;;;--------------------
;;; Drawing
;;;--------------------

(defstruct (point (:type list) (:conc-name nil)) x y)
(defun point (x y) "Retourne un nouveau point 2D." (make-point :x x :y y))


(defun draw-point-at (x y  &key (xenv (default-xenv)))
  "Dessine un point aux coordonnées X,Y indiquées."
  (let* ((height (xlib:drawable-height (xenv-window xenv)))
         (x      (round x))
         (y      (- height (round y))))
    (xlib:draw-point (xenv-backup xenv) (xenv-bcontext xenv) x y)
    (xlib:draw-point (xenv-window xenv) (xenv-wcontext xenv) x y)
    (xlib:display-force-output (xenv-display xenv)))
  (values))


(defun draw-point (point  &key (xenv (default-xenv)))
  "Dessine le POINT."
  (draw-point-at (x point) (y point) :xenv xenv))


(defun draw-points (points  &key (xenv (default-xenv)))
  "Dessine une list de POINTS."
  (let* ((height (xlib:drawable-height (xenv-window xenv)))
         (points (mapcar (lambda (pt) (list (round (x pt)) (- height (round (y pt)))))
                         (cons (car (last points)) points))))
    (xlib:draw-points (xenv-backup xenv) (xenv-bcontext xenv) points)
    (xlib:draw-points (xenv-window xenv) (xenv-wcontext xenv) points)
    (xlib:display-force-output (xenv-display xenv)))
  (values))


(defun draw-line (x1 y1 x2 y2 &key (xenv (default-xenv)))
  "Dessine une ligne entre les points X1,Y1 et X2,Y2."
  (let* ((height (xlib:drawable-height (xenv-window xenv)))
         (x1 (round x1))
         (x2 (round x2))
         (y1 (- height (round y1)))
         (y2 (- height (round y2))))
    (xlib:draw-line (xenv-backup xenv) (xenv-bcontext xenv) x1 y1 x2 y2)
    (xlib:draw-line (xenv-window xenv) (xenv-wcontext xenv) x1 y1 x2 y2)
    (xlib:display-force-output (xenv-display xenv)))
  (values))


(defun draw-polygon (points fillp &key (xenv (default-xenv)))
  "Dessine un polygone défini par ses sommets POINTS. 
Si FILLP est vrai, alors le rempli."
  (let* ((height (xlib:drawable-height (xenv-window xenv)))
         (points (mapcan (lambda (pt) (list (round (x pt)) (- height (round (y pt)))))
                         (cons (car (last points)) points))))
    (xlib:draw-lines (xenv-backup xenv) (xenv-bcontext xenv)
                     points :fill-p fillp)
    (xlib:draw-lines (xenv-window xenv) (xenv-wcontext xenv)
                     points :fill-p fillp)
    (xlib:display-force-output (xenv-display xenv)))
  (values))


(defun draw-rectangle (x y width height fillp &key (xenv (default-xenv)))

  "Dessine un rectangle parallèle aux coordonnées, dont le point en
bas à gauche est X,Y, et de largeur WIDTH et de hauteur HEIGHT.
Si FILLP est vrai, alors le rempli."
  (let* ((wheight (xlib:drawable-height (xenv-window xenv)))
         (width  (round width))
         (height (round height))
         (x      (round x))
         (y      (- wheight (round y) height)))
    (xlib:draw-rectangle (xenv-backup xenv) (xenv-bcontext xenv)
                         x y width height fillp)
    (xlib:draw-rectangle (xenv-window xenv) (xenv-wcontext xenv)
                         x y width height fillp)
    (xlib:display-force-output (xenv-display xenv)))
  (values))


(defun draw-arc (x y width height angle-1 angle-2 fillp
                 &key (xenv (default-xenv)))

  "Dessine un arc parallèle aux coordonnées, dont le point en
bas à gauche est X,Y, et de largeur WIDTH et de hauteur HEIGHT.
Si FILLP est vrai, alors le rempli.
Les angles sont donnés en radian comptés sur l'axe Ox"
  (let* ((wheight (xlib:drawable-height (xenv-window xenv)))
         (width  (round width))
         (height (round height))
         (x      (round x))
         (y      (- wheight (round y) height)))
    (xlib:draw-arc (xenv-backup xenv) (xenv-bcontext xenv)
                   x y width height angle-1 angle-2 fillp)
    (xlib:draw-arc (xenv-window xenv) (xenv-bcontext xenv)
                   x y width height angle-1 angle-2 fillp)
    (xlib:display-force-output (xenv-display xenv)))
  (values))


(defun draw-text (x y string &key (fillp t) (xenv (default-xenv)))
  "Dessine le texte STRING aux coordonnées X,Y."
  (let* ((height (xlib:drawable-height (xenv-window xenv)))
         (x      (round x))
         (y      (- height (round y))))
    (if fillp
        (progn
          (xlib:draw-image-glyphs (xenv-backup xenv) (xenv-bcontext xenv)
                                  x y string)
          (xlib:draw-image-glyphs (xenv-window xenv) (xenv-wcontext xenv)
                                  x y string))
        (progn
          (xlib:draw-glyphs (xenv-backup xenv) (xenv-bcontext xenv) x y string)
          (xlib:draw-glyphs (xenv-window xenv) (xenv-wcontext xenv) x y string)))
    (xlib:display-force-output (xenv-display xenv)))
  (values))

;;;-------------------------------------------
;;; Une boucle REPL traitant les évènements X
;;;-------------------------------------------



;;;--------------------
;;; Tests
;;;--------------------

(defun test-1 ()
  (let ((time (get-universal-time))) 
    (do-window (:timeout 1)
      (when (<= 1 (- (get-universal-time) time))
        (setf time (get-universal-time))
        (set-rgb-color (random 256)  (random 256)  (random 256))
        (draw-rectangle (random 500) (random 300)
                        (random 500) (random 300) t)))))


(defun test-properties ()
  (with-window ()
    (print (window-title))
    (sleep 1)
    (print (setf (window-title) "¿Donde esta la casa de mi amigo?"))
    (sleep 1)
    (print (window-title))
    (sleep 10)))



#-(and) (progn
          
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


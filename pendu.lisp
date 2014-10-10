;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pendu.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Jeu du pendu.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-12-25 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2007 - 2007
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

(defvar *fichier-de-mots* "/usr/share/dict/words"
  "Chemin vers un fichier qui contient des mots, un par ligne.")

(defparameter *mots*
  (with-open-file (ficmot *fichier-de-mots*)
    (loop
       :for ligne = (read-line ficmot nil :fini)
       :until (eql ligne :fini)
       :collect (string-trim " " ligne))))

;; (do ((ligne (read-line ficmot nil :fini) (read-line ficmot nil :fini))
;;      (mots  '() (cons (string-trim " " ligne) mots)))
;;     ((eql ligne :fini) mots))

;; (length (with-open-file (ficmot *fichier-de-mots*)
;;           (let ((mots '())
;;                 (ligne))
;;             (loop
;;                (setf ligne (read-line ficmot nil :fini))
;;                (when (eql ligne :fini)
;;                  (return mots))
;;                (push (string-trim " " ligne) mots)))))



;; (length *mots*)
;; (first *mots*)
;; (elt *mots* 10000)
;; "anticrochet"

(defun mot-au-hasard ()
  (elt *mots* (random (length *mots*))))

;; (list (MOT-AU-HASARD) (MOT-AU-HASARD) (MOT-AU-HASARD))


(defun premiere-et-derniere-lettres (mot)
  (let ((lettres-connues (make-string (length mot) :initial-element #\.)))
    (setf (aref lettres-connues 0) (aref mot 0)
          (aref lettres-connues (1- (length lettres-connues)))
          (aref mot (1- (length lettres-connues))))
    lettres-connues))

;; (PREMIERE-ET-DERNIERE-LETTRES (elt *mots* 10000))
;; "a.........t"


(defun toutes-trouvees-p (mot)
  (not (find #\. mot)))

;; (list (toutes-trouvees-p "p...u")
;;       (toutes-trouvees-p "pen.u")
;;       (toutes-trouvees-p "pendu"))
;; (NIL NIL T)


(defparameter *etats-de-pendu*
  '(initial sol poteau potence corde tete corps bras-gauche bras-droit
    jambe-gauche jambe-droite))

(defun pendup (etat) (eql etat 'jambe-droite))

;; (loop :for etat :in *etats-de-pendu* :collect (pendup etat))
;; (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T)

(defun etat-suivant (etat)
  (second (member etat *etats-de-pendu*)))

;;  (loop :for etat :in *etats-de-pendu*
;;     :collect (list etat '->  (etat-suivant etat)))
;; ((INITIAL -> SOL) (SOL -> POTEAU) (POTEAU -> POTENCE)
;;  (POTENCE -> CORDE) (CORDE -> TETE) (TETE -> CORPS)
;;  (CORPS -> BRAS-GAUCHE) (BRAS-GAUCHE -> BRAS-DROIT)
;;  (BRAS-DROIT -> JAMBE-GAUCHE)
;;  (JAMBE-GAUCHE -> JAMBE-DROITE) (JAMBE-DROITE -> NIL))
(defun lettre-devinee-p (lettre trouvees mot)
  (loop
     :with devinee = nil
     :with nouvelles-trouvees = (make-string (length trouvees))
     :for i :from 0 :below (length mot)
     :do (if (and (char-equal #\.    (aref trouvees i))
                  (char-equal lettre (aref mot      i)))
           (setf devinee t
                 (aref nouvelles-trouvees i) (aref mot i))
           (setf (aref nouvelles-trouvees i) (aref trouvees i)))
     :finally (if devinee
                (return nouvelles-trouvees)
                (return nil))))


(defun pendu ()
  (let* ((mot              (mot-au-hasard))
         (lettres-trouvees (premiere-et-derniere-lettres mot))
         (lettres-essayees '())
         (etat-joueur      'initial))
    (loop
       :until (or (toutes-trouvees-p lettres-trouvees)
                  (pendup etat-joueur))
       :do
       (format t "~A~%" etat-joueur)
       (format t "~A~%" lettres-trouvees)
       (format t "Votre choix: ")
       (let ((ligne (string-trim " " (read-line))))
         (if (= 1 (length ligne))
           (let ((lettre (aref ligne 0)))
             (if (member lettre lettres-essayees)
               (progn
                 (format t "Vous avez déjà essayé cette lettre!~%")
                 (setf etat-joueur (etat-suivant etat-joueur)))
               (let ((devinees (lettre-devinee-p lettre lettres-trouvees
                                                 mot)))
                 (if devinees
                   (setf lettres-trouvees devinees)
                   (progn
                     (format t "Cette lettre n'existe pas dans le mot!~%")
                     (setf etat-joueur (etat-suivant etat-joueur)))))))
           (format t "Veuillez ne saisir qu'une seule lettre!~%")))
       :finally (if (toutes-trouvees-p lettres-trouvees)
                  (format t "Vous avez gagné!~%")
                  (format t "Vous avez perdu!~%Le mot était: ~A" mot)))))


(defparameter *dessins*
  '((initial . " 
               
             
              
             
             
             
              
               
              
                    
")
    (sol . "
               
              
             
             
             
             
             
             
               
------------------
")
    (poteau . "
            +  
            |
            |
            |
            |
            |
            |
            |
           /|\\
------------------
")
    (potence . "
   ---------+  
           \\|
            |
            |
            |
            |
            |
            |
           /|\\
------------------
")
    (corde . "
   --+------+  
     |     \\|
            |
            |
            |
            |
            |
            |
           /|\\
------------------
")
    (tete . "
   --+------+  
     |     \\|
     O      |
            |
            |
            |
            |
            |
           /|\\
------------------
")
    (corps . "
   --+------+  
     |     \\|
     O      |
     |      |
     |      |
     |      |
            |
            |
           /|\\
------------------
")
    (bras-gauche . "
   --+------+  
     |     \\|
     O      |
     |      |
    /|      |
   / |      |
            |
            |
           /|\\
------------------
")
    (bras-droit  . "
   --+------+  
     |     \\|
     O      |
     |      |
    /|\\     |
   / | \\    |
            |
            |
           /|\\
------------------
")
    (jambe-gauche . "
   --+------+  
     |     \\|
     O      |
     |      |
    /|\\     |
   / | \\    |
      \\     |
       \\    |
           /|\\
------------------
")
    (jambe-droite . "
   --+------+  
     |     \\|
     O      |
     |      |
    /|\\     |
   / | \\    |
    / \\     |
   /   \\    |
           /|\\
------------------
")))


(defun dessin-de-l-etat (etat)
   (cdr (assoc etat *dessins*)))


(defun pendu ()
  (let* ((mot              (mot-au-hasard))
         (lettres-trouvees (premiere-et-derniere-lettres mot))
         (lettres-essayees '())
         (etat-joueur      'initial))
    (loop
       :until (or (toutes-trouvees-p lettres-trouvees)
                  (pendup etat-joueur))
       :do
       (format t "~A~%" (dessin-de-l-etat etat-joueur))
       (format t "~A~%" lettres-trouvees)
       (format t "Votre choix: ")
       (let ((ligne (string-trim " " (read-line))))
         (if (= 1 (length ligne))
           (let ((lettre (aref ligne 0)))
             (if (member lettre lettres-essayees)
               (format t "Vous avez déjà essayé cette lettre!~%")
               (let ((devinees (lettre-devinee-p lettre lettres-trouvees
                                                 mot)))
                 (push lettre lettres-essayees)
                 (if devinees
                   (setf lettres-trouvees devinees)
                   (progn
                     (format t "Cette lettre n'existe pas dans le mot!~%")
                     (setf etat-joueur (etat-suivant etat-joueur)))))))
           (format t "Veuillez ne saisir qu'une seule lettre!~%")))
       :finally (if (toutes-trouvees-p lettres-trouvees)
                  (format t "Vous avez gagné!~%Le mot était bien: ~A"
                          mot)
                  (format t "~A~%Vous avez perdu!~%Le mot était: ~A"
                          (dessin-de-l-etat etat-joueur) mot)))))



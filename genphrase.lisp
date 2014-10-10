;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               genphrase.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-10-18 <PJB> Created.
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
;;;;    


(defparameter *français*
  '((phrase         =>
     (sujet/s verbe/s complement)
     (sujet/p verbe/p complement))
    (sujet/s        => (groupe-nominal/s) (nom-propre))
    (sujet/p        => (groupe-nominal/p))
    (verbe/s        => tape regarde voit aime mange gratte)
    (verbe/p        => tapent regardent voient aiment mangent grattent)
    (complement     => (groupe-nominal/s) (groupe-nominal/p))
    (groupe-nominal/s =>
     (article/f adjectif/f* nom/f post-adj/f*)
     (article/m adjectif/m* nom/m post-adj/m*))
    (groupe-nominal/p =>
     (article/mp adjectif/mp* nom/mp post-adj/mp*)
     (article/fp adjectif/fp* nom/fp post-adj/fp*))
    (article/mp     => les des)
    (article/fp     => les des)
    (article/f      => la une)
    (article/m      => le un)
    (adjectif/f*    => () (adjectif/f adjectif/f*))
    (adjectif/m*    => () (adjectif/m adjectif/m*))
    (adjectif/fp*   => () (adjectif/fp adjectif/fp*))
    (adjectif/mp*   => () (adjectif/mp adjectif/mp*))
    (adjectif/f     => belle  grande  méchante  gentille)
    (adjectif/m     => beau   grand   méchant   gentil)
    (adjectif/fp    => belles grandes méchantes gentilles)
    (adjectif/mp    => beaux  grands  méchants  gentils)
    (post-adj/f*    => () (post-adj/f  post-adj/f*))
    (post-adj/m*    => () (post-adj/m  post-adj/m*))
    (post-adj/fp*   => () (post-adj/fp post-adj/fp*))
    (post-adj/mp*   => () (post-adj/mp post-adj/mp*))
    (post-adj/f     => rouge bleue verte)
    (post-adj/m     => rouge bleu vert)
    (post-adj/fp    => rouges bleues vertes)
    (post-adj/mp    => rouges bleus verts)
    (nom/m          =>
     lapin chat chien cheval pain homme marteau
     rat gruyère)
    (nom/mp         =>
     lapins chats chiens chevaux pains hommes marteaux
     rats gruyères)
    (nom/f          =>
     lapine chatte chienne jument pomme orange balle femme table
     souris raclette)
    (nom/fp         =>
     lapines chattes chiennes juments pommes oranges balles femmes tables
     souris raclettes)
    (nom-propre     => Jean Jeanne Claude Dominique))
  "Définition d'une grammaire française simplifiée.")


(defparameter *grammaire* *français*)

(defun production-antécédent (production)
  "Donne l'antécédent d'une production."
  (first  production))

(defun production-conséquent (production)
  "Donne le conséquent d'une production."
  (rest (rest production)))

(defun produit (catégorie)
  "Donne une liste des conséquents possibles de la catégorie
dans la grammaire."
  (production-conséquent (assoc catégorie *grammaire*)))



(defun élément-aléatoire (liste)
  (elt liste (random (length liste))))

(defun mappend (fonction liste)
  "Applique la fonction à chaque élément de la liste et concatène
les listes résultantes"
  (apply (function append) (mapcar fonction liste)))

(defun génère (phrase)
  (cond ((listp phrase)
         (mappend (function génère) phrase))
        ((produit phrase)
         (génère (élément-aléatoire (produit phrase))))
        (t
         (list phrase))))

(defun 10-phrases ()
  (loop repeat 10 do (print (génère 'phrase))))

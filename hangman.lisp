;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               hangman.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Hangman, implemented with MVC and tests.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-11-17 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.HANGMAN"
  (:use "COMMON-LISP")
  (:export "HANGMAN")
  (:documentation "

The hangman game, using the standard I/O for the user interface.

Copyright Pascal J. Bourguignon 2013 - 2013

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with this program.  If not, see <http://www.gnu.org/licenses/>.

"))
(in-package  "COM.INFORMATIMAGO.HANGMAN")

(defun first-element  (sequence) (elt sequence 0))
(defun last-element   (sequence) (elt sequence (1- (length sequence))))
(defun random-element (sequence) (elt sequence (random (length sequence))))

(defgeneric game-over-p (game))
(defgeneric game-win-p (game))
(defgeneric view-display (view))

;;;; Hangman model

;;; letter -- represent a letter that can be guessed or not guessed yet.

(defstruct (letter
             (:constructor %make-letter))
  character
  guessedp)

(defun make-letter (alpha)
  (check-type alpha character)
  (assert (alpha-char-p alpha))
  (%make-letter :character alpha :guessedp nil))


(defun test/letter ()
  (let ((letter (make-letter #\a)))
    (assert (not (letter-guessedp letter)))
    (assert (char= #\a (letter-character letter))))
  :success)



;;; word -- a sequence of letters.  The word is guessed when all its
;;; letters are guessed.  Letters are tried case-insensitively.

(defstruct (word
             (:constructor %make-word))
  string
  letters)

(defun make-word (word)
  (check-type word string)
  (assert (every (function alpha-char-p) word))
  (%make-word :string word
              :letters (map 'vector (function make-letter) word)))

(defun word-guessedp (word)
  (check-type word word)
  (every (function letter-guessedp) (word-letters word)))

(defun word-try-letter (word alpha)
  "Return the number of revealed letters."
  (check-type word word)
  (check-type alpha character)
  (assert (alpha-char-p alpha))
  (count-if (lambda (letter)
              (when (and (not (letter-guessedp letter))
                         (char-equal alpha (letter-character letter)))
                (setf (letter-guessedp letter) t)))
            (word-letters word)))


(defun test/word ()
  (let ((word (make-word "Hello")))
    (assert (string= "Hello" (word-string word)))
    (assert (loop
              :for ch :across "Hello"
              :for letter :across (word-letters word)
              :always (and (char= ch (letter-character letter))
                           (not (letter-guessedp letter)))))
    (assert (not (word-guessedp word)))
    (assert (= 1 (word-try-letter word #\h)))
    (assert (= 0 (word-try-letter word #\h)))
    (assert (= 1 (word-try-letter word #\E)))
    (assert (= 0 (word-try-letter word #\E)))
    (assert (= 2 (word-try-letter word #\L)))
    (assert (= 0 (word-try-letter word #\L)))
    (assert (not (word-guessedp word)))
    (assert (= 0 (word-try-letter word #\z)))
    (assert (not (word-guessedp word)))
    (assert (= 1 (word-try-letter word #\o)))
    (assert (= 0 (word-try-letter word #\o)))
    (assert (word-guessedp word)))
  :success)



;;; hangman -- counts the number of errors, up to a maximum.

(defstruct hangman
  (error-count 0)
  maximum-error-count)

(defun hangman-made-error (hangman)
  (when (< (hangman-error-count hangman) (hangman-maximum-error-count hangman))
    (incf (hangman-error-count hangman))))

(defun hangman-deadp (hangman)
  (check-type hangman hangman)
  (>= (hangman-error-count hangman)
      (hangman-maximum-error-count hangman)))

(defun hangman-state (hangman)
  (check-type hangman hangman)
  (min (hangman-error-count hangman)
       (hangman-maximum-error-count hangman)))

(defun test/hangman ()
  (let ((hangman (make-hangman :maximum-error-count 3)))
    (loop
      :for state :from 0
      :while (< state 3)
      :do (progn
            (assert (= state (hangman-state hangman)))
            (assert (not (hangman-deadp hangman)))
            (hangman-made-error hangman))
      :finally (assert (hangman-deadp hangman))))
  :success)


;;; hangman-game -- wraps up the components of a hangman game, and
;;; tracks the number letters revealed by the last try.

(defstruct hangman-game
  hangman
  word
  guessed-count)


(defun hangman-game-try-letter (game letter)
  (when (zerop (setf (hangman-game-guessed-count game) (word-try-letter (hangman-game-word game) letter)))
    (incf (hangman-error-count (hangman-game-hangman game)))))


(defmethod game-over-p ((game hangman-game))
  (or (hangman-deadp (hangman-game-hangman game))
      (word-guessedp (hangman-game-word game))))

(defmethod game-win-p ((game hangman-game))
  (word-guessedp (hangman-game-word game)))


(defun test/hangman-game ()
  (flet ((play-game (max-errors word tries win)
           (let ((game (make-hangman-game :hangman (make-hangman :maximum-error-count max-errors)
                                          :word (make-word word))))
             (loop
               :for (letter reveals errors) :in tries
               :do (progn
                     (assert (not (game-over-p game)))
                     (assert (not (game-win-p game)))
                     (hangman-game-try-letter game letter)
                     (assert (= reveals (hangman-game-guessed-count game)) () "reveals=~A letter=~A" reveals letter)
                     (assert (= errors (hangman-error-count (hangman-game-hangman game)))))
               :finally
               (assert (game-over-p game))
               (assert (funcall win (hangman-game-win-p game)))))))
    (play-game 3  "Hello" '((#\h 1 0)
                            (#\E 1 0)
                            (#\l 2 0)
                            (#\l 0 1)
                            (#\z 0 2)
                            (#\o 1 2))
               (function identity))
    (play-game 3  "Hello" '((#\h 1 0)
                            (#\E 1 0)
                            (#\a 0 1)
                            (#\l 2 1)
                            (#\l 0 2)
                            (#\b 0 3))
               (function not)))
  :success)


(defun test/model ()
  (test/letter)
  (test/word)
  (test/hangman)
  (test/hangman-game))


;;;; Views

;;; word-view -- displays the letters of the word, hidding the letter
;;; not guessed yet.

(defstruct word-view
  word)

(defmethod view-display ((view word-view))
  (loop
    :for letter :across (word-letters (word-view-word view))
    :initially (format *query-io* "~&Word: ")
    :do (if (letter-guessedp letter)
            (format *query-io* " ~C" (letter-character letter))
            (format *query-io* " _"))
    :finally (format *query-io*  " ~%")))


(defun test/word-view ()
  (assert (string= (with-output-to-string (*query-io*)
                     (let* ((word (make-word "Hello"))
                            (view (make-word-view :word word)))
                       (loop
                         :for ch :across "helo"
                         :initially (view-display view)
                         :do (word-try-letter word ch) (view-display view))))
                   "Word:  _ _ _ _ _ 
Word:  H _ _ _ _ 
Word:  H e _ _ _ 
Word:  H e l l _ 
Word:  H e l l o 
"))
  :success)


;;; hangman-view

(defparameter *drawings*
  '((initial . " 
               
             
              
             
             
             
              
               
              
                    
")
    (ground . "
               
              
             
             
             
             
             
             
               
------------------
")
    (post . "
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
    (bracket . "
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
    (rope . "
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
    (head . "
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
    (body . "
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
    (left-arm . "
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
    (right-arm  . "
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
    (left-leg . "
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
    (right-leg . "
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


(defstruct hangman-view
  hangman)

(defmethod view-display ((view hangman-view))
  (format *query-io* "~%~A~%"
          (cdr
           (elt *drawings* (hangman-state (hangman-view-hangman view))))))

(defun test/hangman-view ()
  (loop
    :with hangman = (make-hangman :maximum-error-count (1- (length *drawings*)))
    :with view = (make-hangman-view :hangman hangman)
    :for (nil . drawing) :in *drawings*
    :do (progn
          (assert (search drawing (with-output-to-string (*query-io*)
                                    (view-display view))))
          (hangman-made-error hangman)))
  :success)



(defstruct (hangman-game-view
             (:constructor %make-hangman-game-view))
  game
  hangman-subview
  word-subview)

(defun make-hangman-game-view (game)
  (check-type game hangman-game)
  (%make-hangman-game-view :game game
                           :hangman-subview (make-hangman-view :hangman (hangman-game-hangman game))
                           :word-subview    (make-word-view    :word    (hangman-game-word game))))

(defmethod view-display ((view hangman-game-view))
  (view-display (hangman-game-view-hangman-subview view))
  (view-display (hangman-game-view-word-subview view))
  (when (game-over-p (hangman-game-view-game view))
    (format *query-io* "~%Game Over, Man.~%You ~:[lose~;win~]!~2%"
            (game-win-p (hangman-game-view-game view)))))


(defun test/hangman-game-view ()
  ;; left as exercise for the reader
  :success)


(defun test/views ()
  (test/word-view)
  (test/hangman-view)
  (test/hangman-game-view))


;;;; Controllers


(defstruct controller
  game
  game-view)


(defun hide-previous-input (&optional (stream *query-io*))
  (format stream "~200%")
  (force-output stream))

(defvar *whitespaces*  #(#\space #\tab))


(defun ask-a-word ()
  (loop
    :named ask
    :do
    (format *query-io* "~2%Enter a new word (only alphabetic characters): ")
    (finish-output *query-io*)
    (let ((word (string-trim *whitespaces* (read-line *query-io*))))
      (when (every (function alpha-char-p) word)
        (return-from ask word)))
    :finally (hide-previous-input *query-io*)))

(defun test/ask-a-word ()
  ;; exercise for the reader (cf test/get-next-command and test/hangman-view)
  :success)


(defun get-next-word (controller)
  (declare (ignore controller))
  (make-word #-(and)
             (random-element
              (load-time-value
               (remove-if-not
                (lambda (word) (and (<= 3 (length word)) (every (function alpha-char-p) word)))
                (com.informatimago.common-lisp.cesarum.file:string-list-text-file-contents
                 "/usr/share/dict/words"))))
             (ask-a-word)))


(defun get-next-command (controller)
  (declare (ignore controller))
  (format *query-io* "Enter a letter, or QUIT: ")
  (finish-output *query-io*)
  (let ((input (string-trim *whitespaces* (read-line *query-io*))))
    (if (= 1 (length input))
        (if (alpha-char-p (aref input 0))
            (list :letter (aref input 0))
            (progn
              (format *query-io* "Please, type a letter, not '~A'. Try again.~%" input)
              '(:redisplay)))
        (if (string-equal "quit" input)
            '(:quit)
            (progn
              (format *query-io* "Please, type a letter or QUIT, not '~A'. Try again.~%" input)
              '(:redisplay))))))


(defun test/get-next-command ()
  (loop :for (input expected) :in '(("a"         (:letter #\a))
                                    (" A "       (:letter #\A))
                                    ("9"         (:redisplay))
                                    ("ab"        (:redisplay))
                                    ("quit"      (:quit))
                                    ("  QUIT  "  (:quit)))
    :do (let ((result))
          (with-input-from-string (input (format nil "~A~%" input))
            (with-output-to-string (output)
              (let ((*query-io* (make-two-way-stream input output)))
                (setf result (get-next-command (make-controller))))))
          (assert (equal result expected)
                  () "input=~S expected=~S obtained=~S" input expected result))))



(defun controller-initialize-game (controller)
  (let ((word (get-next-word controller)))
    (let ((letters (word-letters word)))
      (setf (letter-guessedp (first-element letters)) t
            (letter-guessedp (last-element  letters)) t))
    (setf (controller-game controller) (make-hangman-game :word word
                                                          :hangman (make-hangman :maximum-error-count (1- (length *drawings*)))))
    (setf (controller-game-view controller) (make-hangman-game-view (controller-game controller)))))


(defun controller-play-game (controller)
  (controller-initialize-game controller)
  (loop
    :do (view-display (controller-game-view controller))
    :until (game-over-p (controller-game controller))
    :do (destructuring-bind (command &optional alpha) (get-next-command controller)
          (ecase command
            ((:quit)      (return-from controller-play-game))
            ((:redisplay) #|nop|#)
            ((:letter)    (hangman-game-try-letter (controller-game controller) alpha))))))



(defun test/controller ()
 (test/ask-a-word)
 (test/get-next-command)
 ;; add tests of the whole game
 :success)

(defun test ()
  (test/model)
  (test/views)
  (test/controller))


(defun hangman ()
  (test)
  (controller-play-game (make-controller))
  (values))

;;;; THE END ;;;; 

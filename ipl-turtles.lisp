;;;; -*- coding: utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ipl-turtles.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implement turtles on IPL-CLX.
;;;;
;;;;    The current implementation lacks split: it wors with only one turtle.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-11-04 <PJB> Created.
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
(defpackage "IPL-TURTLES"
  (:use "IPL" "IPL-CLX")
  (:export "TURTLES" "MOVE-OFFSET" "DRAW-OFFSET" "ERASE-OFFSET"
           "MOVE" "DRAW" "ERASE" "TURN" "TURN/RADIANS"
           "CLEAR"  "SPLIT" "SPLIT*" "TPROMPT"
           "REPEAT"))
(in-package "IPL-TURTLES")


(defstruct (turtle (:copier copy-turtle))
  (angle 0) ; angle in radians from Ox
  (position (point 0 0)))

(defun copy-turtle (old)
  (make-turtle :angle (turtle-angle old)
               :position (point (x (turtle-position old))
                                (y (turtle-position old)))))


(defvar *turtles* nil)

(defun turtles (&optional (on t onp))
  "shows and hides the turtles window based on the boolean ON. The
parameter ON is optional; if it is left out, it toggles the state of
the turtles window.
"
  (if onp
      (if on
          (clear)
          (progn (close-window) (setf *turtles* nil)))
      (turtles (not *turtles*))))

(defun ensure-turtles ()
  (unless *turtles* (turtles t)))


;; (defun get-turtles () (mapcar (function copy-turtle) *turtles*))
;; (defun set-turtles (turtles) (setf *turtles* turtles))

(defun move-offset (h v)
  "Moves the turtle in diagonal, h pixels horizontally and v pixels vertically."
  (ensure-turtles)
  (dolist (turtle *turtles* (values))
    (incf (x (turtle-position turtle)) h)
    (incf (y (turtle-position turtle)) v)))
        

(defun draw-offset (h v)
   "Moves the turtle in diagonal, h pixels horizontally and v pixels vertically,
and draws a line on that path."
  (ensure-turtles)
  (dolist (turtle *turtles* (values))
    (draw-line (x (turtle-position turtle))
               (y (turtle-position turtle))
               (incf (x (turtle-position turtle)) h)
               (incf (y (turtle-position turtle)) v))))


(defun erase-offset (h v)
   "Moves the turtle in diagonal, h pixels horizontally and v pixels vertically,
and erases along that path."
  (ensure-turtles)
  (set-color *white*)
  (draw-offset h v)
  (set-color *black*)
  (values))


(defun move (n)
  "Moves the turtle n pixels."
  (ensure-turtles)
  (dolist (turtle *turtles* (values))
    (incf (x (turtle-position turtle)) (* (sin (turtle-angle turtle)) n))
    (incf (y (turtle-position turtle)) (* (cos (turtle-angle turtle)) n))))


(defun draw (n)
  "Moves the turtle n pixels and draws a line on that path."
  (ensure-turtles)
  (dolist (turtle *turtles* (values))
    (draw-line (x (turtle-position turtle))
               (y (turtle-position turtle))
               (incf (x (turtle-position turtle))
                     (* (sin (turtle-angle turtle)) n))
               (incf (y (turtle-position turtle))
                     (* (cos (turtle-angle turtle)) n)))))

(defun erase (n)
  "Moves the turtle n pixels and erases along that path."
  (ensure-turtles)
  (set-color *white*)
  (draw n)
  (set-color *black*)
  (values))


(defun radian<-degree (deg) (* (/ deg 180) pi))

(defun turn (theta)
  "Turns the turtle THETA degrees counter-clockwise."
  (turn/radians (radian<-degree theta)))

(defun turn/radians (theta)
  "Turns the turtle THETA radians counter-clockwise."
  (ensure-turtles)
  (dolist (turtle *turtles* (values))
    (setf (turtle-angle turtle) (mod (- (turtle-angle turtle) theta) (* 2 pi)))))


(defun clear ()
  "Erases the turtles window."
  (clear-window)
  (setf *turtles* (list (make-turtle :position (point (/ (window-width)  2)
                                                      (/ (window-height) 2))))))


(defmacro with-turtle (turtle &body body)
  ;; NOTE: this will prevent split inside splits.
  `(let ((*turtles* (list ,turtle)))
     ,@body))


(defun split/f (fun)
  (let ((new-turtles (mapcar (function copy-turtle) *turtles*)))
    (dolist (turtle new-turtles)
      (with-turtle turtle (funcall fun)))
    (setf *turtles* (nconc new-turtles *turtles*))
    (values)))

(defmacro split (e) `(split/f (lambda () ,e)))


(defun split*/f (&rest funs)
  (let* ((count (length funs))
         (new-turtles (mapcar (lambda (old)
                                (loop :repeat count
                                   :collect (copy-turtle old))) *turtles*)))
    (loop
       :for turtles :in new-turtles
       :do (loop
              :for turtle :in turtles
              :for fun :in funs
              :do (with-turtle turtle (funcall fun))))
    (setf *turtles* (apply (function concatenate) 'list *turtles* new-turtles))
    (values)))

(defmacro split* (&rest expressions)
   `(split*/f ,@(mapcar (lambda (e) `(lambda () ,e)) expressions)))

        
(defmacro tprompt (&body body)
  `(let ((*turtles* (mapcar (function copy-turtle) *turtles*)))
     ,@body))


(defmacro repeat (count &body body)
  `(loop :repeat ,count :do ,@body))


;;;; THE END ;;;;


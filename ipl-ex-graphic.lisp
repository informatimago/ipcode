;;;; -*- coding: utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ipl-ex-graphic.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Quelques exemples graphiques.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-11-01 <PJB> Created.
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
(in-package "IPL-USER")
(print '(in-package "IPL-USER"))

;;;--------------------
;;; Example 1
;;;--------------------

(defun pt-on-circle (radius center angle)
  (point (+ (* radius (sin angle)) (x center))
         (+ (* radius (cos angle)) (y center))))


(defun draw-step (poly)
  (set-rgb-color 255 255 255)
  (draw-rectangle 0     0  240  40 t)
  (draw-rectangle 0    40   40 200 t)
  (draw-rectangle 40  200  200  40 t)
  (draw-rectangle 200  40   40 160 t)
  (set-rgb-color 192 192 192)
  (draw-rectangle  40 40 160 160 t)
  (set-rgb-color 0 0 0)
  (draw-polygon poly nil))


(defun rotating-square ()
  (do-window (:title "Examples -- Rotating Square" :timeout 0.02)
    (loop
       :with pi/2 = (/ pi 2)
       :with 3pi/2 = (/ pi 2/3)
       :with pi/180 = (/ pi 180)
       :with radius = 100
       :with center = (point 120 120)
       :for angle :from 0 to 89
       :for radians = (* pi/180 angle)
       :do (draw-step
           (list (pt-on-circle radius center radians)
                 (pt-on-circle radius center (+ pi/2 radians))
                 (pt-on-circle radius center (+ pi radians))
                 (pt-on-circle radius center (+ 3pi/2 radians))))
         (sleep 0.01))))

(print '(rotating-square))


;;;--------------------
;;; Example 2
;;;--------------------

(defun query-continue (message)
  (clear-input *query-io*)
  (format *query-io* "~A" message)
  (read-line  *query-io* nil nil))


(defun browning ()
  (let ((i 100000)
        (c (random #x1000000))
        (ox 250)
        (oy 250)
        (x (random 500))
        (y (random 500)))
    (do-window (:title "Examples -- Browning" :timeout 0.01)
      (set-color c)
      (when (< (+ (abs (- ox x)) (abs (- oy y))) 400)
        (draw-line ox oy x y))
      (setf i (1- i)
            c (mod (+ c (* (aref #(#x10000 #x100 #x1) (random 3))
                           (random 10))) #x1000000)
            ox x
            oy y
            x (mod (+ x (- (random 21) 10)) 500)
            y (mod (+ y (- (random 21) 10)) 500))
      (when (zerop i)
        (query-continue  "Terminé; tapez RET: ")
        (return-from-do-window)))))

(print '(browning))


;;;--------------------
;;; 
;;;--------------------


(defun draw-function (fun min-x max-x min-y max-y)
  (let ((width  (window-width))
        (height (window-height)))
    (flet ((wx (x) (* width  (/ (- x min-x) (- max-x min-x))))
           (wy (y) (* height (/ (- y min-y) (- max-y min-y)))))
      (set-rgb-color 255 255 255)
      (draw-rectangle 0 0 width height t)
      (set-rgb-color 0 0 50)
      (draw-line 0 (wy 0) width (wy 0))
      (draw-line (wx 0) 0 (wx 0) height)
      (set-rgb-color 0 0 0)
      (loop
         :for x :from min-x :to max-x :by (/ (- max-x min-x) width)
         :do (let ((y (ignore-errors (funcall fun x))))
               (when y (draw-point-at (wx x) (wy y)))))))) 
       

(defun test-draw-function ()
  (do-window (:title "Courbes")
    (draw-function (function sin) (* -4 pi) (* 4 pi) -1.2 +1.2)
    (query-continue "Terminé; tapez RET : ")
    (return-from-do-window)))

(print '(test-draw-function))


;;;--------------------
;;; Turtles Examples
;;;--------------------


(defun ex-turtle-1 ()
  (loop
     :initially (clear)
     :for i :below 6
     :do (draw (/ 50 (expt (1+ i) 3/4))) (turn 45) (split (turn -70))))

(print '(ex-turtle-1))


(defun ex-turtle-2 ()
  (loop
     :initially (clear)
     :for i :below 5
     :do (progn (draw (/ 50 (expt (1+ i) (/ pi 7))))
                (turn 53)
                (split* (turn -107) (turn -88) (turn -65) (turn -30)))))

(print '(ex-turtle-2))


(defun ex-turtle-3 ()
  (loop
     :initially (clear)
     :for i :below 50
     :do (progn (tprompt (draw (+ (* i 3) 20))
                         (split* (turn -107) (turn -46) (turn 65) (turn 30))
                         (draw 25))
                (turn 10))))

(print '(ex-turtle-3))

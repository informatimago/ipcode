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

(IN-PACKAGE "IPL-USER")

(PRINT '(IN-PACKAGE "IPL-USER"))

;;;--------------------
;;; Example 1
;;;--------------------

(DEFUN PT-ON-CIRCLE (RADIUS CENTER ANGLE)
  (POINT (+ (* RADIUS (SIN ANGLE)) (X CENTER))
         (+ (* RADIUS (COS ANGLE)) (Y CENTER))))


(DEFUN DRAW-STEP (POLY)
  (SET-RGB-COLOR 255 255 255)
  (DRAW-RECTANGLE 0     0  240  40 T)
  (DRAW-RECTANGLE 0    40   40 200 T)
  (DRAW-RECTANGLE 40  200  200  40 T)
  (DRAW-RECTANGLE 200  40   40 160 T)
  (SET-RGB-COLOR 192 192 192)
  (DRAW-RECTANGLE  40 40 160 160 T)
  (SET-RGB-COLOR 0 0 0)
  (DRAW-POLYGON POLY nil))


(DEFUN ROTATING-SQUARE ()
  (DO-WINDOW (:TITLE "Examples -- Rotating Square" :TIMEOUT 0.02)
    (LOOP
       :WITH PI/2 = (/ PI 2)
       :WITH 2PI = (* 2 PI)
       :WITH 3PI/2 = (/ PI 2/3)
       :WITH PI/180 = (/ PI 180)
       :WITH RADIUS = 100
       :WITH CENTER = (POINT 120 120)
       :FOR ANGLE :FROM 0 TO 89
       :FOR RADIANS = (* PI/180 ANGLE)
       :DO (DRAW-STEP
           (LIST (PT-ON-CIRCLE RADIUS CENTER RADIANS)
                 (PT-ON-CIRCLE RADIUS CENTER (+ PI/2 RADIANS))
                 (PT-ON-CIRCLE RADIUS CENTER (+ PI RADIANS))
                 (PT-ON-CIRCLE RADIUS CENTER (+ 3PI/2 RADIANS))))
         (SLEEP 0.02))))

(PRINT '(ROTATING-SQUARE))


;;;--------------------
;;; Example 2
;;;--------------------

(DEFUN QUERY-CONTINUE (MESSAGE)
  (CLEAR-INPUT *QUERY-IO*)
  (FORMAT *QUERY-IO* "~A" MESSAGE)
  (READ-LINE  *QUERY-IO* NIL NIL))


(DEFUN BROWNING ()
  (LET ((I 100000)
        (C (RANDOM #X1000000))
        (OX 250)
        (OY 250)
        (X (RANDOM 500))
        (Y (RANDOM 500)))
    (DO-WINDOW (:TITLE "Examples -- Browning" :TIMEOUT 0.01)
      (SET-COLOR C)
      (WHEN (< (+ (ABS (- OX X)) (ABS (- OY Y))) 400)
        (DRAW-LINE OX OY X Y))
      (SETF I (1- I)
            C (MOD (+ C (* (AREF #(#X10000 #X100 #X1) (RANDOM 3))
                           (RANDOM 10))) #X1000000)
            OX X
            OY Y
            X (MOD (+ X (- (RANDOM 21) 10)) 500)
            Y (MOD (+ Y (- (RANDOM 21) 10)) 500))
      (WHEN (ZEROP I)
        (QUERY-CONTINUE  "Terminé; tapez RET: ")
        (RETURN-FROM-DO-WINDOW)))))

(PRINT '(BROWNING))


;;;--------------------
;;; 
;;;--------------------


(DEFUN DRAW-FUNCTION (FUN MIN-X MAX-X MIN-Y MAX-Y)
  (LET ((WIDTH  (WINDOW-WIDTH))
        (HEIGHT (WINDOW-HEIGHT)))
    (FLET ((WX (X) (* WIDTH  (/ (- X MIN-X) (- MAX-X MIN-X))))
           (WY (Y) (* HEIGHT (/ (- Y MIN-Y) (- MAX-Y MIN-Y)))))
      (SET-RGB-COLOR 255 255 255)
      (DRAW-RECTANGLE 0 0 WIDTH HEIGHT T)
      (SET-RGB-COLOR 0 0 50)
      (DRAW-LINE 0 (WY 0) WIDTH (WY 0))
      (DRAW-LINE (WX 0) 0 (WX 0) HEIGHT)
      (SET-RGB-COLOR 0 0 0)
      (LOOP
         :FOR X :FROM MIN-X :TO MAX-X :BY (/ (- MAX-X MIN-X) WIDTH)
         :DO (LET ((Y (IGNORE-ERRORS (FUNCALL FUN X))))
               (WHEN Y (DRAW-POINT-AT (WX X) (WY Y)))))))) 
       

(defun test-draw-function ()
  (DO-WINDOW (:TITLE "Courbes")
    (DRAW-FUNCTION (FUNCTION SIN) (* -4 PI) (* 4 PI) -1.2 +1.2)
    (QUERY-CONTINUE "Terminé; tapez RET : ")
    (RETURN-FROM-DO-WINDOW)))

(print '(test-draw-function))


;;;--------------------
;;; Turtles Examples
;;;--------------------


(defun ex-turtle-1 ()
  (loop
     :initially (clear)
     :for i :below 6
     :do (draw (/ 50 (expt (1+ i) 3/4))) (turn 45) (split (turn -70))))

(PRINT '(ex-turtle-1))


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

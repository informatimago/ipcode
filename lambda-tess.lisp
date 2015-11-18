;;;; -*- coding: utf-8 -*-

(in-package "IPL-USER")

(defun draw-thick-lambda (size &optional mirror)
  (setf mirror (if mirror -1 1))
  (flet ((left  () (turn (* mirror 60)))
         (right () (turn (* mirror -60))))
    (draw size)         (left) 
    (draw size)         (right) (right)
    (draw size)         (left)
    (draw size)         (left) (left)
    (draw (* 3 size))   (left)
    (draw size)         (left) (left)
    (draw size)         (right)
    (draw (* 5/4 size)) (right)
    (draw (* 1/4 size)) (left)
    (draw (* 3/4 size)) (left) (left)
    (draw (* 1/4 size))))


(defun draw-centered-lambdas (size)
  (move size) (turn -120) (move (* 2 size)) (turn 120)
  (turn 60) (move size) (turn -60)
  (repeat 6
          (draw-thick-lambda size)
          (move (- size))
          (turn -60))
  (turn 240) (move size) (turn -240)
  (move size) (turn 120) (move (* 2 size)) (turn -120))


(defun tesselate-lambda-rec ()
  (clear)
  (labels ((tess (size)
             (when (< 1 size)
               (draw-centered-lambdas size)
               (tess (/ size 4)))))
    (tess 120)))


(defun draw-position ()
  (move 5)
  (turn 90)
  (repeat 360
          (draw 0.1)
          (turn 1))
  (turn 90)
  (move 5)
  (turn 180))


(defun draw-turtle ()
  (turn -90)
  (draw 5)
  (turn 120)
  (draw 10)
  (turn 120)
  (draw 10)
  (turn 120)
  (draw 5)
  (turn 90))

;;;; THE END ;;;;  

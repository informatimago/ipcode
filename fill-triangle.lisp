;;;; -*- coding: utf-8 -*-

(use-package :ipl-clx)




(defun solve-line-y (ax ay bx by)
  ;; Calcule la fonction g(y)=x donnant les points de la ligne AB.
  (assert (/= ay by))
  (let* ((p  (/ (- ax bx) (- ay by)))
         (q  (/ (- (+ ax bx) (* p (+ ay by))) 2)))
    (lambda (y) (+ (* p y) q))))

(defun fill-half-triangle (a b ac)
  ;; Remplissage du triangle ayant une base horizontale
  ;; donné par le segment ab et la droite ac.
  (when (< (y a) (y b))
    (let ((ab (solve-line-y (x a) (y a) (x b) (y b))))
      (loop
         :for y :from (y a) :to (y b)
         :do (draw-line (funcall ac y) y (funcall ab y) y)))))

(defun fill-triangle (a b c)
  "Fill the triangle given by its three vertices."
  ;; L'idée générale, est de dessiner des segments horizontaux
  ;; bornés par les côtés du triangle.
  ;; On trie les sommets verticallement:
  (when (> (y a) (y b)) (rotatef a b))
  (when (> (y a) (y c)) (rotatef a c))
  (when (> (y b) (y c)) (rotatef b c))
  (assert (<= (y a) (y b) (y c)))
  (if (= (y a) (y c))
      ;; Un triangle dégénéré en un point ou une ligne horizontale:
      (draw-line (min (x a) (x b) (x c)) (y a) (max (x a) (x b) (x c)) (y c))
      (progn
        ;; Un triangle normal: on le rempli comme deux sous-triangles séparés
        ;; par une ligne horizontale passant par le sommet intermédiaire:
        (assert (< (y a) (y c)))
        (let ((ac (solve-line-y (x a) (y a) (x c) (y c))))
          (fill-half-triangle a b ac)
          (fill-half-triangle b c ac)))))
      


;; Maintenant, on peut optimiser cet algorithme en introduisant
;; l'algorithme de Bresenham pour calculer une ligne:

;; http://en.wikipedia.org/wiki/Bresenham's_line_algorithm
;; Optimized bresenham's line algorithm:
;;  function line(x0, x1, y0, y1)
;;      boolean steep := abs(y1 - y0) > abs(x1 - x0)
;;      if steep then
;;          swap(x0, y0)
;;          swap(x1, y1)
;;      if x0 > x1 then
;;          swap(x0, x1)
;;          swap(y0, y1)
;;      int deltax := x1 - x0
;;      int deltay := abs(y1 - y0)
;;      int error := 0
;;      int ystep
;;      int y := y0
;;      if y0 < y1 then ystep := 1 else ystep := -1
;;      for x from x0 to x1
;;          if steep then plot(y,x) else plot(x,y)
;;          error := error + deltay
;;          if 2×error >= deltax
;;              y := y + ystep
;;              error := error - deltax





(loop
   :repeat 1000
   :do (set-rgb-color (random 256) (random 256) (random 256))
   :do (fill-triangle (make-point :x (random 512) :y (random 342))
                      (make-point :x (random 512) :y (random 342))
                      (make-point :x (random 512) :y (random 342))))


(defun arrangements-sans-repeat (n list)
  (cond
    ((null list)  '())
    ((= 0 n)      '(()))
    ((= 1 n)      (mapcar (function list) list))
    (t (nconc (mapcar (lambda (rest) (cons (first list) rest))
                    (arrangements-sans-repeat (1- n) (rest list)))
            (arrangements-sans-repeat n (rest list))))))


(defun square           (x)   (* x x))
(defun distance-squared (p q) (+ (square (- (x p) (x q)))
                                 (square (- (y p) (y q)))))
(defun triangle-size (tri)
  (+ (distance-squared (first tri)  (second tri))
     (distance-squared (first tri)  (third tri))
     (distance-squared (second tri) (third tri))))

(let* ((triangles
       (let ((points (make-array (list (truncate 512 16) (truncate 342 16)))))
         (loop
            :for j :from 0 :below (truncate 342 16)
            :for y :from 0 :by 16
            :do (loop
                   :for i :from 0 :below (truncate 512 16)
                   :for x :from (if (oddp j) 0 8) :by 16
                   :do (setf (aref points i j)
                             (make-point :x (+ x (random 16))
                                         :y (+ y (random 16))))))
         (loop
            :for j :from 0 :below (1- (truncate 342 16))
            :nconc (loop
                      :for i :from 0 :below (1- (truncate 512 16))
                      :collect (list (aref points i j)
                                     (aref points (1+ i) j)
                                     (aref points i (1+ j)))))))
       (max-size (reduce (function max)
                         (mapcar (function triangle-size) triangles))))
  (dolist (triangle  triangles)
    (let ((size (sqrt (/ (triangle-size triangle) max-size))))
      (set-rgb-color (random (truncate (* 256 size)))
                     (random (truncate (* 256 size)))
                     (random (truncate (* 256 size)))))
    (apply (function fill-triangle) triangle)))







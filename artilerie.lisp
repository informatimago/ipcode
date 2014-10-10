;;;;**************************************************************************
;;;;FILE:               artilerie.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a funny little game of canon fight.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-03-19 <PJB> Created.
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
;;;; -*- coding:utf-8 -*-

;; /home/pjb/works/initprog/code

(defpackage "ARTILERIE" 
  (:use "COMMON-LISP" "IPL-CLX")
  (:export "MAIN"))
(in-package "ARTILERIE")


(DEFSTRUCT (SEGMENT (:TYPE LIST)) from to)
(DEFUN SEGMENT (from to)
  "Retourne un nouveau segment 2D."
  (MAKE-SEGMENT :from from :to to))

(defun segments-to-polygon (segs)
  (cons (segment-from (first segs))
        (loop :for (from to) :in segs
           :collect to)))

(defun make-ground (seg)
  (if (< 2 (- (x (segment-to seg)) (x (segment-from seg))))
      (let ((mid (point (truncate (+ (x (segment-to seg))
                                     (x (segment-from seg)))
                                  2)
                        (+ (truncate (+ (y (segment-to seg))
                                         (y (segment-from seg)))
                                      2)
                           (let ((var (truncate (- (x (segment-to seg))
                                                   (x (segment-from seg)))
                                                2)))
                             (- (random var) (truncate var 2)))))))
        (nconc (make-ground (segment (segment-from seg) mid))
               (make-ground (segment mid (segment-to seg)))))
      (list seg)))


(WITH-WINDOW (:display-name "thalassa:0.0")
  (loop
     :do
     (set-color (IPL-CLX::RGB 150 200 255))
     (draw-rectangle 0 0 (WINDOW-width) (window-height) t)
     (set-color (IPL-CLX::RGB 100 80 10))
     (draw-polygon
      (list*
       (point 512 0) (point 0 0)
       (loop
          :for ground = (SEGMENTS-TO-POLYGON
                         (make-ground (segment (point 0 150) (point 512 150))))
          :until (every (lambda (p) (< 0 (y p) 340)) ground)
          :finally (return ground)))
      t)
     :until (y-or-n-p "~%Done?")))



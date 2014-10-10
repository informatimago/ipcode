;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; X11 routines

(unuse-package :XLIB)
;; (ext:without-package-lock (:posix)
;;   (defun posix::hostent-addr-type (&rest args)
;;     (apply (function posix::hostent-addrtype) args)))

(defparameter *dpy* nil)
(defparameter *win* nil)
(defparameter *gctxt* nil)

(defun get-environment-variable (string)
  #-clisp (cdr (assoc string ext:*environment-list* :test #'string=))
  #+clisp (ext:getenv string))

(defun parse-display-variable (s)
  (let* ((colon (position #\: s))
         (dot (position #\. s :start colon))
         (host-name (if (zerop colon) "localhost" (subseq s 0 colon)))
         (display-number (parse-integer s :start (1+ colon) :end dot)))
    (values host-name display-number)))

(defun open-window ()
  (multiple-value-bind (host display)
      (parse-display-variable (get-environment-variable "DISPLAY"))
    (let* ((dpy (xlib:open-display "")) ;; host :display display))
           (screen (xlib:display-default-screen dpy))
           (black (xlib:screen-black-pixel screen))
           (white (xlib:screen-white-pixel screen))
           (win (xlib:create-window :parent (xlib:screen-root screen)
                                    :background white
                                    :BACKING-STORE :ALWAYS
                                    :x 0 :y 0 :width 500 :height 500))
           (gcontext (xlib:create-gcontext :drawable win
                                           :background white
                                           :foreground black)))
      (xlib:map-window win)
      (xlib:display-force-output dpy)
      (setf *dpy* dpy
            *win* win
            *gctxt* gcontext)
      win)))

(defun close-window ()
  (when *gctxt*
    (XLIB:FREE-GCONTEXT  *gctxt*) (setf *gctxt* nil)
    (XLIB:DESTROY-WINDOW *win*)   (setf *win* nil)
    (xlib:close-display  *dpy*)   (setf *dpy* nil))
  (values))
   
(defun rgb (r g b)
  (xlib:make-color :red r :green g :blue b))

(defun set-color (color)
  (setf (xlib:GCONTEXT-FOREGROUND *gctxt*) color))

(defun set-color-rgb (r g b)
  (setf (xlib:GCONTEXT-FOREGROUND *gctxt*)
        (xlib:make-color :red r :green g :blue b)))

(defun draw-line (x1 y1 x2 y2)
  (xlib:draw-line *win* *gctxt* (round x1) (round y1) (round x2) (round y2))
  (xlib:display-force-output *dpy*))

(defun draw-polygon (points)
  (loop for (fr to) on (cons (car (last points)) points)
       while to
       do (xlib:draw-line *win* *gctxt* (round (x fr)) (round (y fr))
                          (round (x to)) (round (y to))))
  (xlib:display-force-output *dpy*))


(defun draw-polygon (points)
  (xlib:draw-lines *win* *gctxt* 
                   (mapcan (lambda (pt) (list (round (x pt)) (round (y pt))))
                           (cons (car (last points)) points)))
  (xlib:display-force-output *dpy*))

(defun x (pt) (car pt))
(defun y (pt) (cdr pt))
(defun point (x y) (cons x y))


(defun draw-step (poly)
  (set-color #xFFFFFF)
  ;;(xlib:draw-rectangle *win* *gctxt* 0     0  800 800 'fill)
  (xlib:draw-rectangle *win* *gctxt* 0     0  240  40 'fill)
  (xlib:draw-rectangle *win* *gctxt* 0    40   40 200 'fill)
  (xlib:draw-rectangle *win* *gctxt* 40  200  200  40 'fill)
  (xlib:draw-rectangle *win* *gctxt* 200  40   40 160 'fill)
  (set-color #xC0C0C0)
  (XLIB:draw-rectangle *win* *gctxt* 40 40 160 160 'fill)
  (set-color #x000000)
  (draw-polygon poly))


(defun pt-on-circle (radius center angle)
  (point (+ (* radius (sin angle)) (x center))
         (+ (* radius (cos angle)) (y center))))


(defun squares ()
  (unwind-protect
       (progn
         (open-window)
         (loop
            (loop
               with pi/2 = (/ pi 2)
               with 2pi = (* 2 pi)
               with 3pi/2 = (/ pi 2/3)
               with pi/180 = (/ pi 180)
               with radius = 100
               with center = (point 120 120)
               for angle from 0 to 89
               for radians = (* pi/180 angle)
               do (draw-step
                   (list (pt-on-circle radius center radians)
                         (pt-on-circle radius center (+ pi/2 radians))
                         (pt-on-circle radius center (+ pi radians))
                         (pt-on-circle radius center (+ 3pi/2 radians))))
               (sleep 0.01))))
    (close-window)))


  
(defun browning ()
  (unwind-protect
       (progn
         (open-window)
         (loop for c = (random #x1000000)
            then (mod (+ c (* (aref #(#x10000 #x100 #x1) (random 3))
                              (random 10))) #x1000000)
            for ox = 250 then x
            for oy = 250 then y
            for x = (random 500) then (mod (+ x (- (random 21) 10)) 500)
            for y = (random 500) then (mod (+ y (- (random 21) 10)) 500)
            do (set-color c)
            (when (< (+ (abs (- ox x)) (abs (- oy y))) 400)
              (draw-line ox oy x y))))
    (close-window)))


(defun alan ()
  (unwind-protect
       (progn
         (open-window)
         (let ((image (make-array 2048 :element-type '(unsigned-byte 8))))
           (map-into image (lambda() (random 256)))
           (xlib:put-image *win* *gctxt* image :depth 15
                               :x 0 :y 0 :width 32 :height 32
                               :format :z-pixmap)
           (print (xlib:display-bitmap-format *dpy*))
           (describe *dpy*)
           (xlib:display-force-output *dpy*)))
    (close-window)))

;; (browning) (squares)

;; http://www.xfree86.org/current/manindex3.html

(defun test-put-image ()
  (open-window)
  (setf im (let ((visi (XLIB:SCREEN-ROOT-VISUAL-INFO
                        (XLIB:DISPLAY-DEFAULT-SCREEN *dpy*)))
                 ;; #S(XLIB:VISUAL-INFO :ID 35 :CLASS :TRUE-COLOR
                 ;; :RED-MASK 16711680
                 ;; :GREEN-MASK 65280 :BLUE-MASK 255 :BITS-PER-RGB 8
                 ;; :COLORMAP-ENTRIES 256)
                 (data (make-array
                        (* 32 48 (ceiling (XLIB:DRAWABLE-DEPTH *win*) 8))
                        :element-type '(unsigned-byte 8))))
             (map-into data (lambda () (random 256)))
             (xlib:create-image
              :data data :format :x-pixmap 
              :width  48
              :height 32
              :depth      (XLIB:DRAWABLE-DEPTH *win*)
              :red-mask   (xlib:visual-info-red-mask   visi)
              :green-mask (xlib:visual-info-green-mask visi)
              :blue-mask  (xlib:visual-info-blue-mask  visi)
              :bits-per-pixel (* (ceiling (XLIB:DRAWABLE-DEPTH *win*) 8) 8)
              :bytes-per-line (* (ceiling (XLIB:DRAWABLE-DEPTH *win*) 8) 48)
              :byte-lsb-first-p nil
              :bit-lsb-first-p  nil
              ;; :unit ?
              :pad 0
              :left-pad 0)))

  (setf im (let* ((visi (XLIB:SCREEN-ROOT-VISUAL-INFO
                         (XLIB:DISPLAY-DEFAULT-SCREEN *dpy*)))
                  ;; #S(XLIB:VISUAL-INFO :ID 35 :CLASS :TRUE-COLOR
                  ;; :RED-MASK 16711680
                  ;; :GREEN-MASK 65280 :BLUE-MASK 255 :BITS-PER-RGB 8
                  ;; :COLORMAP-ENTRIES 256)
                  (data   (make-array '(32 24) :element-type 'xlib:pixel))
                  (pixels (make-array (* 32 24) :element-type 'xlib:pixel
                                      :displaced-to data
                                      :displaced-index-offset 0)))
             (map-into pixels (lambda () (random (expt 2 24))))
             (xlib:create-image
              :data data :format :z-pixmap 
              :width  (array-dimension data 0)
              :height (array-dimension data 1)
              :depth      (XLIB:DRAWABLE-DEPTH *win*)
              :red-mask   (xlib:visual-info-red-mask   visi)
              :green-mask (xlib:visual-info-green-mask visi)
              :blue-mask  (xlib:visual-info-blue-mask  visi)
              )))

  ;; doesn't work:
  (XLIB:PUT-IMAGE *win* *gctxt* im :x 10 :y 10
                  :src-x 0 :src-y 0
                  :width  (xlib:image-width im)
                  :height (xlib:image-height im))

  (xlib:display-force-output *dpy*)  )

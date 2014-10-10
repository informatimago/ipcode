
;;    (require 'clx)
    
    
    
(defvar *display* nil)
(defvar *screen* nil)
(defvar *rootwin* nil)
(defvar *gc-set* nil)
(defvar *gc-clear* nil)
(defvar *win-queue* nil)
    
    
    
(defun do-test (&optional dpy)
  (unless *display*
    (setf *display* (funcall #'xlib:open-default-display #-clisp dpy))
    (setf *screen* (car (xlib:display-roots *display*)))
    (setf *rootwin* (xlib:screen-root *screen*))
    (setf *gc-set*
    	  (xlib:create-gcontext :drawable *rootwin*
                                :background 0
                                :foreground 1))
    (setf *gc-clear* (xlib:create-gcontext :drawable *rootwin*
                                           :background 0
                                           :foreground 0)))
    
    
    
  (let ((window (xlib:create-window :width 100
                                    :height 100
                                    :x 10
                                    :y 10
                                    :parent *rootwin*
                                    )))
    (xlib:map-window window)
    (push window *win-queue*)
    (let ((clip-pmask (xlib:create-pixmap :drawable window
                                          :width 100
                                          :height 100
                                          :depth 1)))
      (xlib:draw-rectangle clip-pmask *gc-set* 0 0 100 100 t)
      (xlib:draw-arc clip-pmask *gc-clear* 40 40 20 20 0 (* 2 pi) t)
      (xlib:display-force-output *display*)
    
    
    
      (let ((gc (xlib:create-gcontext :drawable window
                                      :background 0
                                      :foreground 65535
                                      :clip-mask clip-pmask
                                      :clip-x 0
                                      :clip-y 0)))
    	(xlib:clear-area window :width 100 :height 100)
    	(xlib:draw-rectangle window *gc-clear* 0 0 100 100 t)
    	(xlib:draw-rectangle window gc 30 30 60 60 t)
    	(xlib:free-gcontext gc))
      (xlib:display-force-output *display*))))
    
    
    
(defun clearup ()
  (loop for win in *win-queue*
     do (progn (xlib:unmap-window win)
               (xlib:destroy-window win)))
  (setf *win-queue* nil)
  (xlib:display-force-output *display*))
    

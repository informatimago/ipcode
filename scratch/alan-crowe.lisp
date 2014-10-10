From: Alan Crowe <alan@cawtech.freeserve.co.uk>
Subject: Re: Simulation: Changes of opinion (a little less newbie...)
Newsgroups: comp.lang.lisp
Date: 24 Aug 2005 17:23:45 +0100

Nathan Baum wrote
> If you want a more convincing movie look, see if inserting
>
>   (format t "~A[2J" #\Escape)

Or you could try an X11 version:

(require :clx)

(defun neighbour (n)
  (multiple-value-bind (quotient remainder)
      (floor (+ n 5) 3)
    (list (- (mod quotient 3) 1)
	  (- remainder 1))))

(defun pick-neighbour (i j w)
  (apply #'aref w
	 (mapcar
	  (lambda(base offset modulus)
	    (mod (+ base offset) modulus))
	  (list i j)
	  (neighbour (random 8))
	  (array-dimensions w))))

(defun situacion-inicial (length breadth)
  (make-array (list length breadth)
	      :initial-contents
	      (loop repeat length
		    collect (loop repeat breadth
				  collect (random 2)))))

(defun x-version (&optional (host "")(size 10)(ancho 10)(alto 10))
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (win (xlib:create-window
	       :parent root-window
	       :x 0
	       :y 0
	       :width (* size ancho)
	       :height (* size alto)

	       :background white
	       :event-mask '(:exposure)))
	 (zero (xlib:create-gcontext :drawable win
				     :foreground white))
	 (one (xlib:create-gcontext :drawable win
				    :foreground black))
	 (torus (situacion-inicial ancho alto)))
    (xlib:map-window win)
    (loop (xlib:event-case (display :force-output-p t
				    :discard-p t
				    :timeout 0)
			   (:exposure ()
				      (dotimes (i ancho)
					(dotimes (j alto)
					  (xlib:draw-rectangle win
							       (if (zerop (aref torus i j))
								   zero
								   one)
							       (* i size)
							       (* j size)
							       size
							       size
							       'fill)))))
	  (let ((i (random ancho))
		(j (random alto)))
	    (setf (aref torus i j)
		  (pick-neighbour i j torus))
	    (xlib:draw-rectangle win
				 (if (zerop (aref torus i j))
         			     zero
				     one)
				 (* i size)
				 (* j size)
				 size
				 size
				 'fill)))))

I don't know whether I have managed to write portable CLX
code, but black is winning in the window I'm watching it run
in just now. Oh, actually white is making a come back, but
is still quite sparse.

Alan Crowe
Edinburgh
Scotland
			      
	       

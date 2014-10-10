;;;; Server side menu

;;; The qestion on comp.lang.lisp has been whether one can
;;; deploy applications over the web using X11. It all
;;; hinges on latency.  When the user clicks to obtain a
;;; window, does he then get confused by the delay before it
;;; appears?

;;; One idea is to draw the menu items into pixmaps.  and
;;; use the pixmaps as backgrounds to the windows.  Pixmaps
;;; live server side. So making the menu pop-up takes two
;;; map commands, to map the window and the subwindows
;;; containing the menu items.

;;; This is proof of concept code, written to demonstrate
;;; that the window is popped up and down with very short
;;; commands

;;; It doesn't really address the point raised on
;;; comp.lang.lisp We are still looking at a round-trip,
;;; with the user's click going to the client program and
;;; back to the server, before the menu can appear If the
;;; latency of the link is seconds, it will still take
;;; seconds for the window to pop up.

;;; Nevertheless, I think the concern on c.l.l. was actually
;;; that one would check ones link, see it pinging 200ms and
;;; think that was adequate. Then one would find that the
;;; client program was mucking about, doing several round
;;; trips and sending a few big gobs of data as well, so
;;; that one needed 10ms ping times to get acceptable
;;; performance out of that particular client.  My code
;;; suggests that a carefully written client might avoid
;;; that trap.

;;; There is another worry. The technique of drawing to a
;;; pixmap and using that as the window background, seems to
;;; avoid the whole issue of client side refresh. I'm
;;; guessing that the issue here is that in the old days,
;;; the server simply didn't have enough memory for this to
;;; be a useful technique. Even to today there might be an
;;; issue of scaling.  At least with client side refresh,
;;; the number of clients you may have is not limited by
;;; server side resources, but it numerous clients start
;;; writing big pixmaps to the server, at some point,
;;; create-pixmap will return an out of memory error

(require :clx)

(defvar item-height 20); this should be computed to suit the font
                       ; used for displaying the item

;;; Irritating menu that pops up for 2 seconds,
;;; Then pops down for 3 seconds.
;;; It also emphasises that the client program is not resending the 
;;; text in response to expose events
(defun menu(list-of-strings)
  (let ((display (xlib:open-display "")))
    (unwind-protect
	(let* ((screen (first (xlib:display-roots display)))
	       (root-window (xlib:screen-root screen))
	       (black (xlib:screen-black-pixel screen))
	       (white (xlib:screen-white-pixel screen))
	       (menu-window (xlib:create-window
			     :parent root-window
			     :background black
			     :x 0
			     :y 0
			     :width 200
			     ;; bad, I should find out how long the strings are
			     ;; and size the window to suit
			     :height (* item-height (length list-of-strings))
			     :event-mask '(:exposure :button-press)))
	       (font (xlib:open-font
		      display
		      "-*-lucida-medium-r-*-*-12-*-*-*-*-*-*"))
	       (grackon (xlib:create-gcontext
			 :drawable root-window
			 :foreground white
			 :background black))
	       ;; This is the key step, creating pixmaps,
	       ;; so that the images of the text are stored server-side
	       (pixmap-list (mapcar
			     (lambda(string)
			       (xlib:create-pixmap
				:drawable root-window
				:width (xlib:text-width font string)
				:height item-height
				:depth 24))
			     list-of-strings))
	       ;; Now we use them as the backgrounds to our windows
	       (item-window-list (let ((count 0))
				   (mapcar
				    (lambda(pixmap)
				      (xlib:create-window
				       :parent menu-window
				       :x 5
				       :y (prog1 (* count item-height)
					    (incf count))
				       :width (xlib:drawable-width pixmap)
				       :height item-height
				       :background pixmap
				       :event-mask '(:button-press)))
				    pixmap-list))))
	  ;; The texts of the items gets written to the pixmaps
	  ;; instead of to the windows
	  (loop for pixmap in pixmap-list
		and item-name in list-of-strings do
		(xlib:draw-glyphs
		 pixmap
		 grackon
		 0 (round (* 3/4 item-height))
		 item-name))
	  (xlib:map-window menu-window)
	  (xlib:map-subwindows menu-window)
	  (do ((event-value
		#1=(xlib:event-case (display :force-output-p t
					  :timeout 2 
					  :discard-p t)
				 (:exposure()
					   ;; How does this code retransmit the texts
					   ;; of the menu items
					   (format t "Lazy bones, sleeping in the sun,~%")
					   (format t "Howya gonna get yer days work done?~%"))
				           ;; It doesn't. That is the point.
				 (:button-press(window)
					       ;; This is where we get the benefit
					       ;; of putting each item in its own
					       ;; little window
					       ;; We could look at the pointer 
					       ;; position and work out which window
					       ;; it is in, but we have set things
					       ;; up so that the server works out
					       ;; which item the pointer is on.
					       (if (member window item-window-list)
						   (nth (position window
								  item-window-list)
							list-of-strings))))
		#1#))
	      (event-value event-value)
	      (xlib:unmap-subwindows menu-window)
	      (xlib:unmap-window menu-window)
	      (xlib:display-finish-output display)
	      (loop for i from 3 downto 1 do
		    (print i)
		    (sleep 1))
	      (xlib:map-window menu-window)
	      (xlib:map-subwindows menu-window)))
      (xlib:close-display display))))

(print(menu '("Lamb" "Beef" "Pork" "Salmon" "Trout")))
			    
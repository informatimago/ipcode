;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; X11 routines

#-clisp (require :clx)
;;(unuse-package :XLIB)

(defparameter *dpy* nil)
(defparameter *win* nil)
(defparameter *gctxt* nil)
(defparameter *window-width* 600)
(defparameter *window-height* 400)

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
    (let* ((dpy (xlib:open-display host :display display))
           (screen (xlib:display-default-screen dpy))
           (black (xlib:screen-black-pixel screen))
           (white (xlib:screen-white-pixel screen))
           (win (xlib:create-window :parent (xlib:screen-root screen)
                                    :background white
                                    :x 0 :y 0 :width *window-width*
                                    :height *window-height*))
           (gcontext (xlib:create-gcontext :drawable win
                                           :background white
                                           :foreground black)))
      (xlib:map-window win)
      (xlib:display-force-output dpy)
      (setf *dpy* dpy
            *win* win
            *gctxt* gcontext)
      win)))

(defun rgb (r g b)
  (xlib:make-color :red r :green g :blue b))

(defun set-color (color)
  (setf (xlib:GCONTEXT-FOREGROUND *gctxt*) color))

(defun set-color-rgb (r g b)
  (setf (xlib:GCONTEXT-FOREGROUND *gctxt*)
        (xlib:make-color :red r :green g :blue b)))

(defun draw-point (x y)
  (xlib:draw-point *win* *gctxt* x y))

(defun draw-square (x y size)
  (xlib:draw-rectangle *win* *gctxt* (round x) (round y) size size 'fill))
  
(defun clear-window ()
  (xlib:clear-area *win* :width *window-width* :height *window-height*)
  (xlib:display-force-output *dpy*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code for reading and displaying FITS images

(defclass imagen-fits ()
  ((cabecera :initarg :cabecera
	     :accessor cabecera)
   (data     :initarg :data
	     :accessor data)))

(defun iniciar-imagen-fits (cabecera)
  (let ((naxis (gethash "EJES" cabecera))
	(bzero (gethash "BZERO" cabecera)))
    (make-instance 'imagen-fits
		   :cabecera cabecera
		   :data   (make-array (coerce naxis 'list) 
				       :initial-element bzero))))

(declaim (inline trim-spaces-subseq))
(defun trim-spaces-subseq (cadena inicio fin)
  (string-trim " " (subseq cadena inicio fin))) 

(defun leer-cabecera (archivo)
  "Reads the header of the FITS file.
   TODO: -check if EXTEND or END keywords are present. If not -> bad, bad file...
         -If EXTEND -> read another 36 lines."
  (let ((línea-vector (make-array 80 :element-type '(unsigned-byte 8)))
	(tabla        (make-hash-table :test 'equal))
	ejes
	(fin          nil)
	(extend       nil))
    (dotimes (i 36)
      (read-sequence línea-vector archivo)
      (let* ((línea (map 'string (function code-char) línea-vector))
	     (llave (trim-spaces-subseq línea 0 8))
	     (valor (trim-spaces-subseq línea 9 30)))
	(unless (or (find #\' valor) (zerop (length valor)))
	  (setf valor (read-from-string valor)))
	(cond
	 ((zerop (length llave))) ; Nothing in the line...
	 ((string= llave "END")
	  (setf fin t))           ; This is good!
	 ((string= llave "EXTEND")
	  (setf extend t))        ; Then I should read 36 more lines...
	 ((string= llave "NAXIS")
	  (setf ejes (make-array valor :initial-element 0))
	  (setf (gethash "EJES" tabla) ejes)
	  (setf (gethash llave tabla) valor))	 
	 ((string= llave "NAXIS" :end1 (min 5 (length llave)))
	  (let ((eje (read-from-string (subseq llave 5))))
	    (setf (aref ejes (1- eje)) valor)))
	 (t
	  (setf (gethash llave tabla) valor)))))
    tabla))

(defmethod leer-data-en-imagen ((imagen imagen-fits) archivo)
  "Reads the pixels data.
   TODO: be able to read n-dimensional data."
  (let* ((data   (data imagen))
	 (ancho  (array-dimension data 0))
	 (alto   (array-dimension data 1))
	 (bscale (gethash "BSCALE" (cabecera imagen)))
	 (bitpix (gethash "BITPIX" (cabecera imagen))))
    (dotimes (j alto)
      (dotimes (i ancho)
	(let ((aux 0))
	  (loop for k downfrom (- bitpix 8) to 0 by 8 do
		(setf (ldb (byte 8 k) aux) (read-byte archivo)))
	  (incf (aref data i j) (* aux bscale)))))))

(defun set-intensidad (valor)
  (let ((color 0))
    (loop for k to 16 by 8 do
	  (setf (ldb (byte 8 k) color) valor))
    (set-color color)))

(defmethod dibujar ((imagen imagen-fits))
  (open-window)
  (let* ((data  (data imagen))
	 (ancho (array-dimension data 0))
	 (alto  (array-dimension data 1))
	 (bits  (expt 2 (gethash "BITPIX" (cabecera imagen)))))
    (dotimes (j alto)
      (dotimes (i ancho)
	(let ((intensidad-por-color (round (* (aref data i j) 256 (/ bits)))))
	  (set-intensidad intensidad-por-color)
	  (xlib:draw-point *win* *gctxt* i j))))
    (xlib:display-force-output *dpy*)))

(defun set-intensidad2 (valor)
  (let ((color 0))
    (loop for k to 16 by 8 do
	  (setf (ldb (byte 8 k) color) valor))
    valor))

(defmethod dibujar2 ((imagen imagen-fits))
  (open-window)
  (let* ((data        (data imagen))
	 (ancho       (array-dimension data 0))
	 (alto        (array-dimension data 1))
	 (bits        (expt 2 (gethash "BITPIX" (cabecera imagen))))
	 (data-en-seq (make-array 2048 :element-type '(unsigned-byte 8)))
	 (puntero     0))
    (dotimes (j alto)
      (dotimes (i ancho)
	(setf (aref data-en-seq puntero) (round (* (aref data i j) 256 (/ bits))))
	(incf puntero)))
    (xlib:put-raw-image *win* *gctxt* data-en-seq :depth 15
			:x 0 :y 0 :width 32 :height 32
			:format :z-pixmap)
    (xlib:display-force-output *dpy*)))

(defun dibujar-zoom (x y radio-zoom tamaño-pixel imagen)
  (let* ((data  (data imagen))  
	 (ancho (array-dimension data 0))
	 (alto  (array-dimension data 1))
	 (bits  (expt 2 (gethash "BITPIX" (cabecera imagen)))))
    (when (and (>= x 0)     (>= y 0) 
	       (<  x ancho) (<  y alto))
      (loop for j from (- y radio-zoom) to (+ y radio-zoom) do
	    (loop for i from (- x radio-zoom) to (+ x radio-zoom) do
		  (set-intensidad (if (and (>= i 0)     (>= j 0) 
					   (<  i ancho) (<  j alto))
				      (round (* (aref data i j) 256 (/ bits)))
				    0))
		  (draw-square (+ (* (- i x (- radio-zoom)) tamaño-pixel) ancho)
			       (* (- j y (- radio-zoom)) tamaño-pixel)
			       tamaño-pixel)))
      (xlib:display-force-output *dpy*))))
  
(defun probar (nombre)
  (with-open-file (archivo nombre :element-type '(unsigned-byte 8))
		  (let* ((imagen (iniciar-imagen-fits (leer-cabecera archivo))))
		    (leer-data-en-imagen imagen archivo)
		    (dibujar imagen)
		    (dotimes (i 200000)
		      (multiple-value-bind (x y)
			  (xlib:pointer-position *win*)
			(dibujar-zoom x y 10 10 imagen))))))
  
(defun alan ()		 
  (open-window)
  (let ((image (make-array 2048 :element-type '(unsigned-byte 8))))
    (map-into image (lambda() (random 256)))
    (xlib:put-raw-image *win* *gctxt* image :depth 24
                        :x 0 :y 0 :width 32 :height 32
                        :format :z-pixmap)
;    (print (xlib:display-bitmap-format *dpy*))
;    (describe *dpy*)
    (xlib:display-force-output *dpy*)))




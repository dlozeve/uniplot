(export #t)

(import :std/format
	:std/iter
	:std/misc/list
	:std/misc/string
	:dlozeve/fancy/format
	:dlozeve/uniplot/braille)

(def +default-colors+ '(green blue red yellow cyan magenta white))

(def (scale-fn lo hi)
  (lambda (x) (/ (- x lo) (- hi lo))))

(def (draw-canvas xs ys x-scale-fn y-scale-fn width: (width 160) height: (height 80))
  (def canvas (make-braille-canvas (1+ height) (1+ width)))
  (for ((x xs) (y ys))
    (braille-canvas-set! canvas
			 (inexact->exact (floor (* height (y-scale-fn y))))
			 (inexact->exact (floor (* width (x-scale-fn x))))
			 #t))
  canvas)

(def (line-plot lsts (colors +default-colors+)
		width: (width 160) height: (height 80)
		xlabel: (xlabel "") names: (names []))
  (define-values (xmin xmax ymin ymax canvases)
    (match lsts
      ([ys] (let ((xmin 0)
		  (xmax (length ys))
		  (ymin (apply min ys))
		  (ymax (apply max ys)))
	      (values xmin xmax ymin ymax
		      [(draw-canvas (iota (length ys)) ys
				    (scale-fn xmin xmax)
				    (scale-fn ymin ymax)
				    width: width height: height)])))
      ([xs ys] (let ((xmin (apply min xs))
		     (xmax (apply max xs))
		     (ymin (apply min ys))
		     (ymax (apply max ys)))
		 (values xmin xmax ymin ymax
			 [(draw-canvas xs ys
				       (scale-fn xmin xmax)
				       (scale-fn ymin ymax)
				       width: width height: height)])))
      ([xs . yss] (let* ((xmin (apply min xs))
			 (xmax (apply max xs))
			 (all-ys (flatten yss))
			 (ymin (apply min all-ys))
			 (ymax (apply max all-ys))
			 (x-scale-fn (scale-fn xmin xmax))
			 (y-scale-fn (scale-fn ymin ymax)))
		    (values xmin xmax ymin ymax
			    (for/collect ((ys yss))
			      (draw-canvas xs ys x-scale-fn y-scale-fn
					   width: width height: height)))))))
  (def canvases-str (canvases->string canvases colors))
  (def hor-size (u8vector-length (vector-ref (car canvases) 0)))
  (def vert-size (vector-length (car canvases)))
  (def plot (str
	     (format "      ┌─~a─┐\n~5,1F ┤ " (make-string hor-size #\─) ymax)
	     (string-subst canvases-str "\n" " │\n      │ ")
	     (format " │\n~5,1F ┤~a │\n" ymin (make-string (+ 1 hor-size) #\ ))
	     (format "      └─┬~a┬─┘\n" (make-string (- hor-size 2) #\─))
	     (format "\n     ~5,1F~a~5,1F\n" xmin (make-string (- hor-size 6) #\ ) xmax)))
  (def plot-with-legend (if (null? names)
			  plot
			  (str plot
			       (cursor-up (+ 4 vert-size))
			       (cursor-forward (+ 12 hor-size))
			       (apply str
				 (for/collect ((name names) (color colors))
				   (str (graphics-style [color]) name (graphics-style)
					(cursor-down 1)
					(cursor-back (string-length name)))))
			       (cursor-down (- (+ 4 vert-size) (length names)))
			       (cursor-back (+ 12 hor-size)))))
  (if (string-empty? xlabel)
    plot-with-legend
    (str plot-with-legend (make-string (quotient hor-size 2) #\ ) xlabel "\n")))

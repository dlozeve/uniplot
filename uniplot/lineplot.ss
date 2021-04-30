(export line-plot)

(import :std/format
	:std/iter
	:std/misc/list
	:std/misc/string
	:dlozeve/fancy/format
	:dlozeve/uniplot/braille)

(defstruct plot
  (str hsize vsize))

(def +default-colors+ '(green blue red yellow cyan magenta white))

(def (nanmin . rest)
  (apply min (filter finite? rest)))

(def (nanmax . rest)
  (apply max (filter finite? rest)))

(def (scale-fn lo hi)
  (lambda (x) (/ (- x lo) (- hi lo))))

(def (draw-canvas xs ys x-scale-fn y-scale-fn width: (width 160) height: (height 80))
  (def canvas (make-braille-canvas (1+ height) (1+ width)))
  (for ((x xs) (y ys) when (and (finite? x) (finite? y)))
    (braille-canvas-set! canvas
			 (inexact->exact (floor (* height (y-scale-fn y))))
			 (inexact->exact (floor (* width (x-scale-fn x))))
			 #t))
  canvas)

(def (add-border! plot xmin xmax ymin ymax)
  (def ymin-label (format "~5,1F" ymin))
  (def ymax-label (format "~5,1F" ymax))
  (def label-width (max (string-length ymin-label) (string-length ymax-label)))
  (def padding (make-string label-width #\ ))
  (set! (plot-str plot)
    (str
     padding " ┌─" (make-string (plot-hsize plot) #\─) "─┐\n"
     (make-string (- label-width (string-length ymax-label)) #\ ) ymax-label " ┤ "
     (string-subst (plot-str plot) "\n" (format " │\n~a │ " padding))
     " │\n"
     (make-string (- label-width (string-length ymax-label)) #\ ) ymax-label " ┤ "
     (make-string (+ 1 (plot-hsize plot)) #\ ) "│\n"
     (make-string label-width #\ ) " └─┬" (make-string (- (plot-hsize plot) 2) #\─) "┬─┘\n"
     "\n" padding (format "~5,1F~a~5,1F\n" xmin (make-string (- (plot-hsize plot) 6) #\ ) xmax)))
  (set! (plot-hsize plot) (+ (plot-hsize plot) label-width 5)))

(def (add-legend! plot names colors)
  (set! (plot-str plot)
    (str (plot-str plot)
	 (cursor-up (+ 4 (plot-vsize plot)))
	 (cursor-forward (+ 1 (plot-hsize plot)))
	 (apply str
	   (for/collect ((name names) (color colors))
	     (str (graphics-style [color]) name (graphics-style)
		  (cursor-down 1)
		  (cursor-back (string-length name)))))
	 (cursor-down (- (+ 4 (plot-vsize plot)) (length names)))
	 (cursor-back (+ 1 (plot-hsize plot))))))

(def (add-xlabel! plot xlabel)
  (set! (plot-str plot)
    (str (plot-str plot) (make-string (quotient (plot-hsize plot) 2) #\ ) xlabel "\n")))

(def (add-title! plot title)
  (set! (plot-str plot)
    (str "\n" (make-string (quotient (- (plot-hsize plot) 10) 2) #\ ) title "\n\n"
	 (plot-str plot))))

(def (line-plot lsts (colors +default-colors+)
		width: (width 160) height: (height 100)
		title: (title "") xlabel: (xlabel "") names: (names []))
  (define-values (xmin xmax ymin ymax canvases)
    (match lsts
      ([ys] (let ((xmin 0)
		  (xmax (length ys))
		  (ymin (apply nanmin ys))
		  (ymax (apply nanmax ys)))
	      (values xmin xmax ymin ymax
		      [(draw-canvas (iota (length ys)) ys
				    (scale-fn xmin xmax)
				    (scale-fn ymin ymax)
				    width: width height: height)])))
      ([xs ys] (let ((xmin (apply nanmin xs))
		     (xmax (apply nanmax xs))
		     (ymin (apply nanmin ys))
		     (ymax (apply nanmax ys)))
		 (values xmin xmax ymin ymax
			 [(draw-canvas xs ys
				       (scale-fn xmin xmax)
				       (scale-fn ymin ymax)
				       width: width height: height)])))
      ([xs . yss] (let* ((xmin (apply nanmin xs))
			 (xmax (apply nanmax xs))
			 (all-ys (flatten yss))
			 (ymin (apply nanmin all-ys))
			 (ymax (apply nanmax all-ys))
			 (x-scale-fn (scale-fn xmin xmax))
			 (y-scale-fn (scale-fn ymin ymax)))
		    (values xmin xmax ymin ymax
			    (for/collect ((ys yss))
			      (draw-canvas xs ys x-scale-fn y-scale-fn
					   width: width height: height)))))))
  (def plot (make-plot (canvases->string canvases colors)
		       (u8vector-length (vector-ref (car canvases) 0))
		       (vector-length (car canvases))))
  (add-border! plot xmin xmax ymin ymax)
  (unless (null? names) (add-legend! plot names colors))
  (unless (string-empty? xlabel) (add-xlabel! plot xlabel))
  (unless (string-empty? title) (add-title! plot title))
  (plot-str plot))

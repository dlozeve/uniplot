(export #t)

(import :std/iter
	:std/misc/list
	:std/misc/string
	:dlozeve/uniplot/braille
	:dlozeve/fancy/format)

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

(def (line-plot lsts width: (width 160) height: (height 80))
  (match lsts
    ([ys] (canvas->string
	   (draw-canvas (iota (length ys)) ys
			(scale-fn 0 (length ys))
			(scale-fn (apply min ys) (apply max ys))
			width: width height: height)))
    ([xs ys] (canvas->string
	      (draw-canvas xs ys
			   (scale-fn (apply min xs) (apply max xs))
			   (scale-fn (apply min ys) (apply max ys))
			   width: width height: height)))
    ([xs . yss]
     (canvases->string
      (let* ((x-scale-fn (scale-fn (apply min xs) (apply max xs)))
	     (all-ys (flatten yss))
	     (min-ys (apply min all-ys))
	     (max-ys (apply max all-ys))
	     (y-scale-fn (scale-fn min-ys max-ys)))
	(for/collect ((ys yss))
	  (draw-canvas xs ys x-scale-fn y-scale-fn
		       width: width height: height)))))))

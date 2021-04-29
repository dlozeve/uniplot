(export #t)

(import :std/iter
	:std/misc/list
	:std/misc/string
	:gerbil/gambit/hvectors
	:gerbil/gambit/bits
	:dlozeve/fancy/format)

(def +braille-signs+
  (list->u8vector (map (lambda (c) (- (char->integer c) (char->integer #\⠀)))
		       (string->list "⡀⠄⠂⠁⢀⠠⠐⠈"))))

(def (make-braille-canvas n m)
  (def canvas-n (+ (quotient n 4) (if (zero? (remainder n 4)) 0 1)))
  (def canvas-m (+ (quotient m 2) (if (zero? (remainder m 2)) 0 1)))
  (def vec (make-vector canvas-n #f))
  (for ((i (in-range canvas-n)))
    (vector-set! vec i (make-u8vector canvas-m 0)))
  vec)

(def (braille-canvas-ref canvas i j)
  (def c (u8vector-ref (vector-ref canvas (quotient i 4)) (quotient j 2)))
  (def offset (u8vector-ref +braille-signs+
			    (+ (remainder i 4) (* 4 (remainder j 2)))))
  (any-bits-set? c offset))

(def (braille-canvas-set! canvas i j v)
  (def old-offset (u8vector-ref (vector-ref canvas (quotient i 4)) (quotient j 2)))
  (def update-offset (u8vector-ref +braille-signs+
				   (+ (remainder i 4) (* 4 (remainder j 2)))))
  (def new-char (if v
		  (bitwise-ior old-offset update-offset)
		  (bitwise-and old-offset (bitwise-not update-offset))))
  (u8vector-set! (vector-ref canvas (quotient i 4)) (quotient j 2) new-char))

(def (canvas->string canvas)
  (def chars (for/collect ((i (in-range (vector-length canvas) 0 -1)))
	       (def row (vector-ref canvas (1- i)))
	       (for/collect ((j (in-range (u8vector-length row))))
		 (integer->char (+ (char->integer #\⠀) (u8vector-ref row j))))))
  (list->string (flatten (map (lambda (l) (append1 l #\newline)) chars))))

(def (indexf pred . lsts)
  (find pred (apply map list lsts)))

(def (canvases->string canvases colors)
  (def canvases-str (map canvas->string canvases))
  (def size (string-length (car canvases-str)))
  (apply str
    (for/collect ((i (in-range size)))
      (let* ((characters (map (lambda (s) (string-ref s i)) canvases-str))
	     (char-color (indexf (lambda (x) (or (char=? (car x) #\newline)
					    (char>? (car x) #\⠀)))
				 characters colors)))
	(match char-color
	  (#f " ")
	  ([#\newline col] "\n")
	  ([c col] (str (graphics-style [col]) c (graphics-style))))))))

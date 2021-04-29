#!/usr/bin/env gxi

(export main)

(import :std/misc/string
	:dlozeve/fancy/format
	:dlozeve/uniplot/lineplot)

(def (main . args)
  (let* ((xs (iota 1000 0 0.01))
	 (ys1 (map cos xs))
	 (ys2 (map (lambda (x) (/ 1 (+ 0.5 x))) xs))
	 (ys3 (map (lambda (x) (sin (* 2 x))) xs)))
    (displayln (line-plot [xs ys1 ys2 ys3] xlabel: "time"))))

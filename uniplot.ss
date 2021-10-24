#!/usr/bin/env gxi

(export main)

(import :std/iter
	:std/misc/func
	:std/text/csv
	:dlozeve/uniplot/lineplot)

(def (parse-number s)
  (or (string->number s) +nan.0))

(def (main . args)
  (def csv (read-csv-lines (current-input-port)))
  (def names (car csv))
  (def lsts
    (map reverse
	 (for/fold (lsts (repeat '() (length names)))
	     ((row (cdr csv)))
	   (map cons (map parse-number row) lsts))))
  (displayln (line-plot lsts title: (if (null? args) "" (car args)) names: names)))

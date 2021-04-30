#!/usr/bin/env gxi

(export main)

(import :std/iter
	:std/text/csv
	:dlozeve/uniplot/lineplot)

(def (main . args)
  (def csv (read-csv-lines (current-input-port)))
  (def names (car csv))
  (def lsts
    (apply map list
	   (for/collect ((row csv))
	     (for/collect ((x row))
	       (or (string->number x) +nan.0)))))
  (displayln (line-plot lsts
			xlabel: (car names) names: (cdr names))))

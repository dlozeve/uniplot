#!/usr/bin/env gxi

(import :std/build-script)

(defbuild-script
  '("uniplot/braille"
    "uniplot/lineplot"
    (static-exe: "uniplot"))
  optimize: #t)

#!/usr/bin/env gxi

(import :std/make)

(def lib-build-spec
  '("uniplot/braille"
    "uniplot/lineplot"))

(def bin-build-spec
  '((exe: "uniplot")))

(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def (main . args)
  (match args
    (["lib"]
     (make srcdir: srcdir
           optimize: #t
           debug: 'src
           static: #t
           lib-build-spec))
    (["bin"]
     (make srcdir: srcdir
	   optimize: #t
	   debug: #f
	   static: #t
	   build-deps: "build-deps-bin"
	   bin-build-spec))
    ([]
     (main "lib")
     (main "bin"))))

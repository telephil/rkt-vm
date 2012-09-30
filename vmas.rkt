#lang racket

;; VM assembler driver

(require racket/cmdline
	 "private/assembler.rkt"
	 "info.rkt")

(define (show-version)
  (printf "VM Assembler v~a~%" vm-version)
  (printf "Copyright (C) 2012 ~a~%" vm-author)
  (exit 0))

;; Command-line parsing
(define out (make-parameter "out.bin"))

(define (file-to-assemble)
  (command-line
   #:once-each
   [("-v" "--version") "Display the assembler version"
    (show-version)]
   [("-o" "--out") output-filename "Output file name"
    (out output-filename)]
   #:args (filename)
   filename))

;; Main
(define (main)
  (define filename (file-to-assemble))
  (with-output-to-file (out) #:exists 'replace
    (lambda ()
      (write-bytes (compile-file filename))))
  (values))

;; Execute main
(main)


;; VM assembler driver
#lang racket/base

(require racket/cmdline
	 "info.rkt"
	 "assembler/assembler.rkt")

(define (show-version)
  (printf "VM Assembler v~a~%" vm-version)
  (printf "Copyright (C) 2012 ~a~%" vm-author))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command-line parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define out (make-parameter "out.bin"))
(define filename
  (command-line
   #:once-each
   [("-v" "--version") "Display the assembler version"
    (show-version) (exit 0)]
   [("-o" "--out") output-filename "Output file name"
    (out output-filename)]
   #:args (filename)
   filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-output-to-file
    (out) #:exists 'replace
    (lambda ()
      (write-bytes (compile-file filename))
      (values)))



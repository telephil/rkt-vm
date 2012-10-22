;; VM assembler driver
#lang racket/base

(require racket/cmdline
         racket/path
         syntax/location
         setup/getinfo
         "core.rkt")

(define info (get-info/full
              (simplify-path (build-path (path-only (quote-module-path)) 'up))))

(define (show-version)
  (printf "VM Assembler v~a~%" (info 'version))
  (printf "Copyright (C) 2012 ~a~%" (info 'author)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command-line parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define out (make-parameter "out.bin"))
(define filename
  (command-line
   #:once-each
   [("-V" "--version") "Display the assembler version"
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

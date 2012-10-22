;; Debugger frontend
#lang racket/base

(require racket/cmdline
         racket/path
         syntax/location
         setup/getinfo
         "core.rkt")

(define info (get-info/full
              (simplify-path (build-path (path-only (quote-module-path)) 'up))))

(define (show-version)
  (printf "VM Debugger v~a~%" (info 'version))
  (printf "Copyright (C) 2012 ~a~%" (info 'author)))

;; Command-line parsing
(define memsize (make-parameter 1024))

(command-line
 #:once-each
 [("-V" "--version") "Display version information"
  (show-version) (exit 0)]
 [("-s" "--memsize") size "Memory size (in bytes)"
  (memsize (string->number size))])

;; Run debugger
(show-version)
(displayln "")
(run-debugger memsize)

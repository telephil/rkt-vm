;; Debugger frontend
#lang racket/base

(require racket/cmdline
	 "info.rkt"
	 "debugger/core.rkt")

(define (show-version)
  (printf "VM Debugger v~a~%" vm-version)
  (printf "Copyright (C) 2012 ~a~%" vm-author))

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

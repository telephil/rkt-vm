;; VM bytecode runner
#lang racket/base

(require racket/cmdline
	 "vm/core.rkt"
	 "info.rkt")

(define (show-version)
  (printf "VM v~a~%" vm-version)
  (printf "Copyright (C) 2012 ~a~%" vm-author))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command-line parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define memsize (make-parameter 1024))
(define dumpregs? (make-parameter #f))
(define filename (command-line
		  #:once-each
		  [("-V" "--version") "Display version information"
		   (show-version) (exit 0)]
		  [("-s" "--memsize") size "Memory size (in bytes)"
		   (memsize (string->number size))]
		  [("-d" "--dump-registers") () "Dump registers"
		   (dumpregs? #t)]
		  #:args (filename)
		  filename))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(create-vm (memsize))
(load-file filename)
(run)
(when (dumpregs?)
  (print-registers 10))
(values)



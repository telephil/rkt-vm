#lang racket

;; VM bytecode runner

(require racket/cmdline
	 "private/vm.rkt"
	 "info.rkt")

(define (show-version)
  (printf "VM v~a~%" vm-version)
  (printf "Copyright (C) 2012 ~a~%" vm-author)
  (exit 0))

;; Command-line parsing
(define memsize (make-parameter 1024))
(define dumpregs? (make-parameter #f))

;; Main
(define (main)
  (define filename (command-line
		    #:once-each
		    [("-V" "--version") "Display version information"
		     (show-version)]
		    [("-s" "--memsize") size "Memory size (in bytes)"
		     (memsize (string->number size))]
		    [("-d" "--dump-registers") () "Dump registers"
		     (dumpregs? #t)]
		    #:args (filename)
		    filename))
  (create-vm (memsize))
  (load-file filename)
  (run)
  (when (dumpregs?)
    (print-registers))
  (values))

;; Execute main
(main)


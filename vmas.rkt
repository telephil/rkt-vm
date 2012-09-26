#lang racket

;; VM assembler driver

(require racket/cmdline
	 "private/assembler.rkt"
	 "info.rkt")

(printf "VM Assembler v~a~%" vm-version)

;; Command-line parsing
(define outfilename (make-parameter "out.bin"))

(define (file-to-assemble)
  (command-line
   #:program "vmas"
   #:once-each
   [("-o" "--out") output-filename "Output file name"
    (outfilename output-filename)]
   #:args (filename)
   filename))

;; Main
(define (main)
  (define filename (file-to-assemble))
  (define bc (compile-file filename))

  (with-output-to-file (outfilename) #:exists 'replace
    (lambda ()
      (write-bytes bc)))

  (values))

;; Execute main
(main)


#lang racket

(require "private/assembler.rkt")

;; VM assembler driver

(printf "VM Assembler~%")

(define args (current-command-line-arguments))

(when (zero? (vector-length args))
  (printf "Usage: vmas <filename>~%")
  (exit 1))

(define filename (vector-ref args 0))
(define outfilename (string-replace filename ".asm" ".bin"))
(define bc (compile-file filename))

(with-output-to-file outfilename
  (lambda ()
    (write-bytes bc)))

#lang racket

(require "parser.rkt"
         "syntax.rkt")

(define (compute-label-addresses src)
  (letrec ([iter (λ (src idx h)
                   (cond
                     ([null? src] h)
                     (else
                      (define insn (car src))
                      (when (label-stx? insn)
                        (hash-set! h (label-stx-name insn) idx))
                      (iter (cdr src) (add1 idx) h))))])
    (iter src 0 (make-hash))))
    
(define (compile-string str)
  (define src (parse-string str))
  (define labels (compute-label-addresses src))
  (values src labels))

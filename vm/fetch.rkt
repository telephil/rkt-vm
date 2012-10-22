#lang racket/base

(require racket/function
         "memory.rkt"
         "../utils/bits.rkt")

(provide register-bytecode-transformer
         ptr-bytecode-transformer
         number-bytecode-transformer
         fetch-insn)

(define register-bytecode-transformer
  (make-parameter identity))

(define ptr-bytecode-transformer
  (make-parameter values))

(define number-bytecode-transformer
  (make-parameter identity))

(define (fetch-op mem ptr)
  (define op (load-dword/fwd! mem ptr))
  op)

(define (fetch-ptr mem ptr)
  (define reg (load-byte/fwd! mem ptr))
  (define off (load-qword/fwd! mem ptr))
  ((ptr-bytecode-transformer) reg off))

(define (fetch-reg mem ptr)
  (define bc (load-byte/fwd! mem ptr))
  ((register-bytecode-transformer) bc))

(define (fetch-number mem ptr)
  (define bc (load-qword/fwd! mem ptr))
  ((number-bytecode-transformer) bc))

(define (fetch-arg op bit mem ptr)
  (define reg? (bitwise-bit-set? op bit))
  (define imm? (bitwise-bit-set? op (sub1 bit)))
  (cond
   [(and reg? imm?) (fetch-ptr mem ptr)]
   [reg? (fetch-reg mem ptr)]
   [imm? (fetch-number mem ptr)]
   [else #f]))

(define (fetch-insn mem ptr)
  (define op (fetch-op mem ptr))
  (define arg1 (fetch-arg op 15 mem ptr))
  (define arg2 (fetch-arg op 13 mem ptr))
  (list (clear-tags op) arg1 arg2))

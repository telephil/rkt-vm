#lang racket/base

(require racket/contract/base)

(provide/contract
 [register->bytecode (symbol? . -> . integer?)]
 [register-count (-> integer?)]
 [bytecode->register (integer? . -> . string?)])

(struct register (name opcode))

(define (register->bytecode register)
  (hash-ref registers-by-symbol register #f))

(define (bytecode->register bc)
  (register-name (vector-ref registers bc)))

(define (register-count)
  (vector-length registers))

(define registers
  (vector
   (register "r0" #x00)
   (register "r1" #x01)
   (register "r2" #x02)
   (register "r3" #x03)
   (register "r4" #x04)
   (register "r5" #x05)
   (register "r6" #x06)
   (register "r7" #x07)
   (register "r8" #x08)
   (register "sp" #x09)
   (register "bp" #x0A)
   (register "ip" #x0B)))

(define registers-by-symbol
  #hash((r0 . #x00)
        (r1 . #x01)
        (r2 . #x02)
        (r3 . #x03)
        (r4 . #x04)
        (r5 . #x05)
        (r6 . #x06)
        (r7 . #x07)
        (r8 . #x08)
        (sp . #x09)
        (bp . #x0A)
        (ip . #x0B)))

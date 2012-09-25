#lang racket

(provide/contract
 [register->bytecode (symbol? . -> . integer?)])

(define (register->bytecode register)
  (hash-ref registers register))

(define registers
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
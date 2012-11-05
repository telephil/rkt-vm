#lang racket/base

(require rackunit
         "../vm/io.rkt")

;; out
(define (test-out size op at what)
  (define memory (make-bytes size 0))
  (op memory at what)
  memory)

;; bytes
(check-equal? (test-out 4 outb 0 1) (bytes 1 0 0 0))
(check-equal? (test-out 4 outb 1 8) (bytes 0 8 0 0))
(check-exn exn:fail? (lambda () (test-out 4 outb 10 1)))

;; word
(check-equal? (test-out 4 outw 0 1) (bytes 1 0 0 0))
(check-equal? (test-out 4 outw 1 10) (bytes 0 10 0 0))
(check-exn exn:fail? (lambda () (test-out 4 outw 10 1)))

;; dword
(check-equal? (test-out 8 outd 0 255) (bytes 255 0 0 0 0 0 0 0))
(check-equal? (test-out 8 outd 4 1024) (bytes 0 0 0 0 0 4 0 0))
(check-exn exn:fail? (lambda () (test-out 8 outd 10 1)))

;; fwd! functions
(define (test-load/fwd! op)
  (define memory (make-bytes 16 0))
  (define ptr (make-parameter 0))
  (op memory ptr)
  (ptr))

(check-equal? (test-load/fwd! inb/fwd!) 1)
(check-equal? (test-load/fwd! inw/fwd!) 2)
(check-equal? (test-load/fwd! ind/fwd!) 4)
(check-equal? (test-load/fwd! inl/fwd!) 8)

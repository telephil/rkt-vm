#lang racket/base

(require rackunit
         "../vm/memory.rkt")

;; creation
(check-equal? (create-memory 4) (bytes 0 0 0 0))

;; store
(define (test-store size op at what)
  (define memory (create-memory size))
  (op memory at what)
  memory)

;; bytes
(check-equal? (test-store 4 store-byte 0 1) (bytes 1 0 0 0))
(check-equal? (test-store 4 store-byte 1 8) (bytes 0 8 0 0))
(check-exn exn:fail? (lambda () (test-store 4 store-byte 10 1)))

;; word
(check-equal? (test-store 4 store-word 0 1) (bytes 1 0 0 0))
(check-equal? (test-store 4 store-word 1 10) (bytes 0 10 0 0))
(check-exn exn:fail? (lambda () (test-store 4 store-word 10 1)))

;; dword
(check-equal? (test-store 8 store-dword 0 255) (bytes 255 0 0 0 0 0 0 0))
(check-equal? (test-store 8 store-dword 4 1024) (bytes 0 0 0 0 0 4 0 0))
(check-exn exn:fail? (lambda () (test-store 8 store-dword 10 1)))

;; fwd! functions
(define (test-load/fwd! op)
  (define memory (create-memory 16))
  (define ptr (make-parameter 0))
  (op memory ptr)
  (ptr))

(check-equal? (test-load/fwd! load-byte/fwd!) 1)
(check-equal? (test-load/fwd! load-word/fwd!) 2)
(check-equal? (test-load/fwd! load-dword/fwd!) 4)
(check-equal? (test-load/fwd! load-qword/fwd!) 8)

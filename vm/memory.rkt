#lang racket/base

(require racket/contract/base)

(define (index? x)
  (and (integer? x) (or (zero? x) (positive? x))))

(provide/contract
 [create-memory (-> (and integer? positive?) bytes?)]
 [store-qword   (-> bytes? index? integer? void)]
 [load-qword    (-> bytes? index? integer?)]
 [store-dword   (-> bytes? index? integer? void)]
 [load-dword    (-> bytes? index? integer?)]
 [store-word    (-> bytes? index? integer? void)]
 [load-word     (-> bytes? index? integer?)]
 [store-byte    (-> bytes? index? integer? void)]
 [load-byte     (-> bytes? index? integer?)])

;; create-memory
(define (create-memory size)
  (make-bytes size 0))

(define (store dest idx v sz)
  (define data (integer->integer-bytes v sz #t))
  (bytes-copy! dest idx data))

(define (load dest idx sz)
  (define data (subbytes dest idx (+ idx sz)))
  (integer-bytes->integer data #t))

;; qword
(define (store-qword dest idx v)
  (store dest idx v 8))

(define (load-qword dest idx)
  (load dest idx 8))

;; dword
(define (store-dword dest idx v)
  (store dest idx v 4))

(define (load-dword dest idx)
  (load dest idx 4))

;; word
(define (store-word dest idx v)
  (store dest idx v 2))

(define (load-word dest idx)
  (load dest idx 2))

;; byte
(define (store-byte dest idx v)
  (bytes-set! dest idx v))

(define (load-byte dest idx)
  (bytes-ref dest idx))


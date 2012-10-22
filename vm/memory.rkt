#lang racket/base

(require racket/contract/base
         racket/function)

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
 [load-byte     (-> bytes? index? integer?)]
 [store-qword/fwd! (-> bytes? parameter/c integer? void)]
 [load-qword/fwd! (-> bytes? parameter/c integer?)]
 [store-dword/fwd! (-> bytes? parameter/c integer? void)]
 [load-dword/fwd! (-> bytes? parameter/c integer?)]
 [store-word/fwd! (-> bytes? parameter/c integer? void)]
 [load-word/fwd! (-> bytes? parameter/c integer?)]
 [store-byte/fwd! (-> bytes? parameter/c integer? void)]
 [load-byte/fwd! (-> bytes? parameter/c integer?)])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-memory size)
  (make-bytes size 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load / Store
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (store dest idx v sz)
  (define data (integer->integer-bytes v sz #t))
  (bytes-copy! dest idx data))

(define (load dest idx sz)
  (define data (subbytes dest idx (+ idx sz)))
  (integer-bytes->integer data #t))

(define (make-size-store-fn sz)
  (curryr store sz))

(define (make-size-load-fn sz)
  (curryr load sz))

;; qword
(define store-qword (make-size-store-fn 8))
(define load-qword (make-size-load-fn 8))

;; dword
(define store-dword (make-size-store-fn 4))
(define load-dword (make-size-load-fn 4))

;; word
(define store-word (make-size-store-fn 2))
(define load-word (make-size-load-fn 2))

;; byte
(define (store-byte dest idx v)
  (bytes-set! dest idx v))

(define (load-byte dest idx)
  (bytes-ref dest idx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load / Store with automatic FWD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fwd! ptr sz)
  (ptr (+ (ptr) sz)))

(define (store/fwd! dest ptr v sz)
  (define data (integer->integer-bytes v sz #t))
  (bytes-copy! dest (ptr) data)
  (fwd! ptr sz))

(define (load/fwd! dest ptr sz)
  (define data (subbytes dest (ptr) (+ (ptr) sz)))
  (define b (integer-bytes->integer data #t))
  (fwd! ptr sz)
  b)

(define (make-size-store/fwd!-fn sz)
  (curryr store/fwd! sz))

(define (make-size-load/fwd!-fn sz)
  (curryr load/fwd! sz))

;; qword
(define store-qword/fwd! (make-size-store/fwd!-fn 8))
(define load-qword/fwd! (make-size-load/fwd!-fn 8))

;; dword
(define store-dword/fwd! (make-size-store/fwd!-fn 4))
(define load-dword/fwd! (make-size-load/fwd!-fn 4))

;; word
(define store-word/fwd! (make-size-store/fwd!-fn 2))
(define load-word/fwd! (make-size-load/fwd!-fn 2))

;; byte
(define (store-byte/fwd! dest ptr v)
  (bytes-set! dest (ptr) v)
  (fwd! ptr 1))

(define (load-byte/fwd! dest ptr)
  (define b (bytes-ref dest (ptr)))
  (fwd! ptr 1)
  b)

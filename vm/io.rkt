#lang racket/base

(require racket/contract/base
         racket/function)

(define (index? x)
  (and (integer? x) (or (zero? x) (positive? x))))

(provide/contract
 [outl   (-> bytes? index? integer? void)]
 [inl    (-> bytes? index? integer?)]
 [outd   (-> bytes? index? integer? void)]
 [ind    (-> bytes? index? integer?)]
 [outw    (-> bytes? index? integer? void)]
 [inw     (-> bytes? index? integer?)]
 [outb    (-> bytes? index? integer? void)]
 [inb     (-> bytes? index? integer?)]
 [outl/fwd! (-> bytes? parameter/c integer? void)]
 [inl/fwd! (-> bytes? parameter/c integer?)]
 [outd/fwd! (-> bytes? parameter/c integer? void)]
 [ind/fwd! (-> bytes? parameter/c integer?)]
 [outw/fwd! (-> bytes? parameter/c integer? void)]
 [inw/fwd! (-> bytes? parameter/c integer?)]
 [outb/fwd! (-> bytes? parameter/c integer? void)]
 [inb/fwd! (-> bytes? parameter/c integer?)])


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
(define outl (make-size-store-fn 8))
(define inl (make-size-load-fn 8))

;; dword
(define outd (make-size-store-fn 4))
(define ind (make-size-load-fn 4))

;; word
(define outw (make-size-store-fn 2))
(define inw (make-size-load-fn 2))

;; byte
(define (outb dest idx v)
  (bytes-set! dest idx v))

(define (inb dest idx)
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
(define outl/fwd! (make-size-store/fwd!-fn 8))
(define inl/fwd! (make-size-load/fwd!-fn 8))

;; dword
(define outd/fwd! (make-size-store/fwd!-fn 4))
(define ind/fwd! (make-size-load/fwd!-fn 4))

;; word
(define outw/fwd! (make-size-store/fwd!-fn 2))
(define inw/fwd! (make-size-load/fwd!-fn 2))

;; byte
(define (outb/fwd! dest ptr v)
  (bytes-set! dest (ptr) v)
  (fwd! ptr 1))

(define (inb/fwd! dest ptr)
  (define b (bytes-ref dest (ptr)))
  (fwd! ptr 1)
  b)

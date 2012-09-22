#lang racket

(require rnrs/bytevectors-6)

(define (index? x)
  (and (integer? x) (or (zero? x) (positive? x))))

(provide/contract
 [create-memory (-> (and integer? positive?) void)]
 [store-qword   (-> bytevector? index? integer? void)]
 [load-qword    (-> bytevector? index? integer?)]
 [store-dword   (-> bytevector? index? integer? void)]
 [load-dword    (-> bytevector? index? integer?)]
 [store-word    (-> bytevector? index? integer? void)]
 [load-word     (-> bytevector? index? integer?)]
 [store-byte    (-> bytevector? index? integer? void)]
 [load-byte     (-> bytevector? index? integer?)])

;; create-memory
(define (create-memory size)
  (make-bytevector size 0))

;; qword
(define store-qword bytevector-u64-native-set!)
(define load-qword bytevector-u64-native-ref)

;; dword
(define store-dword bytevector-u32-native-set!)
(define load-dword bytevector-u32-native-ref)

;; word
(define store-word bytevector-u16-native-set!)
(define load-word bytevector-u16-native-ref)

;; byte
(define store-byte bytevector-u8-set!)
(define load-byte bytevector-u8-ref)
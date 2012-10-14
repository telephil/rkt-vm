;; Bit manipulation functions
#lang racket

(provide/contract
 [untag (integer? integer? . -> . integer?)]
 [clear-tags (integer? . -> . integer?)])

;; integer? integer? -> integer?
;; clear bit in number n
(define (untag bit n)
  (if (bitwise-bit-set? n bit)
   (bitwise-and n (bitwise-not (arithmetic-shift 1 bit)))
   n))

;; integer? -> integer?
;; clear bits 15 to 12 in number n
(define (clear-tags n)
  (foldl untag n '(15 14 13 12)))

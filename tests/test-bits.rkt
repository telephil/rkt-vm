#lang racket/base

(require rackunit
	 "../utils/bits.rkt")

;; tag
(check-= (tag 0 0) 1 0)
(check-= (tag 1 1) 3 0)

;; untag
(check-= (untag 0 0) 0 0)
(check-= (untag 1 3) 1 0)
(check-= (untag 16 (tag 16 42)) 42 0)

;; clear-tags
(check-= (clear-tags 42) 42 0)
(check-= (clear-tags (tag 15 1)) 1 0)
(check-= (clear-tags (tag 14 1)) 1 0)
(check-= (clear-tags (tag 13 1)) 1 0)
(check-= (clear-tags (tag 12 1)) 1 0)


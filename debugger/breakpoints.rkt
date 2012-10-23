;; Breakpoints management
#lang racket/base

(require racket/contract/base)

(provide/contract
 [list-breakpoints    (-> list?)]
 [add-breakpoint      (integer? . -> . void)]
 [remove-breakpoint   (integer? . -> . void)]
 [enable-breakpoint   (integer? . -> . void)]
 [enable-breakpoints  (-> void)]
 [disable-breakpoint  (integer? . -> . void)]
 [disable-breakpoints (-> void)]
 [breakpoint-at?      (integer? . -> . boolean?)])

;; Map of address -> state (enabled if #t, disabled if #f)
(define breakpoints (make-hash))

(define (list-breakpoints)
  (sort (for/list ([(k v) (in-hash breakpoints)])
          (cons k v)) < #:key car))

;; Add a breakpoint for the given address
(define (add-breakpoint addr)
  (hash-set! breakpoints addr #t))

;; Remove a breakpoint at the given address
(define (remove-breakpoint addr)
  (hash-remove! breakpoints addr))

;; Enable breakpoint for given address
(define (enable-breakpoint addr)
  (add-breakpoint addr))

;; Enable all breakpoints
(define (enable-breakpoints)
  (for-each enable-breakpoint (hash-keys breakpoints)))

;; Disable breakpoint for given address
(define (disable-breakpoint addr)
  (hash-set! breakpoints addr #f))

;; Disable all breakpoints
(define (disable-breakpoints)
  (for-each disable-breakpoint (hash-keys breakpoints)))

;; Check whether execution should break at given addr
;; that is: a breakpoint exists and is enabled
(define (breakpoint-at? addr)
  (hash-ref breakpoints addr #f))

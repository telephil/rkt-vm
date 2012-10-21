#lang racket/base

(require racket/contract/base)

;; ptr : (getter procedure?) (setter procedure?)
(struct ptr (getter setter)
        #:mutable
        #:methods gen:custom-write
        [(define write-proc (lambda (p port mode)
                              (fprintf port "#<ptr: ~a>" (p))))]
        #:property prop:procedure
        (case-lambda
          [(p) ((ptr-getter p))]
          [(p v) ((ptr-setter p) v)]))

;; make-collection-ptr : any/c integer? procedure? procedure? -> ptr?
(define (make-collection-ptr coll index coll-ref coll-set!)
  (ptr (lambda () (coll-ref coll index))
       (lambda (v) (coll-set! coll index v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide/contract
 (struct ptr [(getter procedure?)
              (setter procedure?)])
 [make-collection-ptr (any/c integer? procedure? procedure? . -> . ptr?)])

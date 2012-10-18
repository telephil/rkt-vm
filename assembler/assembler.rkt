#lang racket

(require "asm-parser.rkt"
         "syntax.rkt"
         "../vm/registers.rkt"
         "../vm/opcodes.rkt")

(provide/contract
 [compile-string (string? . -> . bytes?)]
 [compile-file (string? . -> . bytes?)])

(define (arg-size stx)
  (cond
   [(register-stx? stx) 1]
   [(number-stx? stx) 8]
   [(label-stx? stx) 8]
   [else 0]))

(define (compute-offsets src)
  (define off 0)
  (list->vector
   (filter-not false?
	       (cons 0
		     (for/list ([insn src])
		       (cond
			([label-stx? insn] #f) ;; do not include labels
			([insn-stx? insn]
			 (define sz (+ 4 ;; = opcode size = 32 bits
				       (arg-size (insn-stx-arg1 insn))
				       (arg-size (insn-stx-arg2 insn))))
			 (set! off (+ off sz))
			 off)))))))

(define (compute-label-addresses src)
  (define offsets (compute-offsets src))
  (letrec ([iter (λ (src idx h)
                   (cond
                     ([null? src] h)
                     (else
                      (define insn (car src))
                      (if (label-stx? insn)
			  (begin
			    (hash-set! h (label-stx-name insn) (vector-ref offsets idx))
			    (iter (cdr src) idx h))
			  (iter (cdr src) (add1 idx) h)))))])
    (iter src 0 (make-hash))))

(define (integer->s16bytes n)
  (integer->integer-bytes n 2 #t (system-big-endian?)))

(define (integer->s32bytes n)
  (integer->integer-bytes n 4 #t (system-big-endian?)))

(define (integer->s64bytes n)
  (integer->integer-bytes n 8 #t (system-big-endian?)))

(define (tag n bit)
  (bitwise-ior n (arithmetic-shift 1 bit)))

(define (encode-register stx)
  (bytes (register->bytecode (register-stx-name stx))))

(define (encode-arg arg labels)
  (cond
    [(register-stx? arg)
     (encode-register arg)]
    [(number-stx? arg)
     (integer->s64bytes (number-stx-value arg))]
    [(label-stx? arg)
     (define addr (hash-ref labels (label-stx-name arg)))
     (integer->s64bytes addr)]))

(define (tag-arg opcode arg bit)
  (cond
    [(register-stx? arg)
     (tag opcode bit)]
    [(number-stx? arg)
     (tag opcode (sub1 bit))]
    [(label-stx? arg)
     (tag opcode (sub1 bit))]))

(define (encode-opcode opcode arg1 arg2)
  (when arg1 (set! opcode (tag-arg opcode arg1 15)))
  (when arg2 (set! opcode (tag-arg opcode arg2 13)))
  opcode)

(define (encode-insn stx labels)
  (define opcode (opcode->bytecode (opcode-stx-name (insn-stx-op stx))))  
  (define arg1 (insn-stx-arg1 stx))
  (define encoded-arg1 (when arg1 (encode-arg arg1 labels)))  
  (define arg2 (insn-stx-arg2 stx))
  (define encoded-arg2 (when arg2 (encode-arg arg2 labels)))
  ;; Write encoded opcode and args
  (write-bytes (integer->s32bytes (encode-opcode opcode arg1 arg2)))
  (unless (void? encoded-arg1)
    (write-bytes encoded-arg1))
  (unless (void? encoded-arg2)
    (write-bytes encoded-arg2)))

(define (encode stx labels)
  (when (insn-stx? stx)
    (encode-insn stx labels)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compile given instruction list coming from the parser
;; <private>
(define (compile-insn-list src)
  (define labels (compute-label-addresses src))
  (with-output-to-bytes
   (λ ()
     (write-bytes (string->bytes/utf-8 "VMBC"))
     (write-bytes (integer->s16bytes (hash-ref labels "start" 0)))
     (for-each (λ (stx)
		  (encode stx labels)) src))))


;; Compile from input port
(define (compile name ip)
  (compile-insn-list (parse name ip)))

;; Compile given string
(define (compile-string str)
  (compile-insn-list (parse-string str)))

;; Compile given file
(define (compile-file filename)
  (compile-insn-list (parse-file filename)))

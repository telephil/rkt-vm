#lang racket

(require "asm-parser.rkt"
         "syntax.rkt"
         "registers.rkt"
         "opcodes.rkt")

(provide/contract
 [compile-string (string? . -> . bytes?)]
 [compile-file (string? . -> . bytes?)])

(define (compute-label-addresses src)
  (letrec ([iter (λ (src idx h)
                   (cond
                     ([null? src] h)
                     (else
                      (define insn (car src))
                      (when (label-stx? insn)
                        (hash-set! h (label-stx-name insn) idx))
                      (iter (cdr src) (add1 idx) h))))])
    (iter src 0 (make-hash))))

(define (integer->s16bytes n)
  (integer->integer-bytes n 2 #t (system-big-endian?)))

(define (integer->s64bytes n)
  (integer->integer-bytes n 8 #t (system-big-endian?)))

(define (tag n bit)
  (bitwise-ior n (arithmetic-shift 1 bit)))

(define (encode-register stx)
  (bytes (register->bytecode (register-stx-name stx))))

(define (encode-arg opcode arg bit labels)
  (cond
    [(register-stx? arg) (tag opcode bit)
     (encode-register arg)]
    [(number-stx? arg) (tag opcode (sub1 bit))
     (integer->s64bytes (number-stx-value arg))]
    [(label-stx? arg) (tag opcode (sub1 bit))
     (define addr (hash-ref labels (label-stx-name arg)))
     (integer->s64bytes addr)]))

(define (encode-insn stx labels)
  (define opcode (opcode->bytecode (opcode-stx-name (insn-stx-op stx))))  
  (define arg1 (insn-stx-arg1 stx))
  (define encoded-arg1 (when arg1 (encode-arg opcode arg1 15 labels)))  
  (define arg2 (insn-stx-arg2 stx))
  (define encoded-arg2 (when arg2 (encode-arg opcode arg2 13 labels)))
  ;; Write encoded opcode and args
  (write-bytes (integer->s16bytes opcode))
  (unless (void? encoded-arg1)
    (write-bytes encoded-arg1))
  (unless (void? encoded-arg2)
    (write-bytes encoded-arg2)))

(define (encode stx labels)
  (cond
    ([label-stx? stx] 
     (write-bytes (integer->s16bytes (opcode->bytecode 'nop))))
    ([insn-stx? stx] (encode-insn stx labels))))

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
     (write-bytes (bytes (hash-ref labels "start" 0)))
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

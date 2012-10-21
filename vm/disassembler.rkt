;; Bytecode disassembler
#lang racket/base

(require racket/contract/base
         racket/match
         srfi/13 ;; for string-pad
         "registers.rkt"
         "memory.rkt"
         "opcodes.rkt"
         "../utils/bits.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide/contract
 [disassemble (integer? bytes? parameter/c . -> . (or/c string? #f))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (addr->string addr)
  (string-pad (number->string addr 16) 8 #\0))

(define (op->string op)
  (string-pad-right (opcode->string op) 6))

(define (reg->string reg)
  (string-pad-right (bytecode->register reg) 3))

(define (num->string num)
  ;(string-pad (number->string num 16) 16 #\0)
  (number->string num 16))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory access
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define BYTE  1)
(define WORD  2)
(define DWORD 4)
(define QWORD 8)

(define (ptr-add! ptr sz)
  (ptr (+ sz (ptr))))

(define (fetch-op mem ptr)
  (define op (load-dword mem (ptr)))
  (ptr-add! ptr DWORD)
  op)

(define (fetch-ptr mem ptr)
  (define reg (load-byte mem (ptr)))
  (ptr-add! ptr BYTE)
  (define off (load-qword mem (ptr)))
  (ptr-add! ptr QWORD)

  (format "~a(~a)"
          (if (= off 0) "" off)
          (bytecode->register reg)))

(define (fetch-reg mem ptr)
  (define bc (load-byte mem (ptr)))
  (ptr-add! ptr BYTE)
  (reg->string bc))

(define (fetch-number mem ptr)
  (define bc (load-qword mem (ptr)))
  (ptr-add! ptr QWORD)
  (num->string bc))

(define (fetch-arg op bit mem ptr)
  (define reg? (bitwise-bit-set? op bit))
  (define imm? (bitwise-bit-set? op (sub1 bit)))
  (cond
   [(and reg? imm?) (fetch-ptr mem ptr)]
   [reg? (fetch-reg mem ptr)]
   [imm? (fetch-number mem ptr)]
   [else #f]))

(define (fetch-insn mem ptr)
  (define op (fetch-op mem ptr))
  (define arg1 (fetch-arg op 15 mem ptr))
  (define arg2 (fetch-arg op 13 mem ptr))
  (list (clear-tags op) arg1 arg2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disassemble
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (disassemble start mem ptr)
  (define addr (ptr))
  (match-define (list op arg1 arg2) (fetch-insn mem ptr))
  (if (= op END) #f
      (let ([res (string-append (if (= addr start) "* " "  ")
                                (addr->string addr)
                                ": "
                                (op->string op)
                                " ")])
        (when arg1 (set! res (string-append res arg1)))
        (when arg2 (set! res (string-append res ", " arg2)))
        res)))

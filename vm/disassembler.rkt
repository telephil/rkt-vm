;; Bytecode disassembler
#lang racket/base

(require racket/contract/base
         racket/match
         srfi/13 ;; for string-pad
         "fetch.rkt"
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
  (opcode->string op))

(define (reg->string reg)
  (bytecode->register reg))

(define (num->string num)
  ;(string-pad (number->string num 16) 16 #\0)
  (number->string num 16))

(define (ptr->string reg off)
   (format "~a(~a)"
           (if (= off 0) "" off)
           (bytecode->register reg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction fetching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fetch-instruction mem ptr)
  (parameterize ([register-bytecode-transformer reg->string]
                 [number-bytecode-transformer num->string]
                 [ptr-bytecode-transformer ptr->string])
    (fetch-insn mem ptr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disassemble
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (disassemble start mem ptr)
  (define addr (ptr))
  (match-define (list op arg1 arg2) (fetch-instruction mem ptr))
  (if (= op END) #f
      (let ([res (string-append (if (= addr start) "* " "  ")
                                (addr->string addr)
                                ":  "
                                (op->string op)
                                " ")])
        (when arg1 (set! res (string-append res "\t" arg1)))
        (when arg2 (set! res (string-append res ",\t" arg2)))
        res)))

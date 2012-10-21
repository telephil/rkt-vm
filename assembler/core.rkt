#lang racket/base

(require racket/contract/base
         racket/bool
         racket/list
         racket/port
         "parser.rkt"
         "syntax.rkt"
         "../vm/registers.rkt"
         "../vm/opcodes.rkt"
         "../utils/bits.rkt")

(provide/contract
 [compile-string (string? . -> . bytes?)]
 [compile-file (string? . -> . bytes?)])

;; arg-size :
;;  (or/c insn-stx? register-stx? number-stx? label-stx?) -> integer?
(define (arg-size stx)
  (cond
   [(insn-stx? stx) (+ 4 ;; = opcode size = 32 bits
                       (arg-size (insn-stx-arg1 stx))
                       (arg-size (insn-stx-arg2 stx)))]
   [(register-stx? stx) 1]
   [(number-stx? stx) 8]
   [(label-stx? stx) 8]
   [else 0]))

;; compute-label-addresses :
;;  (listof (or/c label-stx? insn-stx?)) -> (hash string? integer?)
(define (compute-label-addresses src)
  (letrec ([loop
            (lambda (src acc offsets)
              (if (null? src)
                  (make-hash offsets)
                  (match-let ([(list insn insns ...) src])
                    (cond
                     [(insn-stx? insn)
                      (define s (+ acc (arg-size insn)))
                      (loop insns s offsets)]
                     [(label-stx? insn)
                      (loop insns acc (cons (cons (label-stx-name insn) acc)
                                            offsets))]))))])
    (loop src 0 '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integer conversion functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; integer->s16bytes : integer? -> bytes?
(define (integer->s16bytes n)
  (integer->integer-bytes n 2 #t (system-big-endian?)))

;; integer->s32bytes : integer? -> bytes?
(define (integer->s32bytes n)
  (integer->integer-bytes n 4 #t (system-big-endian?)))

;; integer->s64bytes : integer? -> bytes?
(define (integer->s64bytes n)
  (integer->integer-bytes n 8 #t (system-big-endian?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; encode-register : register-stx? -> bytes?
(define (encode-register stx)
  (bytes (register->bytecode (register-stx-name stx))))

;; encode-arg : (or/c register-stx? number-stx? label-stx?) -> bytes?
(define (encode-arg arg labels)
  (cond
    [(register-stx? arg)
     (encode-register arg)]
    [(number-stx? arg)
     (integer->s64bytes (number-stx-value arg))]
    [(label-stx? arg)
     (define addr (hash-ref labels (label-stx-name arg)))
     (integer->s64bytes addr)]))

;; tag-arg : integer? (or/c register-stx? number-stx? label-stx?) -> integer?
(define (tag-arg opcode arg bit)
  (cond
    [(register-stx? arg)
     (tag bit opcode)]
    [(number-stx? arg)
     (tag (sub1 bit) opcode)]
    [(label-stx? arg)
     (tag (sub1 bit) opcode)]))

;; encode-opcode : integer? (or/c stx? #f) (or/c stx #f) -> integer?
(define (encode-opcode opcode arg1 arg2)
  (when arg1 (set! opcode (tag-arg opcode arg1 15)))
  (when arg2 (set! opcode (tag-arg opcode arg2 13)))
  opcode)

;; encode-insn : insn-stx? (hash string? integer?) -> bytes?
(define (encode-insn stx labels)
  (define opcode (opcode->bytecode (opcode-stx-name (insn-stx-op stx))))
  (define arg1 (insn-stx-arg1 stx))
  (define encoded-arg1 (if arg1 (encode-arg arg1 labels) #f))
  (define arg2 (insn-stx-arg2 stx))
  (define encoded-arg2 (if arg2 (encode-arg arg2 labels) #f))
  ;; Write encoded opcode and args
  (write-bytes (integer->s32bytes (encode-opcode opcode arg1 arg2)))
  (when encoded-arg1
    (write-bytes encoded-arg1))
  (when encoded-arg2
    (write-bytes encoded-arg2)))

;; encode : stx? (hash string? integer?)
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

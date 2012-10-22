#lang racket/base

(require racket/contract/base
         racket/match
         racket/function
         "memory.rkt"
         "registers.rkt"
         "opcodes.rkt"
         "fetch.rkt"
         "../utils/ptr.rkt"
         "../utils/bits.rkt")

(struct vm (memory registers flags))

(provide/contract
 [vm? (any/c . -> . boolean?)]
 [current-vm (parameter/c vm?)]
 [init (integer? . -> . void)]
 [load (string? . -> . void)]
 [run (-> void)]
 [step (bytes? (parameter/c integer?) . -> . void)]
 [print-registers (integer? . -> . void)]
 [print-register (integer? integer? . -> . void)])

(provide vm-memory get-vm-register)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-vm (make-parameter null))
(define ip #f)
(define bp #f)
(define sp #f)
(define flags #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VM Definition & Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (init memsize)
  (current-vm
   (vm (create-memory memsize)
       (for/vector ([i (in-range (register-count))])
         (make-parameter 0))
       (make-parameter 0))))

(define (get-vm-register name)
  (vector-ref (vm-registers (current-vm)) (register->bytecode name)))

(define (initialize-registers start)
  (set! ip (get-vm-register 'ip))
  (set! sp (get-vm-register 'sp))
  (set! bp (get-vm-register 'bp))
  (set! flags (vm-flags (current-vm)))
  (ip start)
  (sp (bytes-length (vm-memory (current-vm))))
  (bp (sp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flags management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ZF_BIT 0)
(define SF_BIT 2)

(define (flag-set? bit)
  (bitwise-bit-set? (flags) bit))

(define (flag-clear! bit)
  (flags (untag bit (flags))))

(define (flag-set! bit predicate arg)
  (if (predicate (arg))
      (flags (tag bit (flags)))
      (flag-clear! bit)))

(define (zf?)
  (flag-set? ZF_BIT))

(define (zf! arg)
  (flag-set! ZF_BIT zero? arg))

(define (sf?)
  (flag-set? SF_BIT))

(define (sf! arg)
  (flag-set! SF_BIT negative? arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stack operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define QWORD 8)

(define (stack-push arg)
  (define mem (vm-memory (current-vm)))
  (define sp (get-vm-register 'sp))
  (define data (integer->integer-bytes arg QWORD #t))
  (sp (- (sp) QWORD))
  (bytes-copy! mem (sp) data))

(define (stack-pop)
  (define mem (vm-memory (current-vm)))
  (define sp (get-vm-register 'sp))
  (define data (subbytes mem (sp) (+ (sp) QWORD)))
  (define val (integer-bytes->integer data #t))
  (sp (+ (sp) QWORD))
  val)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction fetching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reg-trans reg)
  (vector-ref (vm-registers (current-vm)) reg))

(define (ptr-trans mem)
  (lambda (reg off)
    (make-collection-ptr mem (+ ((reg-trans reg)) off) load-qword store-qword)))

(define (fetch-instruction mem ptr)
  (parameterize ([register-bytecode-transformer reg-trans]
                 [number-bytecode-transformer make-parameter]
                 [ptr-bytecode-transformer (ptr-trans mem)])
    (fetch-insn mem ptr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (execute op arg1 arg2 mem ip)
  ;(printf "(debug) ~a ~a ~a (ip = ~a)~%" (opcode->symbol op) (if arg1 (arg1) #f) (if arg2 (arg2) #f) (ip))
  (case (opcode->symbol op)
   [(nop)  (values)]
   [(mov)  (arg1 (arg2))]
   [(and)  (arg1 (bitwise-and (arg1) (arg2)))]
   [(or)   (arg1 (bitwise-ior (arg1) (arg2)))]
   [(xor)  (arg1 (bitwise-xor (arg1) (arg2)))]
   [(not)  (arg1 (bitwise-not (arg1)))]
   [(shl)  (arg1 (arithmetic-shift (arg1) (arg2)))]
   [(shr)  (arg1 (arithmetic-shift (arg1 (- (arg2)))))]
   [(add)  (arg1 (+ (arg1) (arg2)))]
   [(sub)  (arg1 (- (arg1) (arg2)))]
   [(mul)  (arg1 (* (arg1) (arg2)))]
   [(div)  (arg1 (/ (arg1) (arg2)))]
   [(inc)  (arg1 (add1 (arg1)))]
   [(dec)  (arg1 (sub1 (arg1))) (zf! arg1)]
   [(cmp)  (let ([res (make-parameter (- (arg1) (arg2)))])
             (zf! res)
             (sf! res))]
   [(jmp)  (ip (arg1))]
   [(jz)   (when (zf?)   (ip (arg1)))]
   [(jnz)  (unless (zf?) (ip (arg1)))]
   [(je)   (when (zf?)   (ip (arg1)))]
   [(jne)  (unless (zf?) (ip (arg1)))]
   [(jg)   (unless (sf?) (ip (arg1)))]
   [(jge)  (when (or (zf?) (not (sf?)))
             (ip (arg1)))]
   [(jl)   (when (sf?) (ip (arg1)))]
   [(jle)  (when (or (zf?) (sf?))
             (ip (arg1)))]
   [(push) (stack-push (arg1))]
   [(pop)  (arg1 (stack-pop))]
   [(call) (stack-push (ip)) (ip (arg1))]
   [(ret)  (ip (stack-pop))]
   [(end)  (values)]))

(define (step mem ip)
  (match-define (list op arg1 arg2)
                (fetch-instruction mem ip))
  (execute op arg1 arg2 mem ip)
  op)

(define (run)
  (define mem (vm-memory (current-vm)))
  (letrec ([iter (lambda ()
                   (define result (step mem ip))
                   (unless (= result END)
                     (iter)))])
    (iter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (read-and-check-header)
  (define header (read-bytes 4))
  (unless (and (bytes? header) (bytes=? header #"VMBC"))
    (raise-user-error "Invalid VM bytecode file"))
  header)

(define (read-start-label-address)
  (define b (read-bytes 2))
  (integer-bytes->integer b 2))

(define (read-loop loop mem idx)
  (define b (read-byte))
  (cond
   [(eof-object? b) (store-byte mem idx END)]
   [else
    (store-byte mem idx b)
    (loop loop mem (add1 idx))]))

(define (read-program)
  (define mem (vm-memory (current-vm)))
  (define header (read-and-check-header))
  (define start (read-start-label-address))
  (letrec ([loop read-loop]) (loop loop mem 0))
  (initialize-registers start))

;; Load a compiled program from filename
(define (load filename)
  (with-input-from-file filename read-program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (print-register bc radix [out (current-output-port)])
  (define reg (vector-ref (vm-registers (current-vm)) bc))
  (fprintf out "~a = ~a~%" (bytecode->register bc) (number->string (reg) radix)))

(define (print-registers [radix 10] [out (current-output-port)])
  (do ((i 0 (add1 i)))
      ((= i (register-count)))
    (print-register i radix out)))

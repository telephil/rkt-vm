#lang racket

(require "bits.rkt"
	 "memory.rkt"
	 "registers.rkt"
	 "opcodes.rkt")

(struct vm (memory registers flags))

(provide/contract
 [vm? (any/c . -> . boolean?)]
 [current-vm (parameter/c vm?)]
 [create-vm (integer? . -> . void)]
 [load-file (string? . -> . void)]
 [run (-> void)]
 [step (bytes? (parameter/c integer?) . -> . void)]
 [dump-registers (-> void)])

(provide vm-memory get-vm-register)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-vm (make-parameter null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VM Definition & Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (create-vm memsize)
  (current-vm 
   (vm (create-memory memsize)
       (for/vector ([i (in-range (register-count))])
         (make-parameter 0))
       (make-parameter 0))))

(define (get-vm-register name)
  (vector-ref (vm-registers (current-vm)) (register->bytecode name)))

(define (initialize-registers start)
  (define ip (get-vm-register 'ip))
  (define sp (get-vm-register 'sp))
  (define bp (get-vm-register 'bp))
  (ip start)
  (sp (bytes-length (vm-memory (current-vm))))
  (bp (sp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flags management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ZF_BIT 0)
(define SF_BIT 2)

(define (flag-set? bit)
  (define flags (vm-flags (current-vm)))
  (bitwise-bit-set? (flags) bit))

(define (flag-clear! bit)
  (define flags (vm-flags (current-vm)))
  (when (flag-set? bit)
    (flags (bitwise-and (flags) (bitwise-not (arithmetic-shift 1 bit))))))

(define (flag-set! bit predicate arg)
  (define flags (vm-flags (current-vm)))
  (if (predicate (arg))
      (flags (bitwise-ior (flags) (arithmetic-shift 1 bit)))
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

(define (fetch-reg mem ptr)
  (define bc (load-byte mem (ptr)))
  (ptr-add! ptr BYTE)
  (vector-ref (vm-registers (current-vm)) bc))

(define (fetch-number mem ptr)
  (define bc (load-qword mem (ptr)))
  (ptr-add! ptr QWORD)
  (make-parameter bc))

(define (fetch-arg op bit mem ptr)
  (cond
   [(bitwise-bit-set? op bit) (fetch-reg mem ptr)]
   [(bitwise-bit-set? op (sub1 bit)) (fetch-number mem ptr)]
   [else #f]))

(define (fetch-insn mem ptr)
  (define op (fetch-op mem ptr))
  (define arg1 (fetch-arg op 15 mem ptr))
  (define arg2 (fetch-arg op 13 mem ptr))
  (list (clear-tags op) arg1 arg2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (execute op arg1 arg2 mem ip)
;  (printf "(debug) ~a ~a ~a (ip = ~a)~%" op (if arg1 (arg1) #f) (if arg2 (arg2) #f) (ip))
  (case op
   [(NOP)  (values)]
   [(MOV)  (arg1 (arg2))]
   [(AND)  (arg1 (bitwise-and (arg1) (arg2)))]
   [(OR)   (arg1 (bitwise-ior (arg1) (arg2)))]
   [(XOR)  (arg1 (bitwise-xor (arg1) (arg2)))]
   [(NOT)  (arg1 (bitwise-not (arg1)))]
   [(SHL)  (arg1 (arithmetic-shift (arg1) (arg2)))]
   [(SHR)  (arg1 (arithmetic-shift (arg1 (- (arg2)))))]
   [(ADD)  (arg1 (+ (arg1) (arg2)))]
   [(SUB)  (arg1 (- (arg1) (arg2)))]
   [(MUL)  (arg1 (* (arg1) (arg2)))]
   [(DIV)  (arg1 (/ (arg1) (arg2)))]
   [(INC)  (arg1 (add1 (arg1)))]
   [(DEC)  (arg1 (sub1 (arg1))) (zf! arg1)]
   [(CMP)  (let ([res (make-parameter (- (arg1) (arg2)))])
     	  (zf! res)
     	  (sf! res))]
   [(JMP)  (ip (arg1))]
   [(JZ)   (when (zf?)   (ip (arg1)))]
   [(JNZ)  (unless (zf?) (ip (arg1)))]
   [(JE)   (when (zf?)   (ip (arg1)))]
   [(JNE)  (unless (zf?) (ip (arg1)))]
   [(JG)   (unless (sf?) (ip (arg1)))]
   [(JGE)  (when (or (zf?) (not (sf?)))
     	  (ip (arg1)))]
   [(JL)   (when (sf?) (ip (arg1)))]
   [(JLE)  (when (or (zf?) (sf?))
     	  (ip (arg1)))]
   [(PUSH) (stack-push (arg1))]
   [(POP)  (arg1 (stack-pop))]
   [(CALL) (stack-push (ip)) (ip (arg1))]
   [(RET)  (ip (stack-pop))]
   [(END)  (values)]))

(define (step mem ip)
  (match-define (list op arg1 arg2)
		(fetch-insn mem ip))
  (execute op arg1 arg2 mem ip)
  op)

(define (run)
  (define ip (get-vm-register 'ip))
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
(define (load-file filename)
  (with-input-from-file filename read-program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (dump-register bc [out (current-output-port)])
  (define reg (vector-ref (vm-registers (current-vm)) bc))
  (fprintf out "~a = ~a~%" (bytecode->register bc) (reg)))

(define (dump-registers [out (current-output-port)])
  (do ((i 0 (add1 i)))
      ((= i (register-count)))
    (dump-register i out)))

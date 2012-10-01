#lang racket

(require "memory.rkt"
	 "registers.rkt"
	 "opcodes.rkt")

(struct vm (memory registers flags))

(provide/contract
 [current-vm (-> (parameter/c vm?))]
 [create-vm (integer? . -> . void)]
 [load-file (string? . -> . void)]
 [run (-> void)]
 [dump-registers (-> void)])

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
;; Misc Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (untag n bit)
  (if (bitwise-bit-set? n bit)
   (bitwise-and n (bitwise-not (arithmetic-shift 1 bit)))
   n))

;; TODO ugly!!!
(define (clear-tags op)
  (set! op (untag op 15))
  (set! op (untag op 14))
  (set! op (untag op 13))
  (set! op (untag op 12))
  op)

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
  (cond
   [(= op NOP)  #;noop ]
   [(= op MOV)  (arg1 (arg2))]
   [(= op AND)  (arg1 (bitwise-and (arg1) (arg2)))]
   [(= op OR)   (arg1 (bitwise-ior (arg1) (arg2)))]
   [(= op XOR)  (arg1 (bitwise-xor (arg1) (arg2)))]
   [(= op NOT)  (arg1 (bitwise-not (arg1)))]
   [(= op SHL)  (arg1 (arithmetic-shift (arg1) (arg2)))]
   [(= op SHR)  (arg1 (arithmetic-shift (arg1 (- (arg2)))))]
   [(= op ADD)  (arg1 (+ (arg1) (arg2)))]
   [(= op SUB)  (arg1 (- (arg1) (arg2)))]
   [(= op MUL)  (arg1 (* (arg1) (arg2)))]
   [(= op DIV)  (arg1 (/ (arg1) (arg2)))]
   [(= op INC)  (arg1 (add1 (arg1)))]
   [(= op DEC)  (arg1 (sub1 (arg1))) (zf! arg1)]
   [(= op CMP)  (let ([res (make-parameter (- (arg1) (arg2)))])
		  (zf! res)
		  (sf! res))]
   [(= op JMP)  (ip (arg1))]
   [(= op JZ)   (when (zf?)   (ip (arg1)))]
   [(= op JNZ)  (unless (zf?) (ip (arg1)))]
   [(= op JE)   (when (zf?)   (ip (arg1)))]
   [(= op JNE)  (unless (zf?) (ip (arg1)))]
   [(= op JG)   (unless (sf?) (ip (arg1)))]
   [(= op JGE)  (when (or (zf?) (not (sf?)))
		  (ip (arg1)))]
   [(= op JL)   (when (sf?) (ip (arg1)))]
   [(= op JLE)  (when (or (zf?) (sf?))
		  (ip (arg1)))]
   [(= op PUSH) (stack-push (arg1))]
   [(= op POP)  (arg1 (stack-pop))]
   [(= op CALL) (stack-push (ip)) (ip (arg1))]
   [(= op RET)  (ip (stack-pop))]
   [(= op END)  #;noop ]))

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

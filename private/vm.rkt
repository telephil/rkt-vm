#lang racket

(require "memory.rkt"
	 "registers.rkt"
	 "opcodes.rkt")

(struct vm (memory registers flags))

(provide/contract
 [current-vm (-> (parameter/c vm?))]
 [create-vm (integer? . -> . void)]
 [load-file (string? . -> . void)])

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flags management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (maybe-set-zf arg)
  (define flags (vm-flags (current-vm)))
  (when (zero? (arg))
    (flags (bitwise-ior (flags) 1))))

(define (zf?)
  (define flags (vm-flags (current-vm)))
  (bitwise-bit-set? (flags) 0))

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
(define (step mem ip)
  (match-define (list op arg1 arg2) (fetch-insn mem ip))
  (cond
   ;; NOP
   [(= op #x00) ]
   ;; MOV
   [(= op #x01) (arg1 (arg2))]
   ;; AND
   [(= op #x02) (arg1 (bitwise-and (arg1) (arg2)))]
   ;; OR
   [(= op #x03) (arg1 (bitwise-ior (arg1) (arg2)))]
   ;; XOR
   [(= op #x04) (arg1 (bitwise-xor (arg1) (arg2)))]
   ;; NOT
   [(= op #x05) (arg1 (bitwise-not (arg1)))]
   ;; SHL
   [(= op #x06) (arg1 (arithmetic-shift (arg1) (arg2)))]
   ;; SHR
   [(= op #x07) (arg1 (arithmetic-shift (arg1 (- (arg2)))))]
   ;; ADD
   [(= op #x08) (arg1 (+ (arg1) (arg2)))]
   ;; SUB
   [(= op #x09) (arg1 (- (arg1) (arg2)))]
   ;; MUL
   [(= op #x0A) (arg1 (* (arg1) (arg2)))]
   ;; DIV
   [(= op #x0B) (arg1 (/ (arg1) (arg2)))]
   ;; INC
   [(= op #x0C) (arg1 (add1 (arg1)))]
   ;; DEC
   [(= op #x0D) (arg1 (sub1 (arg1)))
    (maybe-set-zf arg1)]
   ;; JMP
   [(= op #x0E) (ip (arg1))]
   ;; JZ
   [(= op #x0F) (when (zf?)
                  (ip (arg1)))]
   ;; JNZ
   [(= op #x10) (unless (zf?)
		  (ip (arg1)))]
   ;; JE
   [(= op #x11) ]
   ;; JNE
   [(= op #x12) ]
   ;; JG
   [(= op #x13) ]
   ;; JGE
   [(= op #x14) ]
   ;; JL
   [(= op #x15) ]
   ;; JLE
   [(= op #x16) ]
   ;; PUSH
   [(= op #x17) ]
   ;; POP
   [(= op #x18) ]
   ;; END
   [(= op #xff)])
  op)

(define (run)
  (define ip (get-vm-register 'ip))
  (define mem (vm-memory (current-vm)))

  (letrec ([iter (lambda ()
		   (define result (step mem ip))
		   (unless (= result #xFF)
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
   [(eof-object? b) (store-byte mem idx #xff)]
   [else
    (store-byte mem idx b)
    (loop loop mem (add1 idx))]))

(define (read-program)
  (define mem (vm-memory (current-vm)))
  (define header (read-and-check-header))
  (define start (read-start-label-address))
  (letrec ([loop read-loop]) (loop loop mem 0)))

;; Load a compiled program from filename
(define (load-file filename)
  (with-input-from-file filename read-program))

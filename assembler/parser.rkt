#lang racket/base

(require racket/contract/base
	 racket/bool
	 racket/port
         parser-tools/yacc
         parser-tools/lex
	 "lexer.rkt"
	 "syntax.rkt"
         "../vm/opcodes.rkt")

;; Contract for parser result
(define insn-list/c (listof (or/c label-stx? insn-stx?)))

;; Module exports
(provide/contract
 [parse (-> input-port? string? insn-list/c)]
 [parse-string (-> string? insn-list/c)]
 [parse-file (-> string? insn-list/c)])

;; instruction syntax construction
(define (raise-invalid-argcount opcode expected received source-name start)
  (raise-user-error
   (format 
    "~a: parse error at line ~a column ~a : ~a opcode expects ~a arguments but received ~a."
    source-name
    (position-line start)
    (position-col start)
    opcode
    expected
    received)))

(define (raise-invalid-opcode opcode source-name start)
  (raise-user-error
   (format 
    "~a: parse error at line ~a column ~a : invalid opcode ~a."
    source-name
    (position-line start)
    (position-col start)
    opcode)))


(define (check-opcode-stx opcode argcount source-name start)
  (define argc (opcode-argcount opcode)) 
  (cond
    [(false? argc)
     (raise-invalid-opcode opcode source-name start)]
    [(not (= argc argcount))
     (raise-invalid-argcount opcode argc argcount source-name start)])) 

  
;; TODO : implement args checking

;; Parser
(define (asm-parser source-name)
  (parser
   (src-pos)
   
   (start s)
   (end   EOF)
   (tokens data delim)
  
   (error (lambda (a name val start end)
            (raise-user-error
             (format 
              "~a: parse error at line ~a column ~a : unexpected token ~a."
             source-name
             (position-line start)
             (position-col start)
             name))))

   (grammar
    (s
     [(insn-list) (reverse $1)])
    
    (insn [(LABEL) (label-stx $1)]          
          [(ID NEWLINE)
           (begin
             (check-opcode-stx $1 0 source-name $1-start-pos)
             (insn-stx (opcode-stx $1) #f #f))]
          [(ID ID NEWLINE) ;; match OPCODE LABEL-REF
           (begin
             (check-opcode-stx $1 1 source-name $1-start-pos)
             (insn-stx (opcode-stx $1) (label-stx (symbol->string $2)) #f))]
          [(ID REGISTER NEWLINE)
           (begin
             (check-opcode-stx $1 1 source-name $1-start-pos)
             (insn-stx (opcode-stx $1) (register-stx $2) #f))]
          [(ID REGISTER COMMA NUMBER NEWLINE) 
           (begin
             (check-opcode-stx $1 2 source-name $1-start-pos)
             (insn-stx (opcode-stx $1)
                       (register-stx $2)
                       (number-stx $4)))]
          [(ID REGISTER COMMA REGISTER NEWLINE)
           (begin
             (check-opcode-stx $1 2 source-name $1-start-pos)
             (insn-stx (opcode-stx $1)
                       (register-stx $2)
                       (register-stx $4)))])
    
    (insn-list [() null]
               [(insn-list insn) (cons $2 $1)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public parser API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parse from an input port
(define (parse name ip)
  (port-count-lines! ip)
  ((asm-parser name)
   (lambda ()
     (asm-lexer ip))))

;; Parse given string
(define (parse-string str)
  (call-with-input-string str                          
    (lambda (ip) (parse "<unknown>" ip))))

;; Parse from file
(define (parse-file filename)
  (call-with-input-file filename 
    (lambda (ip) (parse filename ip))))

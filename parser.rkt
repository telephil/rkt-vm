#lang racket

(require "insn.rkt"
         parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide/contract
 [parse (-> input-port? string? (listof (or/c label? insn?)))]
 [parse-string (-> string? (listof (or/c label? insn?)))]
 [parse-file (-> string? (listof (or/c label? insn?)))])

;; Utils
(define (bin-string->number str)
  (string->number
   (substring str 0 (- (string-length str) 1))
   2))

;; Lexer tokens definition
(define-tokens       data  (REGISTER ID LABEL NUMBER))
(define-empty-tokens delim (COMMA EOF))

;; Lexer
(define asm-lexer
  (lexer-src-pos
   ;; EOF
   [(eof) 'EOF]
   
   ;; Skip whitespaces
   [(:or #\tab #\space #\newline) (return-without-pos (asm-lexer input-port))]
   
   ;; Comma
   [#\, 'COMMA]

   ;; Register
   [(:or (:: (:or #\r #\R) (:/ "0" "8"))
         (:: (:or #\s #\S #\b #\B #\i #\I) (:or #\p #\P)))
    (token-REGISTER (string->symbol (string-downcase lexeme)))]
      
   ;; ID
   [(:: alphabetic (:+ (:or alphabetic numeric)))
    (token-ID (string->symbol lexeme))]

   ;; LABEL
   [(:: alphabetic (:* (:or alphabetic numeric #\$ #\_ #\.)) ":")
    (token-LABEL (substring lexeme 0 (- (string-length lexeme) 1)))]
   
   ;; NUMBER
   ;;; - Binary
   [(:: (:+ (:or "0" "1")) "b")
    (token-NUMBER (bin-string->number lexeme))]
   ;;; - Decimal
   [(:+ numeric)
    (token-NUMBER (string->number lexeme))]
   ;;; - Hexadecimal
   [(:: "0x" (:+ (:or (:/ "0" "9") (:/ "a" "f") (:/ "A" "F"))))
    (token-NUMBER (string->number (substring lexeme 2) 16))]))

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
             val))))

   (grammar
    (s
     [(insn-list) (reverse $1)])
    
    (insn 
     [(LABEL) (label $1)]
     [(ID REGISTER) (insn $1 (list $2))]
     [(ID REGISTER COMMA NUMBER) (insn $1 (list $2 $4))]
     [(ID REGISTER COMMA REGISTER) (insn $1 (list $2 $4))])
    
    (insn-list
     [() null]
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

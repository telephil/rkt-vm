#lang racket

(require "insn.rkt"
         parser-tools/yacc
         parser-tools/lex
         syntax/readerr
         (prefix-in : parser-tools/lex-sre))

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
  (lexer ;-src-pos
   [(eof) 'EOF]
   
   ;; Skip whitespaces
   [(:or #\tab #\space #\newline) (asm-lexer input-port)]
   
   ;; Comma
   ["," (token-COMMA)]

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
   [(:+ numeric) (token-NUMBER (string->number lexeme))]
   ;;; - Hexadecimal
   [(:: "0x" (:+ (:or (:/ "0" "9") (:/ "a" "f") (:/ "A" "F"))))
    (token-NUMBER (string->number (substring lexeme 2) 16))]))

;; Parser
(define asm-parser
  (parser
   ;(src-pos)
   
   (start s)
   (end   EOF)
   (tokens data delim)
  
   (error (lambda (a b c)
            (printf "ERROR: ~a - ~a~%" b c)))
   #|
   (error (lambda (a name val start end)
            (raise-read-error 
             "read-error"
             ""
             (position-line start)
             (position-col start)
             (position-offset start)
             (- (position-offset end)
                (position-offset start)))))
|#
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

(define (parse ip)
  (port-count-lines! ip)
  (asm-parser
   (lambda ()
     (asm-lexer ip))))

(define (parse-string str)
  (call-with-input-string str parse))

(define (parse-file filename)
  (call-with-input-file filename parse))

#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (REGISTER ID LABEL NUMBER))
(define-empty-tokens op-tokens (COMMA))

(define asml
  (lexer
   ;; Skip whitespaces
   [(:or #\tab #\space #\newline) (asml input-port)]
   
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
    (token-NUMBER (string->number
                   (substring lexeme 0 (- (string-length lexeme) 1)) 2))]
   ;;; - Decimal
   [(:+ numeric) (token-NUMBER (string->number lexeme))]
   ;;; - Hexadecimal
   [(:: "0x" (:+ (:or (:/ "0" "9") (:/ "a" "f") (:/ "A" "F"))))
    (token-NUMBER (string->number (substring lexeme 2) 16))]))

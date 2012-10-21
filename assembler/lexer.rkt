#lang racket/base

(require racket/contract/base
         syntax/readerr
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide/contract
 [asm-lexer (input-port? . -> . position-token?)])

(provide data delim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check that there's a whitespace or eof coming up in the input-port.
(define (assert-delimiter-follows! lexeme ip)
  (define next-char (peek-char ip))
  (unless (or (eof-object? next-char)
              (char-whitespace? next-char)
              (char=? next-char #\,))
    (define-values (line column position) (port-next-location ip))
    (raise-read-error (format "expected delimiter after ~e, got ~e"
                              lexeme (string next-char))
                      (object-name ip)
                      line column position 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexeme conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lexeme->symbol lexeme)
  (string->symbol (string-downcase lexeme)))

(define (lexeme->label lexeme)
  (substring lexeme 0 (- (string-length lexeme) 1)))

(define (lexeme->number2 lexeme)
  (string->number (substring lexeme 0 (- (string-length lexeme) 1)) 2))

(define lexeme->number10 string->number)

(define (lexeme->number16 lexeme)
  (string->number (substring lexeme 2) 16))

;; convert a lexeme to a token and check that the token is
;; immediately followed by a delimiter
(define (lexeme->token/check-delimiter! lexeme ip token-fn convert-fn)
  (assert-delimiter-follows! lexeme ip)
  (token-fn (convert-fn lexeme)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lexer tokens definition
(define-tokens       data  (ID LABEL LABEL-REF REGISTER NUMBER))
(define-empty-tokens delim (COMMA NEWLINE EOF))

;; Lexer abbreviations
(define-lex-abbrevs
  [b (char-set "bB")]
  [i (char-set "iI")]
  [p (char-set "pP")]
  [r (char-set "rR")]
  [s (char-set "sS")]

  [register (:or (:: r (:/ "0" "8"))
                 (:: s p)
                 (:: b p)
                 (:: i p))]

  [digit (:/ "0" "9")]
  [digit2 (:/ "0" "1")]
  [digit16 (:/ "af" "AF" "09")])

;; Lexer
(define asm-lexer
  (lexer-src-pos
   [(eof) 'EOF]
   [(:or #\tab #\space) (return-without-pos (asm-lexer input-port))]
   [#\, 'COMMA]
   [#\newline 'NEWLINE]
   ;; Register
   [register
    (lexeme->token/check-delimiter! lexeme input-port
                                    token-REGISTER lexeme->symbol)]
   ;; ID
   [(:: alphabetic (:+ (:or alphabetic numeric)))
    (lexeme->token/check-delimiter! lexeme input-port token-ID lexeme->symbol)]
   ;; LABEL
   [(:: alphabetic (:* (:or alphabetic numeric #\$ #\_ #\.)) ":")
    (lexeme->token/check-delimiter! lexeme input-port
                                    token-LABEL lexeme->label)]
   ;;; Binary number
   [(:: (:+ digit2) "b")
    (lexeme->token/check-delimiter! lexeme input-port
                                    token-NUMBER lexeme->number2)]
   ;; Decimal number
   [(:+ digit)
    (lexeme->token/check-delimiter! lexeme input-port
                                    token-NUMBER lexeme->number10)]
   ;; Hexadecimal number
   [(:: "0x" (:+ digit16))
    (lexeme->token/check-delimiter! lexeme input-port
                                    token-NUMBER lexeme->number16)]))

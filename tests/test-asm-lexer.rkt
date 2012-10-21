#lang racket/base

(require racket/port
	 rackunit
         parser-tools/lex)

(require/expose "../assembler/lexer.rkt"
		(asm-lexer token-ID token-LABEL token-REGISTER token-NUMBER))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexer tests utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (collect-lexer-tokens instr)
  (call-with-input-string
   instr
   (lambda (ip)
     (define producer (lambda () (position-token-token (asm-lexer ip))))
     (for/list ([token (in-producer producer 'EOF)])
       token))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base tests - Single tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ID tokens
(check-equal? (collect-lexer-tokens "mov")  (list (token-ID 'mov)))
(check-equal? (collect-lexer-tokens "Cmp")  (list (token-ID 'cmp)))
(check-equal? (collect-lexer-tokens "jZ")   (list (token-ID 'jz)))
(check-equal? (collect-lexer-tokens "jNe")  (list (token-ID 'jne)))
(check-equal? (collect-lexer-tokens "PUSH") (list (token-ID 'push)))
(check-exn exn:fail? (lambda () (collect-lexer-tokens "mov+")))
(check-exn exn:fail? (lambda () (collect-lexer-tokens "m.ov")))

;; LABEL tokens
(check-equal? (collect-lexer-tokens "label:")      (list (token-LABEL "label")))
(check-equal? (collect-lexer-tokens "label0:")     (list (token-LABEL "label0")))
(check-equal? (collect-lexer-tokens "label0a:")    (list (token-LABEL "label0a")))
(check-equal? (collect-lexer-tokens "label$:")     (list (token-LABEL "label$")))
(check-equal? (collect-lexer-tokens "label_a:")    (list (token-LABEL "label_a")))
(check-equal? (collect-lexer-tokens "label.l:")    (list (token-LABEL "label.l")))
(check-equal? (collect-lexer-tokens "label.l42:")  (list (token-LABEL "label.l42")))
(check-equal? (collect-lexer-tokens "label.42:")   (list (token-LABEL "label.42")))
(check-equal? (collect-lexer-tokens "label.$:")    (list (token-LABEL "label.$")))
(check-equal? (collect-lexer-tokens "label.$42l:") (list (token-LABEL "label.$42l")))
(check-exn exn:fail? (lambda () (collect-lexer-tokens "label#:")))

;; REGISTER tokens
(check-equal? (collect-lexer-tokens "r0") (list (token-REGISTER 'r0)))
(check-equal? (collect-lexer-tokens "R8") (list (token-REGISTER 'r8)))
(check-equal? (collect-lexer-tokens "Ip") (list (token-REGISTER 'ip)))
(check-equal? (collect-lexer-tokens "sP") (list (token-REGISTER 'sp)))
(check-equal? (collect-lexer-tokens "BP") (list (token-REGISTER 'bp)))

;; NUMBER tokens
(check-equal? (collect-lexer-tokens "42") (list (token-NUMBER 42)))
(check-equal? (collect-lexer-tokens "+42") (list (token-NUMBER 42)))
(check-equal? (collect-lexer-tokens "-42") (list (token-NUMBER -42)))
(check-equal? (collect-lexer-tokens "0000000042") (list (token-NUMBER 42)))
(check-equal? (collect-lexer-tokens "101010b") (list (token-NUMBER 42)))
(check-equal? (collect-lexer-tokens "00101010b") (list (token-NUMBER 42)))
(check-equal? (collect-lexer-tokens "0x2a") (list (token-NUMBER 42)))
(check-equal? (collect-lexer-tokens "0x00000000002A") (list (token-NUMBER 42)))
(check-equal? (collect-lexer-tokens "0xfF002A") (list (token-NUMBER 16711722)))
(check-exn exn:fail? (lambda () (collect-lexer-tokens "42a")))
(check-exn exn:fail? (lambda () (collect-lexer-tokens "0xG")))

;; Compound forms
(check-equal? (collect-lexer-tokens "-8(sp)")
              (list (token-NUMBER -8) 'OP (token-REGISTER 'sp) 'CP))

(check-equal? (collect-lexer-tokens "+24(bp)")
              (list (token-NUMBER 24) 'OP (token-REGISTER 'bp) 'CP))

(check-equal? (collect-lexer-tokens "(r0)")
              (list 'OP (token-REGISTER 'r0) 'CP))

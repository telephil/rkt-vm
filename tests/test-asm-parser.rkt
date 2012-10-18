#lang racket/base

(require racket/port
         racket/bool
         rackunit
         rackunit/text-ui
         parser-tools/lex)

(require/expose "../assembler/asm-parser.rkt" (asm-lexer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests case creation macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a test case for a given lexer token
(define-syntax make-lexer-token-test-case
  (syntax-rules ()
    [(_ test str name value op)
     (test-case test
      (call-with-input-string str
                              (λ (ip)
                                (let ([token (asm-lexer ip)])
                                  (check-pred position-token? token "Not a position token")
                                  (check-pred token? (position-token-token token) "Not a token")
                                  (check-eq? (token-name (position-token-token token)) name "Invalid name")
                                  (check op (token-value (position-token-token token)) value "Invalid value"))
                                (let ([token (asm-lexer ip)])
                                  (check-pred position-token? token)
                                  (check-eq? (position-token-token token) 'EOF)))))]))

(define-syntax create-lexer-token-take-case-macro
  (syntax-rules ()
    [(_ mname type op)
     (define-syntax mname
       (syntax-rules ()
         [(_ name in out)
          (make-lexer-token-test-case name in type out op)]))]))

(create-lexer-token-take-case-macro id-test-case       'ID       symbol=?)
(create-lexer-token-take-case-macro label-test-case    'LABEL    string=?)
(create-lexer-token-take-case-macro register-test-case 'REGISTER symbol=?)
(create-lexer-token-take-case-macro number-test-case   'NUMBER   =)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexer tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test-suite id-tests
  (id-test-case "mov" "mov" 'mov)
  (id-test-case "Cmp" "Cmp" 'cmp)
  (id-test-case "jZ" "jZ" 'jz)
  (id-test-case "jNe" "jNe" 'jne)
  (id-test-case "PUSH" "PUSH" 'push))

(define-test-suite label-tests
  (label-test-case "label" "label:" "label")
  (label-test-case "label0" "label0:" "label0")
  (label-test-case "label0a" "label0a:" "label0a")
  (label-test-case "label$" "label$:" "label$")
  (label-test-case "label_a" "label_a:" "label_a")
  (label-test-case "label.l" "label.l:" "label.l")
  (label-test-case "label.l42" "label.l42:" "label.l42")
  (label-test-case "label.l42" "label.42:" "label.42")
  (label-test-case "label.$" "label.$:" "label.$"))

(define-test-suite register-tests
  (register-test-case "reg r0" "r1" 'r0)
  (register-test-case "reg R8" "R8" 'r8)
  (register-test-case "reg Ip" "Ip" 'ip)
  (register-test-case "reg sP" "sP" 'sp)
  (register-test-case "reg BP" "BP" 'bp))

(define-test-suite number-tests
  (number-test-case "dec" "42" 42)
  (number-test-case "dec2" "000000042" 42)
  (number-test-case "bin" "101010b" 42)
  (number-test-case "bin2" "00101010b" 42)
  (number-test-case "hex" "0x2A" 42)
  (number-test-case "hex2" "0x000000000002A" 42)
  (number-test-case "hex3" "0xfF002A" 16711722))


(define-test-suite lexer-tests
  id-tests
  label-tests
  register-tests
  number-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test-suite all-tests
  lexer-tests)

(run-tests all-tests)

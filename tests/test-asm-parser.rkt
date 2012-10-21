#lang racket/base

(require racket/contract/base
         rackunit
         "../assembler/parser.rkt"
         "../assembler/syntax.rkt")

;; Simple invalid expressions
(check-exn exn:fail? (lambda () (parse-string "label:\n")))
(check-exn exn:fail? (lambda () (parse-string "42\n")))


;; Label and instruction
(check-equal? (parse-string "l: ret\n")
              (list (label-stx "l") (insn-stx (opcode-stx 'ret) #f #f)))

;; arg-less instruction
(check-equal? (parse-string "ret\n") (list (insn-stx (opcode-stx 'ret) #f #f)))

;; single arg instruction
(check-equal? (parse-string "push r0\n")
              (list (insn-stx (opcode-stx 'push) (register-stx 'r0) #f)))

;; label-stx arg
(check-equal? (parse-string "call foobar\n")
              (list (insn-stx (opcode-stx 'call) (label-stx "foobar") #f)))

;; register-stx args
(check-equal? (parse-string "mov r0, r1\n")
              (list (insn-stx (opcode-stx 'mov)
                              (register-stx 'r0) (register-stx 'r1))))

;; number-stx args
(check-equal? (parse-string "and bp, 42\n")
              (list (insn-stx (opcode-stx 'and)
                              (register-stx 'bp) (number-stx 42))))

;; ptr-stx args
(check-equal? (parse-string "mov +8(bp), r0\n")
              (list (insn-stx (opcode-stx 'mov)
                              (ptr-stx 'bp 8) (register-stx 'r0))))

(check-equal? (parse-string "mov r0, -24(sp)\n")
              (list (insn-stx (opcode-stx 'mov)
                              (register-stx 'r0) (ptr-stx 'sp -24))))

(check-equal? (parse-string "mov (r0), r1\n")
              (list (insn-stx (opcode-stx 'mov)
                              (ptr-stx 'r0 0) (register-stx 'r1))))

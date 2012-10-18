#lang racket/base

(require racket/contract/base
	 rackunit
	 "../assembler/parser.rkt"
	 "../assembler/syntax.rkt")

;; Simple invalid expressions
(check-exn exn:fail? (lambda () (parse-string "label:\n")))
(check-exn exn:fail? (lambda () (parse-string "42\n")))

;; Simple instructions
(check-equal? (parse-string "ret\n") (list (insn-stx (opcode-stx 'ret) #f #f)))

(check-equal? (parse-string "push r0\n") (list (insn-stx (opcode-stx 'push)
							 (register-stx 'r0) #f)))

(check-equal? (parse-string "call foobar\n") (list (insn-stx (opcode-stx 'call)
							     (label-stx "foobar")
							     #f)))

(check-equal? (parse-string "mov r0, r1\n") (list (insn-stx (opcode-stx 'mov)
							    (register-stx 'r0)
							    (register-stx 'r1))))

(check-equal? (parse-string "and bp, 42\n") (list (insn-stx (opcode-stx 'and)
							    (register-stx 'bp)
							    (number-stx 42))))



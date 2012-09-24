#lang racket

;; syntax tree definition

(provide
 (struct-out label-stx)
 (struct-out insn-stx)
 (struct-out opcode-stx)
 (struct-out register-stx)
 (struct-out number-stx))

(struct label-stx (name))
(struct opcode-stx (name))
(struct register-stx (name))
(struct number-stx (value))
(struct insn-stx (op arg1 arg2))
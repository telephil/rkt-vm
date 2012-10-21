#lang racket/base

;; syntax tree definition

(provide
 (struct-out label-stx)
 (struct-out insn-stx)
 (struct-out opcode-stx)
 (struct-out register-stx)
 (struct-out ptr-stx)
 (struct-out number-stx))

(struct label-stx (name) #:inspector #f)

(struct opcode-stx (name) #:inspector #f)

(struct register-stx (name) #:inspector #f)

(struct ptr-stx (register offset) #:inspector #f)

(struct number-stx (value) #:inspector #f)

(struct insn-stx (op arg1 arg2)	#:inspector #f)

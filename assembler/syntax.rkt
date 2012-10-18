#lang racket/base

;; syntax tree definition

(provide
 (struct-out label-stx)
 (struct-out insn-stx)
 (struct-out opcode-stx)
 (struct-out register-stx)
 (struct-out number-stx))

(struct label-stx (name)
	#:inspector (make-inspector))

(struct opcode-stx (name)
	#:inspector (make-inspector))

(struct register-stx (name)
	#:inspector (make-inspector))

(struct number-stx (value)
	#:inspector (make-inspector))

(struct insn-stx (op arg1 arg2)
	#:inspector (make-inspector))

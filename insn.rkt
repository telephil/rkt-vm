#lang racket

(provide
 (struct-out label)
 (struct-out insn))

(struct label 
  (name))

(struct insn 
  (op args))

(define label/c
  (struct/dc label
             [name string?]))

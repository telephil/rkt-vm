#lang racket

(provide/contract
 [opcode-argcount (symbol? . -> . (or/c integer? #f))])

(define (opcode-argcount opcode)
  (hash-ref opcodes-argcount opcode #f))

(define opcodes-argcount
  #hash((nop . 0)
        (mov . 2)
        (and . 2)
        (or . 2)
        (xor . 2)
        (not . 1)
        (shl . 1)
        (shr . 1)
        (add . 2)
        (sub . 2)
        (mul . 2)
        (div . 2)
        (inc . 1)
        (dec . 1)
        (jmp . 1)
        (jz . 1)
        (jnz . 1)
        (je . 1)
        (jne . 1)
        (jg . 1)
        (jge . 1)
        (jl . 1)
        (jle . 1)))

#|
(struct opcode
  (name argcount))

(define opcodes
  (list
   (opcode "nop" 0)
   (opcode "mov" 2)
   (opcode "and" 2)
   (opcode "or"  2)
   (opcode "xor" 2)
   (opcode "not" 1)
   (opcode "shl" 1)
   (opcode "shr" 1)
   (opcode "add" 2)
   (opcode "sub" 2)
   (opcode "mul" 2)
   (opcode "div" 2)
   (opcode "inc" 1)
   (opcode "dec" 1)
   (opcode "jmp" 1)
   (opcode "jz"  1)
   (opcode "jnz" 1)
   (opcode "je"  1)
   (opcode "jne" 1)
   (opcode "jg"  1)
   (opcode "jge" 1)
   (opcode "jl"  1)
   (opcode "jle" 1)))
|#
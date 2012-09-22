#lang racket

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

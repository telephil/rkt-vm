#lang racket

(provide/contract
 [opcode->bytecode (symbol? . -> . (or/c integer? #f))]
 [opcode-argcount (symbol? . -> . (or/c integer? #f))])

;; Return the opcode for the given OP
;; If OP does not exist return #f
(define (opcode->bytecode op)
  (define res (hash-ref opcodes op #f))
  (or (and res (first res))))

;; Return the argcount for the given op
;; If OP does not exist return #f
(define (opcode-argcount op)
  (define res (hash-ref opcodes op #f))
  (or (and res (second res))))

(define opcodes
  ;; Hash of SYMBOL -> '(OPCODE ARGCOUNT)
  #hash((nop . (#x00 0))
        (mov . (#x01 2))
        (and . (#x02 2))
        (or  . (#x03 2))
        (xor . (#x04 2))
        (not . (#x05 1))
        (shl . (#x06 1))
        (shr . (#x07 1))
        (add . (#x08 2))
        (sub . (#x09 2))
        (mul . (#x0A 2))
        (div . (#x0B 2))
        (inc . (#x0C 1))
        (dec . (#x0D 1))
        (jmp . (#x0E 1))
        (jz .  (#x0F 1))
        (jnz . (#x10 1))
        (je .  (#x11 1))
        (jne . (#x12 1))
        (jg .  (#x13 1))
        (jge . (#x14 1))
        (jl .  (#x15 1))
        (jle . (#x16 1))
        (push . (#x17 1))
        (pop . #(x18 1))))

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
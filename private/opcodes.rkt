#lang racket

(provide/contract
 [opcode->bytecode (symbol? . -> . (or/c integer? #f))]
 [opcode-argcount (symbol? . -> . (or/c integer? #f))])

;; Return the opcode for the given OP
;; If OP does not exist return #f
(define (opcode->bytecode op)
  (define res (hash-ref opcode-by-symbol op #f))
  (or (and res (opcode-bytecode res))))

;; Return the argcount for the given op
;; If OP does not exist return #f
(define (opcode-argcount op)
  (define res (hash-ref opcode-by-symbol op #f))
  (or (and res (opcode-argc res))))

(struct opcode (symbol bytecode argc))

;; List of opcodes
(define opcodes
  (list
   (opcode 'nop  #x00 0)
   (opcode 'mov  #x01 2)
   (opcode 'and  #x02 2)
   (opcode 'or   #x03 2)
   (opcode 'xor  #x04 2)
   (opcode 'not  #x05 1)
   (opcode 'shl  #x06 2)
   (opcode 'shr  #x07 2)
   (opcode 'add  #x08 2)
   (opcode 'sub  #x09 2)
   (opcode 'mul  #x0A 2)
   (opcode 'div  #x0B 2)
   (opcode 'inc  #x0C 1)
   (opcode 'dec  #x0D 1)
   (opcode 'jmp  #x0E 1)
   (opcode 'jz   #x0F 1)
   (opcode 'jnz  #x10 1)
   (opcode 'je   #x11 1)
   (opcode 'jne  #x12 1)
   (opcode 'jg   #x13 1)
   (opcode 'jge  #x14 1)
   (opcode 'jl   #x15 1)
   (opcode 'jle  #x16 1)
   (opcode 'push #x17 1)
   (opcode 'pop  #x18 1)
   (opcode 'end  #xFF 0)))

;; Hash of SYMBOL -> OPCODE
(define opcode-by-symbol
  (for/hash ([op opcodes])
    (values (opcode-symbol op) op)))

;; Hash of BYTECODE -> OPCODE 
(define opcode-by-bytecode
  (for/hash ([op opcodes])
    (values (opcode-bytecode op) op)))

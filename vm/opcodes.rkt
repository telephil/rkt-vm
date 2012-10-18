#lang racket/base

(require racket/contract/base)

(provide/contract
 [opcode->bytecode (symbol? . -> . (or/c integer? #f))]
 [opcode-argcount (symbol? . -> . (or/c integer? #f))]
 [opcode->string (integer? . -> . (or/c string? #f))]
 [opcode->symbol (integer? . -> . (or/c symbol? #f))]
 (NOP  integer?)
 (MOV  integer?)
 (AND  integer?)
 (OR   integer?) 
 (XOR  integer?)
 (NOT  integer?)
 (SHL  integer?)
 (SHR  integer?)
 (ADD  integer?)
 (SUB  integer?)
 (MUL  integer?)
 (DIV  integer?)
 (INC  integer?)
 (DEC  integer?)
 (CMP  integer?)
 (JMP  integer?)
 (JZ   integer?) 
 (JNZ  integer?)
 (JE   integer?) 
 (JNE  integer?)
 (JG   integer?) 
 (JGE  integer?)
 (JL   integer?) 
 (JLE  integer?)
 (PUSH integer?)
 (POP  integer?)
 (CALL integer?)
 (RET  integer?)
 (END  integer?))

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

(define (opcode->symbol bc)
  (define res (hash-ref opcode-by-bytecode bc #f))
  (or (and res (opcode-symbol res))))

(define (opcode->string bc)
  (define res (hash-ref opcode-by-bytecode bc #f))
  (or (and res (symbol->string (opcode-symbol res)))))

(struct opcode (symbol bytecode argc))

;; opcode constants
(define NOP  #x00)
(define MOV  #x01)
(define AND  #x02)
(define OR   #x03)
(define XOR  #x04)
(define NOT  #x05)
(define SHL  #x06)
(define SHR  #x07)
(define ADD  #x08)
(define SUB  #x09)
(define MUL  #x0A)
(define DIV  #x0B)
(define INC  #x0C)
(define DEC  #x0D)
(define CMP  #x0E)
(define JMP  #x0F)
(define JZ   #x10)
(define JNZ  #x11)
(define JE   #x12)
(define JNE  #x13)
(define JG   #x14)
(define JGE  #x15)
(define JL   #x16)
(define JLE  #x17)
(define PUSH #x18)
(define POP  #x19)
(define CALL #x1A)
(define RET  #x1B)
(define END  #xFF)

;; List of opcodes
(define opcodes
  (list
   (opcode 'nop  NOP  0)
   (opcode 'mov  MOV  2)
   (opcode 'and  AND  2)
   (opcode 'or   OR   2)
   (opcode 'xor  XOR  2)
   (opcode 'not  NOT  1)
   (opcode 'shl  SHL  2)
   (opcode 'shr  SHR  2)
   (opcode 'add  ADD  2)
   (opcode 'sub  SUB  2)
   (opcode 'mul  MUL  2)
   (opcode 'div  DIV  2)
   (opcode 'inc  INC  1)
   (opcode 'dec  DEC  1)
   (opcode 'cmp  CMP  2)
   (opcode 'jmp  JMP  1)
   (opcode 'jz   JZ   1)
   (opcode 'jnz  JNZ  1)
   (opcode 'je   JE   1)
   (opcode 'jne  JNE  1)
   (opcode 'jg   JG   1)
   (opcode 'jge  JGE  1)
   (opcode 'jl   JL   1)
   (opcode 'jle  JLE  1)
   (opcode 'push PUSH 1)
   (opcode 'pop  POP  1)
   (opcode 'call CALL 1)
   (opcode 'ret  RET  0)
   (opcode 'end  END  0)))

;; Hash of SYMBOL -> OPCODE
(define opcode-by-symbol
  (for/hash ([op opcodes])
    (values (opcode-symbol op) op)))

;; Hash of BYTECODE -> OPCODE 
(define opcode-by-bytecode
  (for/hash ([op opcodes])
    (values (opcode-bytecode op) op)))

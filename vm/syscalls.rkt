;; Syscalls implementation (included from vm/core.rkt)


(define (syscall)
  (define id (r0))
  (define call (hash-ref syscalls id #f))
  (when call
    (call)))

(define (syscall_writeint)
  (define v (r1))
  (display v))

(define (syscall_writechar)
  (define v (r1))
  (display (integer->char v)))

(define syscalls
  (hash
   10 syscall_writeint
   11 syscall_writechar))

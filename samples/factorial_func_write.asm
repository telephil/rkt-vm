;;; Sample use of the syscall op
;;; once computed the result of calling fact is printed
fact:   mul     r0,     r1
        dec     r1
        jnz     fact
        ret
start:  mov     r0,     1
        mov     r1,     5
        call    fact
        ;; syscall 10: write-int
        mov     r1,     r0
        mov     r0,     10
        syscall
        ;; syscall 11: write-char
        mov     r1,     10
        mov     r0,     11
        syscall

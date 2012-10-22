fact:   mul     r0,     r1
        dec     r1
        jnz     fact
        ret
start:  mov     r0,     1
        mov     r1,     5
        call    fact
        mov     r1,     r0
        mov     r0,     10
        syscall
        mov     r1,     10
        mov     r0,     11
        syscall

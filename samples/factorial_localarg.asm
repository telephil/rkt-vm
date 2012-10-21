fact:   push    bp
        mov     bp,     sp
        mov     -8(bp), 1
loop:   mul     -8(bp), 16(bp)
        dec     16(bp)
        jnz     loop
        mov     r0,     -8(bp)
        pop     bp
        ret
start:  push    5
        call    fact

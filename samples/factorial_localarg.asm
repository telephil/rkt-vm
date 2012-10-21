fact:   push    bp
        mov     bp,     sp
        sub     sp,     8
        mov     -8(bp), 1
loop:   mul     -8(bp), 16(bp)
        dec     16(bp)
        jnz     loop
        mov     r0,     -8(bp)
        add     sp,     8
        pop     bp
        ret
start:  push    5
        call    fact

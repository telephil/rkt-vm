fact:   push bp
        mov  bp, sp
        mov  r0, 16(bp)
        mov  r1, 24(bp)
loop:   mul  r0, r1
        dec  r1
        jnz  loop
        pop  bp
        ret
start:  push 5
        push 1
        call fact

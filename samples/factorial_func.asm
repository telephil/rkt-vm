fact:	mul r0, r1
	dec r1
	jnz fact
	ret
start:	mov r0, 1
	mov r1, 5
	call fact

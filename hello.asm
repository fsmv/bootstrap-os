

mov si, hello
mov cl, 6
loop:
mov ah, 0x0e
mov byte al, [ds:si]
int 0x10
inc si
dec cl
test cl, cl
jnz loop

hlt

hello: db "Hello!"


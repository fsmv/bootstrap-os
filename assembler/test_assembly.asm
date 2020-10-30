; This file gets included in the tester.asm binary and run through our assembler
; (for testing it against the host OS assembler)

; First just check all the register tables

; All 8 bit reg arms
inc al
inc cl
inc dl
inc bl
inc ah
inc ch
inc dh
inc bh
; All 16 bit reg args
dec ax
dec bx
dec cx
dec dx
dec si
dec di
dec sp
dec bp
; All memory args
mov bx, [si]
mov bx, [di]
mov bx, [bp]
mov bx, [bx]
mov bx, [bp+si]
mov bx, [bp+di]
mov bx, [bx+si]
mov bx, [bx+di]
; All segment prefixes
mov bx, [es:bx]
mov bx, [ds:bp+si]
mov bx, [ss:bx]
mov bx, [ds:bp+si]

; TODO test all argument parsing types

; Now we can just test one of each arg type for all of the opcodes (since we
; know that the different register numbers work from above)

; non-jcc ops

; IMM_8|DEFAULT_10
aad 0x04
aam 0x10

; NO_ARG
aaa
aas
cbw
clc
cld
cmc
cwd
daa
das
hlt
insb
insw
outsb
outsw
into
iret
lahf
;lock  ; TODO we don't really support this as a prefix
nop
popf
pushf
sahf
stc
std
xlatb

; REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY
adc cx, 0xBEEF
adc [bx], ax
adc cx, [es:si]
adc ax, bx
adc cl, 0x1
adc bl, ah
adc ch, [ds:di]
adc al, bl

add bx, 0xACDC
add cx, cx
add cx, [bx]
add ax, bp
add bl, 0xFA
add ch, [bp]
add [bx], ch
add al, bh

;and bx, 0x9 ; TODO: nasm uses the special sign extend opcode for this if you don't say "word"
and cx, cx
and cx, [bp]
and ax, bp
and bl, 0xFA
and cl, ch
and ch, [di]
and al, bh

;or bx, 0x9
or cx, cx
or cx, [bp]
or ax, bp
or bl, 0xFA
or cl, ch
or ch, [di]
or al, bh

cmp bx, 0x1234
cmp cx, cx
cmp bx, [di]
cmp ax, bp
cmp bl, 0xFA
cmp cl, ch
cmp cl, [bx+si]
cmp al, bh

xor bx, 0x1234
xor cx, cx
xor bx, [di]
xor ax, bp
xor bl, 0xFA
xor cl, ch
xor cl, [bx+si]
xor al, bh

;sub bx, 0x9
sub cx, cx
sub cx, [bp]
sub ax, bp
sub bl, 0xFA
sub cl, ch
sub ch, [di]
sub al, bh

sbb bx, 0xACDC
sbb cx, cx
sbb cx, [bx]
sbb ax, bp
sbb bl, 0xFA
sbb ch, [bp]
sbb [bx], ch
sbb al, bh

; REG_IMM|TWO_REG|SHORT_REG_AL_ONLY
test cx, 0x01
test cl, 0x01
test bx, ax
test [di], sp
test ax, bx
test al, cl

; IMM_16|REG_16|FAR_JUMP
; TODO: nasm converts these immediates from absolute to relative offsets
;call 0x7C0 
call di
call [ds:bx+si]
; ONE_IMM|REG_16|FAR_JUMP
;jmp 0x4
;jmp 0xFEED
jmp ax
jmp [bx]

; ONE_REG (maybe also SHORT_REG)
inc cx
inc al
; inc [di] ; TODO: nasm requires operand size
dec cx
dec al
;dec [di]

div cx
div al
;div [di]
idiv cx
idiv al
;idiv [di]

mul cx
mul al
;mul [di]
imul cx
imul al
;imul [di]

not cx
not al
;not [di]
neg cx
neg al
;neg [di]

; IMM_8|SHORT_REG_DX_ONLY
; TODO: nasm expects the required al parameter first parametere but we don't want it
;in 0x8
;in dl
;out 0x9
;out dl

; IMM_8
int 0x16

; TODO: nasm converts these immediates from absolute to relative offsets
;loop 0x13
;loope 0x37
;loopne 0xBC

; SWAP_TWO_REG|LOAD_ADDR_16_ONLY
lds cx, [bx]
lea dx, [bp+di]
les ax, [si]

; mov ;TODO

; SHIFT (reg/mem, imm8 or reg/mem, 1 or reg/mem, cl)
rcl cl, 0x02
;rcr cl, 0x1; TODO: nasm wants operand size [bx], 1
;rol ax, cl
ror bx, cl
sal dh, 0x8
sar dl, 0x15
;shl dl, 0x1
;shr ah, 0x2 ; TODO: nasm wants operand size [es:di], 0x12

; NO_ARG|IMM_16
ret
retf
ret 0x1201 ; TODO: this doesn't infer 8bit when you don't type 3-4 chars
retf 0x5678

; REG_16|SHORT_REG
pop bx

; REG_16|IMM_16|SHORT_REG
push cx
push 0x1234

; TWO_REG
xchg cx, bx
xchg [di], ah

; MOD_* constants and *_addressing constants come from:
; Vol 2. Chapter 2.1.5 Table 2-1

; The first 2 bits of the ModRM byte
MOD_MEM:        equ 0x00
MOD_MEM_DISP8:  equ 0x40
MOD_MEM_DISP16: equ 0x80
MOD_REG:        equ 0xC0

; In order. Count to get the 3 bit R/M field value
mem_addressing:
db 'bx+si',0
db 'bx+di',0
db 'bp+si',0
db 'bp+di',0
db 'si',0
db 'di',0
db 'bp',0
db 'bx',0
db 0

; In order. Count to get the 3 bit R/M field value
reg_8_addressing:
db 'al'
db 'cl'
db 'dl'
db 'bl'
db 'ah'
db 'ch'
db 'dh'
db 'bh'
dw 0

; In order. Count to get the 3 bit R/M field value
reg_16_addressing:
db 'ax'
db 'cx'
db 'dx'
db 'bx'
db 'sp'
db 'bp'
db 'si'
db 'di'
dw 0

; The segment register names and their prefix byte values
;  - Prefix byte values are from Vol 2. Chapter 2.1.1
;  - These are in order of the Sreg arg (for mov) from Vol 2. Chapter 3.1.1.3
segment_prefixes:
db 'es', 0x26
db 'cs', 0x2E
db 'ss', 0x36
db 'ds', 0x3E
dw 0

; These are only the original 8086 instructions.
; List from Vol 3. Chapter 20.1.3
instructions:
db 3,'aaa'
db 3,'aad'
db 3,'aam'
db 3,'aas'
db 3,'adc'
db 3,'add'
db 3,'and'
db 4,'call'
db 3,'cbw'
db 3,'clc'
db 3,'cld'
db 3,'cmc'
db 3,'cmp'
db 3,'cwd'
db 3,'daa'
db 3,'das'
db 3,'dec'
db 3,'div'
db 3,'hlt'
db 4,'idiv'
db 4,'imul'
db 2,'in'
db 3,'inc'
db 4,'insb'
db 4,'insw'
db 3,'int'
db 4,'into'
db 4,'iret'
db 4,'lahf'
db 3,'lds'
db 3,'lea'
db 3,'les'
db 4,'lock'
db 4,'loop'
db 5,'loope'
db 6,'loopne'
db 3,'mov'
db 3,'mul'
db 3,'neg'
db 3,'nop'
db 3,'not'
db 2,'or'
db 3,'out'
db 5,'outsb'
db 5,'outsw'
db 3,'pop'
db 4,'popf'
db 4,'push'
db 5,'pushf'
db 3,'rcl'
db 3,'rcr'
db 3,'ret'
db 4,'retf'
db 3,'rol'
db 3,'ror'
db 4,'sahf'
db 3,'sal'
db 3,'sar'
db 3,'sbb'
db 3,'shl'
db 3,'shr'
db 3,'stc'
db 3,'std'
db 3,'sub'
db 4,'test'
db 4,'xchg'
db 5,'xlatb'
db 3,'xor'
db 0

; Conditional jumps (jcc) are in a separate ID space from the main instructions
; because there's a different pattern for 8/16 bit immediate opcodes and we
; don't need the args type flags
jcc_instructions:
db 2,'ja'
db 3,'jae'
db 2,'jb'
db 3,'jbe'
db 2,'jc'
db 4,'jcxz'
db 2,'je'
db 2,'jg'
db 3,'jge'
db 2,'jl'
db 3,'jle'
db 3,'jna'
db 4,'jnae'
db 3,'jnb'
db 4,'jnbe'
db 3,'jnc'
db 3,'jne'
db 3,'jng'
db 4,'jnge'
db 3,'jnl'
db 4,'jnle'
db 3,'jno'
db 3,'jnp'
db 3,'jns'
db 3,'jnz'
db 2,'jo'
db 2,'jp'
db 3,'jpe'
db 3,'jpo'
db 2,'js'
db 2,'jz'
db 0

; These are the short (8bit) jcc opcodes.
; To get the near (16bit) jcc opcodes: prefix with 0F and add 0x10
jcc_opcodes:
db 0x77, ; ja
db 0x73, ; jae
db 0x72, ; jb
db 0x76, ; jbe
db 0x72, ; jc
db 0xE3, ; jcxz    Note: Special because it has no "near" jump opcode
db 0x74, ; je
db 0x7F, ; jg
db 0x7D, ; jge
db 0x7C, ; jl
db 0x7E, ; jle
db 0x76, ; jna
db 0x72, ; jnae
db 0x73, ; jnb
db 0x77, ; jnbe
db 0x73, ; jnc
db 0x75, ; jne
db 0x7E, ; jng
db 0x7C, ; jnge
db 0x7D, ; jnl
db 0x7F, ; jnle
db 0x71, ; jno
db 0x7B, ; jnp
db 0x79, ; jns
db 0x75, ; jnz
db 0x70, ; jo
db 0x7A, ; jp
db 0x7A, ; jpe
db 0x7B, ; jpo
db 0x78, ; js
db 0x74, ; jz

; Argument format options for the instructions (bit flags you can OR together)
; Note: These aren't in the intel manual, it's just a classification I made up
NO_ARG:       equ 0x0001
REG_8:        equ 0x0002
REG_16:       equ 0x0004
ONE_REG:      equ REG_8|REG_16 ; reg/mem (might have a short_reg_opcode)
IMM_16:       equ 0x0008 ; 16 bit immediate only
IMM_8:        equ 0x0010 ; 8 bit immediate only
ONE_IMM:      equ IMM_8|IMM_16
TWO_REG:      equ 0x0020 ; reg/mem, reg
REG_IMM:      equ 0x0040 ; reg/mem, 8 bit or 16 bit (might have a short_reg_opcode)
SWAP_TWO_REG: equ 0x0080 ; reg, mem (encoded with opcode+=2)
; Less common argument options
SHORT_REG:         equ 0x0100 ; A 16 bit reg arg can be added into the short_reg_opcode to save the modRM byte
SHORT_REG_AL_ONLY: equ 0x0200 ; only al/ax, imm
SHORT_REG_DX_ONLY: equ 0x0400 ; only allows a single arg of dx
DEFAULT_10:        equ 0x0800 ; aad and aam have a special default arg of 10 and 8bit imm otherwise
LOAD_ADDR_16_ONLY: equ 0x1000 ; lea, lds, les all are reg16, mem only
SHIFT:             equ 0x2000 ; reg/mem, imm8 or reg/mem, 1 or reg/mem, cl
FAR_JUMP:          equ 0x4000
MOV_SEGMENT:       equ 0x8000

; All information below is copied out of Vol 2 and the pages for each instruction

SHIFT_ONE_OPCODE: equ 0xD0
SHIFT_CL_OPCODE:  equ 0xD2
JMP_SHORT_OPCODE: equ 0xEB
JMP_NEAR_OPCODE:  equ 0xE9
JMP_REG_OPCODE:   equ 0xFF
READ_SEGMENT_OPCODE:  equ 0x8C
WRITE_SEGMENT_OPCODE: equ 0x8E

JCC_FLAGS: equ ONE_IMM
JMP_FLAGS: equ ONE_IMM|REG_16|FAR_JUMP
JMP_EXTRA_INDEX: equ JMP_EXTRA_ADDR-extra_opcodes ; Note: the opcodes are one byte each

supported_args:
dw NO_ARG, ; aaa
dw IMM_8|DEFAULT_10, ; aad
dw IMM_8|DEFAULT_10, ; aam
dw NO_ARG, ; aas
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; adc
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; add
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; and
dw IMM_16|REG_16|FAR_JUMP, ; call
dw NO_ARG, ; cbw
dw NO_ARG, ; clc
dw NO_ARG, ; cld
dw NO_ARG, ; cmc
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; cmp
dw NO_ARG, ; cwd
dw NO_ARG, ; daa
dw NO_ARG, ; das
dw ONE_REG|SHORT_REG, ; dec
dw ONE_REG, ; div
dw NO_ARG, ; hlt
dw ONE_REG, ; idiv
dw ONE_REG, ; imul Note: the other 2 forms weren't on the 8086
dw IMM_8|SHORT_REG_DX_ONLY, ; in `in al/ax, imm8` or `in al/ax, dx` are the only forms
dw ONE_REG|SHORT_REG, ; inc
dw NO_ARG, ; insb
dw NO_ARG, ; insw
dw IMM_8, ; int
dw NO_ARG, ; into
dw NO_ARG, ; iret
dw NO_ARG, ; lahf
dw SWAP_TWO_REG|LOAD_ADDR_16_ONLY, ; lds
dw SWAP_TWO_REG|LOAD_ADDR_16_ONLY, ; lea
dw SWAP_TWO_REG|LOAD_ADDR_16_ONLY, ; les
dw NO_ARG, ; lock
dw IMM_8, ; loop
dw IMM_8, ; loope
dw IMM_8, ; loopne
dw TWO_REG|SWAP_TWO_REG|REG_IMM|SHORT_REG|MOV_SEGMENT, ; mov
dw ONE_REG, ; mul
dw ONE_REG, ; neg
dw NO_ARG, ; nop
dw ONE_REG, ; not
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; or
dw IMM_8|SHORT_REG_DX_ONLY, ; out
dw NO_ARG, ; outsb
dw NO_ARG, ; outsw
dw REG_16|SHORT_REG, ; pop   Note: there are special segment register opcodes we don't support
dw NO_ARG, ; popf
dw REG_16|IMM_16|SHORT_REG, ; push   Note: there are special segment register opcodes plus it has IMM_8 just not following the pattern
dw NO_ARG, ; pushf
dw SHIFT, ; rcl
dw SHIFT, ; rcr
dw NO_ARG|IMM_16, ; ret
dw NO_ARG|IMM_16, ; retf
dw SHIFT, ; rol
dw SHIFT, ; ror
dw NO_ARG, ; sahf
dw SHIFT, ; sal
dw SHIFT, ; sar
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; sbb
dw SHIFT, ; shl
dw SHIFT, ; shr
dw NO_ARG, ; stc
dw NO_ARG, ; std
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; sub
dw REG_IMM|TWO_REG|SHORT_REG_AL_ONLY, ; test
dw TWO_REG, ; xchg Note: also has a short-reg with just al we don't support because it's different
dw NO_ARG, ; xlatb
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; xor

; TODO: should we combine these lookup tables and just make each row a tuple?
;       note: 8086 mode doesn't support [ebx+4*ecx]
;       also takes up the same amount of space either way

reg_opcodes:
db 0x37, ; aaa
db 0x00, ; aad
db 0x00, ; aam
db 0x3F, ; aas
db 0x10, ; adc
db 0x00, ; add (base value is 0)
db 0x20, ; and
db 0xFE, ; call
db 0x98, ; cbw
db 0xF8, ; clc
db 0xFC, ; cld
db 0xF5, ; cmc
db 0x38, ; cmp
db 0x99, ; cwd
db 0x27, ; daa
db 0x2F, ; das
db 0xFE, ; dec
db 0xF6, ; div
db 0xF4, ; hlt
db 0xF6, ; idiv
db 0xF6, ; imul
db 0x00, ; in
db 0xFE, ; inc
db 0x6C, ; insb
db 0x6D, ; insw
db 0x00, ; int
db 0xCE, ; into
db 0xCF, ; iret
db 0x9F, ; lahf
db 0xC5-3, ; lds Note: -3 because these are reg16, mem only, which does opcode+3
db 0x8D-3, ; lea
db 0xC4-3, ; les
db 0xF0, ; lock
db 0x00, ; loop
db 0x00, ; loope
db 0x00, ; loopne
db 0x88, ; mov
db 0xF6, ; mul
db 0xF6, ; neg
db 0x90, ; nop
db 0xF6, ; not
db 0x08, ; or
db 0x00, ; out
db 0x6E, ; outsb
db 0x6F, ; outsw
db 0x8F, ; pop
db 0x9D, ; popf
db 0xFF, ; push
db 0x9C, ; pushf
db 0x00, ; rcl
db 0x00, ; rcr
db 0xC3, ; ret
db 0xCB, ; retf
db 0x00, ; rol
db 0x00, ; ror
db 0x9E, ; sahf
db 0x00, ; sal
db 0x00, ; sar
db 0x18, ; sbb
db 0x00, ; shl
db 0x00, ; shr
db 0xF9, ; stc
db 0xFD, ; std
db 0x28, ; sub
db 0x84, ; test
db 0x86, ; xchg
db 0xD7, ; xlatb
db 0x30, ; xor

immediate_opcodes:
db 0x00, ; aaa
db 0xD5, ; aad
db 0xD4, ; aam
db 0x00, ; aas
db 0x80, ; adc
db 0x80, ; add
db 0x80, ; and
db 0xE7, ; call (opcode-1 since it's 16bit only)
db 0x00, ; cbw
db 0x00, ; clc
db 0x00, ; cld
db 0x00, ; cmc
db 0x80, ; cmp
db 0x00, ; cwd
db 0x00, ; daa
db 0x00, ; das
db 0x00, ; dec
db 0x00, ; div
db 0x00, ; hlt
db 0x00, ; idiv
db 0x00, ; imul
db 0xE4, ; in
db 0x00, ; inc
db 0x00, ; insb
db 0x00, ; insw
db 0xCD, ; int
db 0x00, ; into
db 0x00, ; iret
db 0x00, ; lahf
db 0x00, ; lds
db 0x00, ; lea
db 0x00, ; les
db 0x00, ; lock
db 0xE2, ; loop
db 0xE1, ; loope
db 0xE0, ; loopne
db 0xC6, ; mov
db 0x00, ; mul
db 0x00, ; neg
db 0x00, ; nop
db 0x00, ; not
db 0x80, ; or
db 0x00, ; out
db 0x00, ; outsb
db 0x00, ; outsw
db 0x00, ; pop
db 0x00, ; popf
db 0x67, ; push (opcode-1 since it's 16bit only)
db 0x00, ; pushf
db 0xC0, ; rcl
db 0xC0, ; rcr
db 0xC1, ; ret (opcode-1 since it's 16bit only)
db 0xC9, ; retf (opcode-1 since it's 16bit only)
db 0xC0, ; rol
db 0xC0, ; ror
db 0x00, ; sahf
db 0xC0, ; sal
db 0xC0, ; sar
db 0x80, ; sbb
db 0xC0, ; shl
db 0xC0, ; shr
db 0x00, ; stc
db 0x00, ; std
db 0x80, ; sub
db 0xF6, ; test
db 0x00, ; xchg
db 0x00, ; xlatb
db 0x80, ; xor

; This is the "/digit" shown in the intel manual. It goes in the register bits
; in the ModRM byte for instructions with only 1 register argument sometimes.
extra_opcodes:
db 0x00, ; aaa
db 0x00, ; aad
db 0x00, ; aam
db 0x00, ; aas
db 0x02, ; adc
db 0x00, ; add  Note: actually needs the value 0
db 0x04, ; and
db 0x02, ; call
db 0x00, ; cbw
db 0x00, ; clc
db 0x00, ; cld
db 0x00, ; cmc
db 0x07, ; cmp
db 0x00, ; cwd
db 0x00, ; daa
db 0x00, ; das
db 0x01, ; dec
db 0x06, ; div
db 0x00, ; hlt
db 0x07, ; idiv
db 0x05, ; imul
db 0x00, ; in
db 0x00, ; inc (has value 0)
db 0x00, ; insb
db 0x00, ; insw
db 0x00, ; int
db 0x00, ; into
db 0x00, ; iret
db 0x00, ; lahf
db 0x00, ; lds
db 0x00, ; lea
db 0x00, ; les
db 0x00, ; lock
db 0x00, ; loop
db 0x00, ; loope
db 0x00, ; loopne
db 0x00, ; mov
db 0x04, ; mul
db 0x03, ; neg
db 0x00, ; nop
db 0x02, ; not
db 0x01, ; or
db 0x00, ; out
db 0x00, ; outsb
db 0x00, ; outsw
db 0x00, ; pop
db 0x00, ; popf
db 0x06, ; push
db 0x00, ; pushf
db 0x02, ; rcl
db 0x03, ; rcr
db 0x00, ; ret
db 0x00, ; retf
db 0x00, ; rol (value 0)
db 0x01, ; ror
db 0x00, ; sahf
db 0x04, ; sal
db 0x07, ; sar
db 0x03, ; sbb
db 0x04, ; shl
db 0x05, ; shr
db 0x00, ; stc
db 0x00, ; std
db 0x05, ; sub
db 0x00, ; test (value 0)
db 0x00, ; xchg
db 0x00, ; xlatb
db 0x06, ; xor
; Hack: Put jmp at the end even though it's not in the search list,
;       so that we can re-use write_mem_write to get this value
JMP_EXTRA_ADDR:
db 0x04, ; jmp

short_reg_opcodes:
db 0x00, ; aaa
db 0x00, ; aad
db 0x00, ; aam
db 0x00, ; aas
db 0x14, ; adc
db 0x04, ; add
db 0x24, ; and
db 0x00, ; call
db 0x00, ; cbw
db 0x00, ; clc
db 0x00, ; cld
db 0x00, ; cmc
db 0x3C, ; cmp
db 0x00, ; cwd
db 0x00, ; daa
db 0x00, ; das
db 0x48, ; dec
db 0x00, ; div
db 0x00, ; hlt
db 0x00, ; idiv
db 0x00, ; imul
db 0xEC, ; in
db 0x40, ; inc
db 0x00, ; insb
db 0x00, ; insw
db 0x00, ; int
db 0x00, ; into
db 0x00, ; iret
db 0x00, ; lahf
db 0x00, ; lds
db 0x00, ; lea
db 0x00, ; les
db 0x00, ; lock
db 0x00, ; loop
db 0x00, ; loope
db 0x00, ; loopne
db 0xB8, ; mov Note: there's also an 8bit short-reg op for this but we don't support it
db 0x00, ; mul
db 0x00, ; neg
db 0x00, ; nop
db 0x00, ; not
db 0x0C, ; or
db 0x00, ; out
db 0x00, ; outsb
db 0x00, ; outsw
db 0x58, ; pop
db 0x00, ; popf
db 0x50, ; push
db 0x00, ; pushf
db 0x00, ; rcl
db 0x00, ; rcr
db 0x00, ; ret
db 0x00, ; retf
db 0x00, ; rol
db 0x00, ; ror
db 0x00, ; sahf
db 0x00, ; sal
db 0x00, ; sar
db 0x1C, ; sbb
db 0x00, ; shl
db 0x00, ; shr
db 0x00, ; stc
db 0x00, ; std
db 0x2C, ; sub
db 0xA8, ; test
db 0x00, ; xchg
db 0x00, ; xlatb
db 0x34, ; xor

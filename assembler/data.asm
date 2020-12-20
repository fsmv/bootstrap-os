; Constants for offsets into symbol table data rows
;
; The layout is basically the same as instructions and jcc_instructions but we
; generate the data in memory
symbol_table: ; Note: this label is just for namespacing the two below
  .extra_data_size: equ 3
  .location: equ 0 ; line number if not resolved, address if resolved
  .is_resolved: equ 2

; MOD_* constants and *_addressing constants come from:
; Vol 2. Chapter 2.1.5 Table 2-1

; The first 2 bits of the ModRM byte
MOD_MEM:        equ 0x00
MOD_MEM_DISP8:  equ 0x40
MOD_MEM_DISP16: equ 0x80
MOD_REG:        equ 0xC0

; The MODRM byte value for when we use [imm16] as an argument
MODRM_ABSOLUTE_ADDRESS: equ 0x06 ; The middle 3 bits reg field is 0 here

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

; Intel manual references:
;  - Supported instriction list mainly from Vol 3. Chapter 20.1.3
;  - Opcode data is from the instructions listing in Vol 2
;
; Table schema:
;   instruction_size, instruction
;   supported_args
;   reg_opcode,imm_opcode,extra_opcode,short_reg_opcode
instructions:
.extra_data_size: equ 6
; Named offsets into the extra data:
.supported_args: equ 0
.reg_opcode: equ 2
.imm_opcode: equ 3 ; note: if it's IMM_16 only the opcode is opcode-1
; This is the "/digit" shown in the intel manual. It goes in the register bits
; in the ModRM byte for instructions with only 1 register argument sometimes.
.extra_opcode: equ 4
.short_reg_opcode: equ 5

db 3,'aaa'
  dw NO_ARG,
  db 0x37,0x00,0x00,0x00,
db 3,'aad'
  dw IMM_8|DEFAULT_10,
  db 0x00,0xD5,0x00,0x00,
db 3,'aam'
  dw IMM_8|DEFAULT_10,
  db 0x00,0xD4,0x00,0x00,
db 3,'aas'
  dw NO_ARG,
  db 0x3F,0x00,0x00,0x00,
db 3,'adc'
  dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY,
  db 0x10,0x80,0x02,0x14,
db 3,'add'
  dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY,
  db 0x00,0x80,0x00,0x04,
db 3,'and'
  dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY,
  db 0x20,0x80,0x04,0x24,
db 4,'call'
  dw IMM_16|REG_16|FAR_JUMP,
  db 0xFE,0xE7,0x02,0x00,
db 3,'cbw'
  dw NO_ARG,
  db 0x98,0x00,0x00,0x00,
db 3,'clc'
  dw NO_ARG,
  db 0xF8,0x00,0x00,0x00,
db 3,'cld'
  dw NO_ARG,
  db 0xFC,0x00,0x00,0x00,
db 3,'cmc'
  dw NO_ARG,
  db 0xF5,0x00,0x00,0x00,
db 3,'cmp'
  dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY,
  db 0x38,0x80,0x07,0x3C,
db 3,'cwd'
  dw NO_ARG,
  db 0x99,0x00,0x00,0x00,
db 3,'daa'
  dw NO_ARG,
  db 0x27,0x00,0x00,0x00,
db 3,'das'
  dw NO_ARG,
  db 0x2F,0x00,0x00,0x00,
db 3,'dec'
  dw ONE_REG|SHORT_REG,
  db 0xFE,0x00,0x01,0x48,
db 3,'div'
  dw ONE_REG,
  db 0xF6,0x00,0x06,0x00,
db 3,'hlt'
  dw NO_ARG,
  db 0xF4,0x00,0x00,0x00,
db 4,'idiv'
  dw ONE_REG,
  db 0xF6,0x00,0x07,0x00,
db 4,'imul'
  dw ONE_REG, ; Note: the other 2 forms weren't on the 8086
  db 0xF6,0x00,0x05,0x00,
db 2,'in'
  dw IMM_8|SHORT_REG_DX_ONLY, ; in `in al/ax, imm8` or `in al/ax, dx` are the only forms
  db 0x00,0xE4,0x00,0xEC,
db 3,'inc'
  dw ONE_REG|SHORT_REG,
  db 0xFE,0x00,0x00,0x40,
db 4,'insb'
  dw NO_ARG,
  db 0x6C,0x00,0x00,0x00,
db 4,'insw'
  dw NO_ARG,
  db 0x6D,0x00,0x00,0x00,
db 3,'int'
  dw IMM_8,
  db 0x00,0xCD,0x00,0x00,
db 4,'into'
  dw NO_ARG,
  db 0xCE,0x00,0x00,0x00,
db 4,'iret'
  dw NO_ARG,
  db 0xCF,0x00,0x00,0x00,
db 4,'lahf'
  dw NO_ARG,
  db 0x9F,0x00,0x00,0x00,
db 3,'lds'
  dw SWAP_TWO_REG|LOAD_ADDR_16_ONLY,
  ; Note: reg_opcode has -3 for these next few because these are `reg16, mem` only, which does opcode+3
  db 0xC5-3,0x00,0x00,0x00,
db 3,'lea'
  dw SWAP_TWO_REG|LOAD_ADDR_16_ONLY,
  db 0x8D-3,0x00,0x00,0x00,
db 3,'les'
  dw SWAP_TWO_REG|LOAD_ADDR_16_ONLY,
  db 0xC4-3,0x00,0x00,0x00,
db 4,'lock'
  dw NO_ARG,
  db 0xF0,0x00,0x00,0x00,
db 4,'loop'
  dw IMM_8,
  db 0x00,0xE2,0x00,0x00,
db 5,'loope'
  dw IMM_8,
  db 0x00,0xE1,0x00,0x00,
db 6,'loopne'
  dw IMM_8,
  db 0x00,0xE0,0x00,0x00,
db 3,'mov'
  ; Note: there's also an 8bit short-reg op for this but we don't support it
  dw TWO_REG|SWAP_TWO_REG|REG_IMM|SHORT_REG|MOV_SEGMENT,
  db 0x88,0xC6,0x00,0xB8,
db 3,'mul'
  dw ONE_REG,
  db 0xF6,0x00,0x04,0x00,
db 3,'neg'
  dw ONE_REG,
  db 0xF6,0x00,0x03,0x00,
db 3,'nop'
  dw NO_ARG,
  db 0x90,0x00,0x00,0x00,
db 3,'not'
  dw ONE_REG,
  db 0xF6,0x00,0x02,0x00,
db 2,'or'
  dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY,
  db 0x08,0x80,0x01,0x0C,
db 3,'out'
  dw IMM_8|SHORT_REG_DX_ONLY,
  db 0x00,0x00,0x00,0x00,
db 5,'outsb'
  dw NO_ARG,
  db 0x6E,0x00,0x00,0x00,
db 5,'outsw'
  dw NO_ARG,
  db 0x6F,0x00,0x00,0x00,
db 3,'pop'
  dw REG_16|SHORT_REG, ; Note: there are special segment register opcodes we don't support
  db 0x8F,0x00,0x00,0x58,
db 4,'popf'
  dw NO_ARG,
  db 0x9D,0x00,0x00,0x00,
db 4,'push'
  dw REG_16|IMM_16|SHORT_REG, ; Note: there are special segment register opcodes plus it has IMM_8 just not following the pattern
  db 0xFF,0x67,0x06,0x50,
db 5,'pushf'
  dw NO_ARG,
  db 0x9C,0x00,0x00,0x00,
db 3,'rcl'
  dw SHIFT,
  db 0x00,0xC0,0x02,0x00,
db 3,'rcr'
  dw SHIFT,
  db 0x00,0xC0,0x03,0x00,
db 3,'ret'
  dw NO_ARG|IMM_16,
  db 0xC3,0xC1,0x00,0x00,
db 4,'retf'
  dw NO_ARG|IMM_16,
  db 0xCB,0xC9,0x00,0x00,
db 3,'rol'
  dw SHIFT,
  db 0x00,0xC0,0x00,0x00,
db 3,'ror'
  dw SHIFT,
  db 0x00,0xC0,0x01,0x00,
db 4,'sahf'
  dw NO_ARG,
  db 0x9E,0x00,0x00,0x00,
db 3,'sal'
  dw SHIFT,
  db 0x00,0xC0,0x04,0x00,
db 3,'sar'
  dw SHIFT,
  db 0x00,0xC0,0x07,0x00,
db 3,'sbb'
  dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY,
  db 0x18,0x80,0x03,0x1C,
db 3,'shl'
  dw SHIFT,
  db 0x00,0xC0,0x04,0x00,
db 3,'shr'
  dw SHIFT,
  db 0x00,0xC0,0x05,0x00,
db 3,'stc'
  dw NO_ARG,
  db 0xF9,0x00,0x00,0x00,
db 3,'std'
  dw NO_ARG,
  db 0xFD,0x00,0x00,0x00,
db 3,'sub'
  dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY,
  db 0x28,0x80,0x05,0x2C,
db 4,'test'
  dw REG_IMM|TWO_REG|SHORT_REG_AL_ONLY,
  db 0x84,0xF6,0x00,0xA8,
db 4,'xchg'
  dw TWO_REG, ; Note: also has a short-reg with just al we don't support because it's different
  db 0x86,0x00,0x00,0x00,
db 5,'xlatb'
  dw NO_ARG,
  db 0xD7,0x00,0x00,0x00,
db 3,'xor'
  dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY,
  db 0x30,0x80,0x06,0x34,
db 0

; Special case opcodes
SHIFT_ONE_OPCODE: equ 0xD0
SHIFT_CL_OPCODE:  equ 0xD2
JMP_SHORT_OPCODE: equ 0xEB
JMP_NEAR_OPCODE:  equ 0xE9
JMP_REG_OPCODE:   equ 0xFF
JMP_EXTRA_OPCODE: equ 0x04
READ_SEGMENT_OPCODE:  equ 0x8C
WRITE_SEGMENT_OPCODE: equ 0x8E

; Supported args for jumps
JCC_FLAGS: equ ONE_IMM
JMP_FLAGS: equ ONE_IMM|REG_16|FAR_JUMP

; Table schema: instruction_len,instruction,opcode
;
; Note: These are the short (8bit) jcc opcodes. To get the near (16bit) jcc
; opcodes, prefix with 0F and add 0x10.
jcc_instructions:
.extra_data_size: equ 1
db 2,'ja',   0x77,
db 3,'jae',  0x73,
db 2,'jb',   0x72,
db 3,'jbe',  0x76,
db 2,'jc',   0x72,
db 4,'jcxz', 0xE3, ; Note: Special because it has no "near" jump opcode
db 2,'je',   0x74,
db 2,'jg',   0x7F,
db 3,'jge',  0x7D,
db 2,'jl',   0x7C,
db 3,'jle',  0x7E,
db 3,'jna',  0x76,
db 4,'jnae', 0x72,
db 3,'jnb',  0x73,
db 4,'jnbe', 0x77,
db 3,'jnc',  0x73,
db 3,'jne',  0x75,
db 3,'jng',  0x7E,
db 4,'jnge', 0x7C,
db 3,'jnl',  0x7D,
db 4,'jnle', 0x7F,
db 3,'jno',  0x71,
db 3,'jnp',  0x7B,
db 3,'jns',  0x79,
db 3,'jnz',  0x75,
db 2,'jo',   0x70,
db 2,'jp',   0x7A,
db 3,'jpe',  0x7A,
db 3,'jpo',  0x7B,
db 2,'js',   0x78,
db 2,'jz',   0x74,
db 0

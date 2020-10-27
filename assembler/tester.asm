%include "bootloader/bootsect.asm"

; Segment register value (so the actual start is at the address 0x10*this)
; This is the first sector after the editor's code
%define OUTPUT_LOC (CODE_SEGMENT+(SECTOR_SIZE/0x10)*(NUM_EXTRA_SECTORS+1))

start_:

  ; Set the input [es:si] to the included test assembly
  mov ax, input_code
  shr ax, 4 ; segment registers are addr/0x10
  add ax, CODE_SEGMENT
  mov ds, ax ; = (CODE_SEGMENT+(input_code/0x10)) nasm can't calculate this statically because they do linker stuff with labels
  mov si, input_code
  and si, 0x000F ; We need to keep the lower bits we shifted away if it's not aligned
  ; Setup our assembler output location
  mov ax, OUTPUT_LOC
  mov es, ax
  xor di, di

  ; TODO: add a debugging hook into assemble to let us know which line we didn't
  ;       match on
  call assemble
  test bx, bx
  jnz error

  ; If the length is wrong we don't even need to compare
  mov cx, golden_output_len
  cmp di, cx
  jne fail

  ; Check if we wrote the same bytes as the host assembler
  xor di, di
  compare_loop:
  mov al, [es:di]
  cmp [cs:di+golden_output], al
  jne fail
  cmp di, cx
  je pass
  inc di
  jmp compare_loop

  jmp $

pass:
  mov ax, cs
  mov es, ax
  mov ax, 0x1301 ; write string with move cursor
  mov bp, pass_str
  mov cx, result_len
  xor dx, dx
  xor bh, bh
  mov bl, 0x02 ; black bg green text
  int 0x10

  jmp $

fail:
  mov ax, cs
  mov es, ax
  mov ax, 0x1301 ; write string with move cursor
  mov bp, fail_str
  mov cx, result_len
  xor dx, dx
  xor bh, bh
  mov bl, 0x04 ; black bg red text
  int 0x10
  jmp $

error:
  push dx ; save the error line number

  mov ax, cs
  mov es, ax
  mov ax, 0x1301 ; write string with move cursor
  mov bp, bx ; the read pointer is [es:bp]
  ; cx is already right
  xor dx, dx ; top left corner
  xor bh, bh
  mov bl, 0x4F ; white on red text
  int 0x10

  ; Print the line number label
  mov bp, line_num_str
  mov cx, line_num_str_len
  mov dx, 0x0100 ; second line
  mov bl, 0x07 ; black bg grey text
  int 0x10
  ; Print the line number
  pop cx
  call print_hex

  jmp $

%include "assembler/assemble.asm"

line_num_str: db "On line: "
line_num_str_len: equ $-line_num_str

result_len: equ 4
pass_str: db "PASS"
fail_str: db "FAIL"

; The code we'll run through our assembler
input_code:
incbin "assembler/test_assembly.asm"
db 0

; The output from the host OS assembler for the test code
golden_output:
%include "assembler/test_assembly.asm"
golden_output_len: equ $-golden_output

NUM_EXTRA_SECTORS: equ NUM_SECTORS(start_)

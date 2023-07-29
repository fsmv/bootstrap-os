%include "bootloader/bootsect.asm"

; Segment register value (so the actual start is at the address 0x10*this)
; This is the first sector after the editor's code
%define OUTPUT_LOC (CODE_SEGMENT+(SECTOR_SIZE/0x10)*(NUM_EXTRA_SECTORS+1))

; defined by the video mode we're using
%define NUM_ROWS 25
%define NUM_COLS 80

%define BIOS_PRINT_STRING 0x1301
%define BIOS_PRINT_CHAR 0x0E

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

  ; save the first location in the file for the callback to use
  mov [cs:last_instruction_input], si
  dec word [cs:last_instruction_input] ; -1 because normally this points to a line ending we need to skip

  %define TESTER_CALLBACK
  call assemble ; calls test_assembled_instruction for each line
  test bx, bx
  jnz error

  cmp byte [cs:num_errors], 0
  je pass

  ; Clear the bottom row which might have the text saying there's another error
  ; Do it by scrolling so we don't need to move the cursor or have a spaces string)
  mov ah, 0x06
  xor cl, cl
  mov ch, NUM_ROWS-1
  mov dh, NUM_ROWS-1
  mov dl, NUM_COLS-1
  mov al, 1
  int 0x10

  jmp $

pass:
  mov ax, cs
  mov es, ax
  mov ax, BIOS_PRINT_STRING
  mov bp, pass_str
  mov cx, pass_str_len
  mov dx, pass_pos
  xor bh, bh
  mov bl, 0x02 ; black bg green text
  int 0x10

  ; Hide the cursor by moving it off screen
  mov ah, 0x02
  mov dx, 0xFFFF
  xor bh, bh
  int 0x10 ; dx is the cursor position

  jmp $

error:
  push dx ; save the error line number

  mov ax, cs
  mov es, ax
  mov ax, BIOS_PRINT_STRING
  mov bp, bx ; the read pointer is [es:bp]
  ; cx is already right
  xor dx, dx ; top left corner
  xor bh, bh
  mov bl, 0x4F ; white on red text
  int 0x10

  ; Print the line number label
  mov bp, line_num_str
  mov cx, line_num_str_len
  ; TODO: Move the line number down if the string is longer than one line
  mov dx, 0x0100 ; second line
  mov bl, 0x07 ; black bg grey text
  int 0x10
  ; Print the line number
  pop cx
  call print_hex

  jmp $

last_instruction_input: dw 0x0000
last_instruction_output: dw 0x0000
generated_code_start: dw 0x0000
num_errors: db 0x00

save_output_code_start:
  mov [cs:generated_code_start], di
  mov [cs:last_instruction_output], di
  ret

; Arguments: (values must be preserved)
;  - [ds:si] - the end of line char for the instruction line
;  - [es:di] - one past the end of the newly assembled code
;  - cx      - the line number of the instruction
test_assembled_instruction:
  push bp
  ; Check if we wrote the same bytes as the host assembler
  mov bx, [cs:last_instruction_output]
  mov bp, bx
  sub bp, [cs:generated_code_start]
  .compare_loop:
  cmp bx, di
  je .done
  mov al, [es:bx]
  cmp [cs:bp+golden_output], al
  jne .fail
  inc bx
  inc bp
  jmp .compare_loop

  .color: equ 0x0007 ; grey text on black background

.fail:
  push cx ; save the line number

  ; Set es = cs since we're going to BIOS_PRINT_STRING
  mov ax, cs
  mov es, ax

  ; Support scrolling for more errors than the screen fits
  mov dh, [cs:num_errors] ; row = the number of errors printed
  cmp byte [cs:num_errors], NUM_ROWS-2 ; -1 for 0-indexing, -1 for one before the end
  jbe .print_mismatch

  ; If we hit the bottom of the screen we wait for the user to hit enter to show
  ; the next line
  mov ax, BIOS_PRINT_STRING
  mov bx, .color
  xor dl, dl ; print from column 0
  mov dh, NUM_ROWS-1
  mov bp, scroll_str
  mov cx, scroll_str_len
  int 0x10
  ; Read keyboard
  mov ah, 0x00
  int 0x16
  ; Scroll the errors up leaving space at the bottom
  mov ah, 0x06 ; BIOS scroll up
  mov bh, bl ; this call takes the color in bh instead of bl
  xor cx, cx ; start from the top left corner
  ; end the scroll area at one row from the bottom right
  mov dh, NUM_ROWS-2
  mov dl, NUM_COLS-1
  mov al, 1 ; scroll up 1 line
  int 0x10

  mov dh, NUM_ROWS-2 ; print this error in the space we just made
  ; fallthrough

.print_mismatch:

  xor dl, dl ; print from column 0
  mov ax, BIOS_PRINT_STRING
  mov bx, .color
  mov bp, got_str
  mov cx, got_str_len
  int 0x10
  add dl, got_str_len

  ; print the code we just assembled
  mov ax, OUTPUT_LOC
  mov es, ax ; restore the segment register for the code output
  mov bp, [cs:last_instruction_output]
  call print_bytecode

  ; set es = cs since we need to print some strings and BIOS uses es:bp for this
  mov ax, cs
  mov es, ax
  mov ax, BIOS_PRINT_STRING
  mov bp, want_str
  mov cx, want_str_len
  int 0x10
  add dl, want_str_len

  ; print the want bytecode
  push di
  mov bp, [cs:last_instruction_output]
  sub bp, [cs:generated_code_start]
  add bp, golden_output ; this buffer doesn't start at 0 in cs
  add di, golden_output ; the stop point has to be relative to the start too
  sub di, [cs:generated_code_start]
  call print_bytecode
  pop di

  mov ax, BIOS_PRINT_STRING
  mov bp, line_num_str
  mov cx, line_num_str_len
  int 0x10
  add dl, line_num_str_len

  ; Print the line number and leave it on the stack
  mov bp, sp
  mov cx, [bp]
  call print_hex ; this is the bootloader call to print all of cx
  add dl, 4 ; we just printed 4 chars

  mov ah, BIOS_PRINT_CHAR
  mov al, ' '
  int 0x10
  mov al, '|'
  int 0x10
  mov al, ' '
  int 0x10
  add dl, 3 ; maintain the cursor

  ; Print the code for the instruction we didn't match on
  mov bp, [cs:last_instruction_input]
  inc bp
  mov ax, ds
  mov es, ax ; es = ds
  ; cx = min(line_end-line_start, NUM_COLS-cursor_col)
  mov cx, si
  sub cx, bp
  mov ax, NUM_COLS
  sub al, dl
  cmp cx, ax
  jbe .no_clamp
  mov cx, ax
  .no_clamp:
  mov bx, .color
  mov ax, BIOS_PRINT_STRING
  int 0x10

  pop cx ; restore the line number
  ; restore the segment register
  mov ax, OUTPUT_LOC
  mov es, ax

  inc byte [cs:num_errors]

  ; fallthrough
  .done:
  pop bp
  mov [cs:last_instruction_input], si
  mov [cs:last_instruction_output], di
  ret

; print hex bytes starting from [es:bp] up until [es:di]
; increments dl by the number of chars printed
print_bytecode:
  .loop:
  cmp bp, di
  je .done
  mov ch, [es:bp]
  call print_byte
  inc bp
  jmp .loop
  .done:
  ret

print_byte:
  mov ah, BIOS_PRINT_CHAR
  ; Nibble 0 (most significant)
  mov al, ch
  shr al, 4
  call _print_hex_char
  ; Nibble 1
  mov al, ch
  and al, 0x0F
  call _print_hex_char
  mov al, ' '
  int 0x10

  add dl, 3 ; move the cursor column
  ret

%include "assembler/assemble.asm"

line_num_str: db "line: "
line_num_str_len: equ $-line_num_str

got_str: db "got: "
got_str_len: equ $-got_str

want_str: db "want: "
want_str_len: equ $-want_str

scroll_str: db "Press any key to see the next error..."
scroll_str_len: equ $-scroll_str

; 21 spaces to let us center the 38 chars wide str in 80 chars with one print
pass_str:
db "                     ########  ########  ########  ########",`\n\r`
db "                     ##    ##  ##    ##  ##        ##      ",`\n\r`
db "                     ##    ##  ##    ##  ##        ##      ",`\n\r`
db "                     ########  ########  ########  ########",`\n\r`
db "                     ##        ##    ##        ##        ##",`\n\r`
db "                     ##        ##    ##        ##        ##",`\n\r`
db "                     ##        ##    ##        ##        ##",`\n\r`
db "                     ##        ##    ##  ########  ########"
pass_str_len: equ $-pass_str
pass_pos: equ 0x0800 ; row 8 column 0 to get it centered

; The code we'll run through our assembler
input_code:
incbin "assembler/test_assembly.asm"
db 0

; The output from the host OS assembler for the test code
golden_output:
%include "assembler/test_assembly.asm"
golden_output_len: equ $-golden_output

NUM_EXTRA_SECTORS: equ NUM_SECTORS(start_)

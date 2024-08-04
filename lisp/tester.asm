%include "bootloader/bootsect.asm"

extra_sectors_start:

%include "lisp/lisp.asm"

%define LISP_DATA_LOC (CODE_SEGMENT+(SECTOR_SIZE/0x10)*(NUM_EXTRA_SECTORS+1))
%define OUTPUT_LOC LISP_DATA_LOC+NEXT_SEGMENT

start_:
  ; set [es:si] to the lisp code to run
  mov ax, CODE_SEGMENT
  mov es, ax
  mov si, input_code
  ; Set the output buffer location
  mov word [cs:output_seg], OUTPUT_LOC
  mov word [cs:output_addr], 0
  xor di, di
  ; Set the lisp data location
  mov ax, LISP_DATA_LOC
  mov ds, ax

  call setup_env

  .test_loop:
  mov ax, [cs:output_addr]
  push ax

  call repl_step

  ; TODO: stolen from scan:
  .skip_spaces:
  cmp byte [es:si], ' '
  ja .past_spaces ; all ascii chars less than ' ' are whitespace
  cmp byte [es:si], 0
  je .end_of_file
  inc si
  jmp .skip_spaces
  .past_spaces:

  cmp byte [es:si], 'w'
  jne .test_file_error
  cmp byte [es:si+1], 'a'
  jne .test_file_error
  cmp byte [es:si+2], 'n'
  jne .test_file_error
  cmp byte [es:si+3], 't'
  jne .test_file_error
  cmp byte [es:si+4], ':'
  jne .test_file_error
  cmp byte [es:si+5], ' '
  jne .test_file_error
  add si, 6

  ; Compare [es:si] with the output at [ds:di]
  pop di
  push si
  push di
  mov ax, OUTPUT_LOC
  mov ds, ax
  xor cx, cx
  .compare_loop:

  mov byte al, [es:si]
  cmp byte al, [ds:di]
  jne .fail

  cmp byte [ds:di], `\n`
  je .match
  cmp byte [ds:di], 0
  je .match

  inc di
  inc si
  jmp .compare_loop

  .fail:

  ; common print_string args
  xor dx, dx
  mov bx, 7 ; black background ; grey text

  mov bp, fail_str
  mov cx, fail_str_len
  call print_string
  add dx, fail_str_len

  ; Print the got string
  mov ax, ds
  mov es, ax
  pop di
  mov bp, di
  .str_len:
  inc bp
  cmp byte [es:bp], `\n`
  jne .str_len
  mov cx, bp
  sub cx, di
  mov bp, di
  call print_string
  add dx, cx

  mov ax, CODE_SEGMENT
  mov es, ax
  mov bp, want_str
  mov cx, want_str_len
  call print_string
  add dx, want_str_len

  pop si
  mov bp, si
  .str_len2:
  inc bp
  cmp byte [es:bp], `\n`
  je .found_len
  cmp byte [es:bp], 0
  je .found_len
  jmp .str_len2
  .found_len:
  mov cx, bp
  sub cx, si
  mov bp, si
  call print_string

  mov al, `\n`
  call print_char

  jmp halt

  ; fallthrough
  .match:
  pop ax ; clear the stack

  ; Reset and go again
  mov ax, LISP_DATA_LOC
  mov ds, ax
  xor di, di
  call setup_env
  cmp byte [es:si], 0
  jne .test_loop
  .end_of_file:

  jmp pass



  .test_file_error: ; TODO
  ;fallthrough

halt:
%ifdef HEADLESS
  ; qemu shutdown
  mov ax, 0x2000
  mov dx, 0x604
  out dx, ax
  ; older qemu shutdown
  mov dx, 0xB004
  out dx, ax
%endif
  jmp $

print_char:
%ifdef DEBUGCON
  out QEMU_DEBUG_PORT, al
%endif

  mov ah, BIOS_PRINT_CHAR
  int 0x10
  ret

; [es:bp] = string to print
; cx      = length to print
; dx      = cursor position to print (doesn't affect qemu)
; bx      = color (doesn't affect qemu)
print_string:
%ifdef DEBUGCON
  push cx
  push si
  xor si, si
  .string_loop:
  mov byte al, [es:bp+si]
  out QEMU_DEBUG_PORT, al
  dec cx
  inc si
  test cx, cx
  jnz .string_loop
  pop si
  pop cx
%endif

  mov ax, BIOS_PRINT_STRING
  int 0x10
  ret

pass:
  mov ax, cs
  mov es, ax
  mov bp, pass_str
  mov cx, pass_str_len
  mov dx, pass_pos
  mov bl, 0x02 ; black bg green text
  xor bh, bh
  call print_string

  ; Hide the cursor by moving it off screen
  mov ah, 0x02
  mov dx, 0xFFFF
  xor bh, bh
  int 0x10 ; dx is the cursor position

  jmp halt

fail_str: db "FAIL! got: "
fail_str_len: equ $-fail_str

want_str: db " want: "
want_str_len: equ $-want_str

; 21 spaces to let us center the 38 chars wide str in 80 chars with one print
pass_str:
db "                     ########  ########  ########  ########",`\n\r`
db "                     ##    ##  ##    ##  ##        ##      ",`\n\r`
db "                     ##    ##  ##    ##  ##        ##      ",`\n\r`
db "                     ########  ########  ########  ########",`\n\r`
db "                     ##        ##    ##        ##        ##",`\n\r`
db "                     ##        ##    ##        ##        ##",`\n\r`
db "                     ##        ##    ##        ##        ##",`\n\r`
db "                     ##        ##    ##  ########  ########",`\n`
pass_str_len: equ $-pass_str
pass_pos: equ 0x0800 ; row 8 column 0 to get it centered

input_code:
incbin "lisp/data-for-test.lisp"
db 0

NUM_EXTRA_SECTORS: equ NUM_SECTORS(extra_sectors_start)

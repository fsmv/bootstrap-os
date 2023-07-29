; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andrew Kallmeyer <ask@ask.systems>

%include "bootloader/bootsect.asm"

; This is the start for calculating NUM_EXTRTA_SECTORS
; There must be nothing other than the bootsector above this label
extra_sectors_start:

%define RUN_CODE
%include "text-editor.asm"
%include "assembler/assemble.asm"

run_code:
  call assemble

  test bx, bx
  jz .no_error
  ; cx is already the arg values we need already
  mov bp, bx ; print_error wants bp (because that's what the BIOS wants)
  call print_error
  jmp $ ; TODO: Repaint the screen and go back to typing_loop.
        ;       Also maybe need to implement jumping to the line with the error.

  .no_error:

%if 1
  ; Placeholder: just print the code we outputted instead of running it
  mov si, di
  xor di, di
  xor bp, bp ; print count
  mov dx, MAIN_TOP_LEFT
  .print_loop:
  cmp si, di
  je .end
  mov byte ch, [es:di]
  inc di

  mov ah, 0x0E ; Scrolling teletype BIOS routine (used with int 0x10)
  xor bx, bx ; Clear bx. bh = page, bl = color
  ; Nibble 0 (most significant)
  mov al, ch
  shr al, 4
  call _print_hex_char
  ; Nibble 1
  mov al, ch
  and al, 0x0F
  call _print_hex_char

  ; print a space
  mov ah, 0x0E ; Scrolling teletype BIOS routine (used with int 0x10)
  xor bx, bx ; Clear bx. bh = page, bl = color
  mov al, ' '
  int 0x10

  inc bp
  cmp bp, ROW_LENGTH/3
  ja .next_row
  jmp .print_loop
  .next_row:
  ; Set the cursor position to the start of the next row
  inc dh
  mov ah, 0x02
  xor bh, bh
  int 0x10
  jmp .print_loop

  .end:
  jmp $
%endif

  ; far jump to the assembled code
  mov ax, es
  mov ds, ax ; also reset ds to the code start
  push ax
  xor ax, ax
  push ax
  retf

; The -1 is because we don't want the next sector until we have 512+1 bytes
; e.g. for exactly 512 bytes we want 1 extra sector not 2
;
; The +1 is because int division does floor() and we want ceil()
NUM_EXTRA_SECTORS: equ NUM_SECTORS(extra_sectors_start)

org 0x100

; Set video mode, 16 color 80x25 chars
; This is supported by CGA cards and PCjr
;
; The IBM BIOS manual describes a long procedure for determining which video
; modes are supported and all the possible options for supporting both mono and
; color. Sometimes mono isn't supported if the PC supports color modes. So, I'm
; just going to assume all hardware this is used on supports color modes.
mov ax, 0x0003
int 0x10

; Set cursor type to an underline
mov ax, 0x0100
mov cx, 0x0607
int 0x10

; Set the cursor location
mov dx, 0x0C27
mov ah, 0x02
xor bh, bh
int 0x10 ; dx is the cursor position

mov ax, cs
mov es, ax
mov bp, hello
mov cx, hello_len
mov bl, 0x0D
call print_string

; Set the cursor location off screen
mov dx, 0x1950
mov ah, 0x02
xor bh, bh
int 0x10 ; dx is the cursor position

jmp $

hello: db "Hello, PC!"
hello_len: equ $-hello

%define BIOS_PRINT_CHAR 0x09
%define BIOS_MOVE_CURSOR 0x02

; Mirrors the ah = 0x13 int 0x10 BIOS call which was added in PC AT (and later
; back ported to PC XT) but it works on the original PC.
;
; Also writes to qemu stdout if DEBUGCON is defined.
;
; Only supports \n and \r, other tty chars may be missing.
;
; All registers except for ax are restored like BIOS calls.
;
; [es:bp] = string to print
; cx      = length to print
; dx      = cursor position to print (doesn't affect qemu debug terminal output)
; bl      = color (doesn't affect qemu debug terminal output)
; Works on the original PC
print_string:
  test cx, cx
  jnz .not_zero
  ret
  .not_zero:

  push bx
  push cx
  push dx
  push bp
  push si
  push di

  ; Move the cursor to dx
  mov ah, BIOS_MOVE_CURSOR
  xor bh, bh ; page 0
  int 0x10

  mov di, cx
  xor si, si
  .string_loop:
  mov byte al, [es:bp+si] ; read the char

  ; qemu stdout if enabled
  %ifdef DEBUGCON
  out QEMU_DEBUG_PORT, al
  %endif

  cmp al, `\n`
  je .new_line
  cmp al, `\r`
  je .start_line

  mov cx, 1
  mov ah, BIOS_PRINT_CHAR
  int 0x10
  inc dl ; advance the cursor forward one
  jmp .next_char

  .new_line:
  inc dh
  jmp .next_char

  .start_line:
  xor dl, dl
  ; fallthrough

  .next_char:
  mov ah, BIOS_MOVE_CURSOR
  int 0x10

  inc si
  dec di
  jnz .string_loop

  pop di
  pop si
  pop bp
  pop dx
  pop cx
  pop bx
  ret

; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andrew Kallmeyer <ask@ask.systems>
%include "util/bootsect-header.asm"

%define USER_CODE_LOC 0x500

; Values in ax after the keyboard read BIOS call
; See Figure 4-3 of the 1987 BIOS manual. (page 195)
%define EOT 0x2004 ; Ctrl+D

%define MAIN_COLOR 0x17
%define BORDER_COLOR 0x91
%define MAIN_TOP_LEFT 0x0110 ; row = 1,  col = 16
%define MAIN_BOTTOM_RIGHT 0x173F ; row = 23, col = 79-16

; Set video mode, 16 color 80x25 chars
;
; The IBM BIOS manual describes a long procedure for determining which video
; modes are supported and all the possible options for supporting both mono and
; color. Sometimes mono isn't supported if the PC supports color modes. So, I'm
; just going to assume all modern hardware supports color modes.
mov ax, 0x0003
int 0x10

; Make the 8th bit of the colors bg-intensity instead of blink
mov ax, 0x1003
mov bl, 0
int 0x10

; Make the 4th bit of the colors fg-intensity instead of font selection
; Use block 0 for the font
; Apparently this is the default in VGA so maybe we don't need it
mov ax, 0x1103
mov bl, 0
int 0x10

; Color byte is: bg-intensity,bg-r,bg-g,bg-b ; fg-intensity,fg-r,fg-g,fg-b

; Set cursor shape to an underline
mov ax, 0x0100
mov cx, 0x0607
int 0x10

; Set the border color (by clearing the whole screen)
mov ax, 0x0600
xor cx, cx     ; row = 0,  col = 0
mov dx, 0x184F ; row = 24, col = 79
mov bh, BORDER_COLOR
int 0x10

; Set the background color (by clearing just the middle)
mov ax, 0x0600
mov cx, MAIN_TOP_LEFT
mov dx, MAIN_BOTTOM_RIGHT
mov bh, MAIN_COLOR
int 0x10

; Print the starting greeting
mov ax, 0x1301 ; Write String, move cursor mode in al
mov bp, greeting ; String pointer in es:bp (es is at code start from bootsect-header.asm)
mov cx, greeting_len ; Streng length
mov dx, 0x0010 ; row = 0, col = 16
mov bx, BORDER_COLOR ; bh = 0 (page number); bl = color
int 0x10

; Set cursor position to the start
mov ax, 0x0200
mov dx, 0x0110 ; row = 0, col = 16
xor bh, bh ; page 0
int 0x10

; --- Global register variables ---
;
; dx      - cursor position (set above)
; [es:di] - the code the user types
; cl      - storage for one byte while the user types it out (2 chars per byte)
; ch      - non-zero iff the first nibble is written (the nibble could be zero)
mov ax, USER_CODE_LOC
mov es, ax
xor di, di
xor cx, cx

; Note: all of the jumps loop back here except for run_code
typing_loop:
    ; Read keyboard
    mov ah, 0x00
    int 0x16
    ; ah = key code, al = ascii value

    ; For the consecutive range checks below, we save an add instruction by
    ; doing a bit of algebra and packing it into the next sub instruction
    ; (letting the assembler do the work statically).

    sub al, 'a'
    cmp al, 'f'-'a'
    jbe save_and_print_hex_letter ; jump if al is between 'a' and 'f'

    sub al, 'A'-'a' ; -'a' compensates for the sub al, 'a' instruction above
    cmp al, 'F'-'A'
    jbe save_and_print_hex_letter

    sub al, '0'-'A'
    cmp al, '9'-'0'
    jbe save_and_print_hex_number
    add al, '0' ; We're done with the range checks so we don't need the offset

    cmp ax, EOT
    je run_code

    jmp typing_loop ; Not a hex character or EOF

save_and_print_hex_letter:
    mov bl, al
    add bl, 0xA ; bl = nibble value (al = 0x0 if input was 'A' or 'a')
    add al, 'A' ; al = the character to print
    jmp save_and_print_nibble

save_and_print_hex_number:
    mov bl, al  ; bl = nibble value
    add al, '0' ; al = the character to print
save_and_print_nibble:
    or cl, bl ; put the nibble into the low half of cl

    test ch, ch ; Check the sentinel value in ch
    jnz save_and_print_second_nibble

; save_and_print_first_nibble:
    shl cl, 4 ; Shift the first nibble to the high half
    mov ch, 0xFF ; Signal that we have seen the first nibble

    ; Print the first nibble (in al)
    mov ah, 0x0E ; Write teletype character
    xor bx, bx
    int 0x10
    inc dl ; Update the cursor location
    ; Note: the first nibble never causes a new line

    jmp typing_loop

save_and_print_second_nibble:
    ; Save the byte
    mov [es:di], cl
    inc di

    xor cx, cx ; Clear the temp storage for the next byte

    ; Print the second nibble (in al)
    mov ah, 0x0E ; Write teletype character
    xor bx, bx
    int 0x10

    cmp dl, 0x26 ; The column of the last char of the 8th byte
    je extra_space

    cmp dl, 0x3F ; The last column of the row
    je new_line

    ; Normal case, the char printed plus 1 space
    add dl, 2
    jmp set_cursor

extra_space: ; Put two spaces in the middle
    add dl, 3
    jmp set_cursor

new_line:
    inc dh ; Next row
    mov dl, 0x10 ; Beginning column

    cmp dh, 24 ; if we're at the bottom
    jne set_cursor
    ; if we're at the bottom scroll
    push cx
    push dx

    mov ax, 0x0601 ; scroll up one line
    mov cx, MAIN_TOP_LEFT
    mov dx, MAIN_BOTTOM_RIGHT
    mov bh, MAIN_COLOR
    int 0x10

    pop dx
    pop cx

    dec dh ; Back up one since we scrolled
set_cursor:
    mov ah, 0x02
    xor bh, bh
    int 0x10 ; dx is the cursor position

    jmp typing_loop

run_code:
    ; Set video mode, 16 color 80x25 chars
    mov ax, 0x0003
    int 0x10

    jmp USER_CODE_LOC:0x00

greeting: db `Write your x86 (16-bit real mode) hex here:\r\n`
greeting_len: equ $-greeting

%include "util/bootsect-footer.asm"

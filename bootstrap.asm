; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andrew Kallmeyer <ask@ask.systems>
%include "util/bootsect-header.asm"

%define SECTOR_SIZE 0x200

; Segment register value. Actual location is 0x00500
%define USER_CODE_LOC 0x0050
%define USER_CODE_MAX 0x7700 ; 0x7C00 (boot sector loc) - 0x0500

; Values in ax after the keyboard read BIOS call
; See Figure 4-3 of the 1987 BIOS manual. (page 195)
%define EOT 0x2004 ; Ctrl+D

%define MAIN_COLOR 0x17
%define BORDER_COLOR 0x91
%define MAIN_TOP_LEFT 0x0210 ; row = 2,  col = 16
%define MAIN_BOTTOM_RIGHT 0x163F ; row = 22, col = 79-16
%define LINE_NUM_TOP_LEFT 0x020B ; row = 2,  col = 11
%define LINE_NUM_BOTTOM_RIGHT 0x160F ; row = 22, col = 15
%define END_ROW 0x17
%define START_COL 0x10
%define LINE_NUM_COL 0x0B

push dx ; save the boot disk number (dl)

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
;mov ax, 0x0600
mov cx, MAIN_TOP_LEFT
mov dx, MAIN_BOTTOM_RIGHT
mov bh, MAIN_COLOR
int 0x10

; Load the code from the extra sectors
mov ah, 0x02
mov al, NUM_EXTRA_SECTORS
mov bx, SECTOR_SIZE ; es:bx is address to write to. es = cs, so write directly after the boot sector
mov cx, 0x0002 ; Cylinder 0; Sector 2 (1 is the boot sector)
pop dx ; the boot disk number (dl)
xor dh, dh ; Head 0
int 0x13

; Check for errors
cmp ax, NUM_EXTRA_SECTORS
je start_

push ax ; push the error code

; Print the error message
mov ax, 0x1301 ; Write String, move cursor mode in al
mov bp, error_msg ; String pointer in es:bp (es is at code start from bootsect-header.asm)
mov cx, error_msg_len ; Streng length
mov dx, 0x0210 ; row = 2, col = 16
mov bx, MAIN_COLOR ; bh = 0 (page number); bl = color
int 0x10

pop cx ; pop the error code
call print_hex ; print the error code

jmp $ ; stop forever

; Turns al into a hex ascii char. Expects it to only be 4 low bits.
; Also assumes ah is already set to 0x0E
_print_hex_char:
    cmp al, 9
    jg .over_9

    add al, '0'
    int 0x10
    ret

.over_9:
    sub al, 10
    add al, 'A'
    int 0x10
    ret

; cx = two bytes to write at current cursor
print_hex:
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
    ; Nibble 2
    mov al, cl
    shr al, 4
    call _print_hex_char
    ; Nibble 3
    mov al, cl
    and al, 0x0F
    call _print_hex_char

    ret

error_msg: db `Error reading additional sectors from disk: `
error_msg_len: equ $-error_msg

%include "util/bootsect-footer.asm"

start_:

; Print the starting greeting
mov ax, 0x1301 ; Write String, move cursor mode in al
mov bp, greeting ; String pointer in es:bp (es is at code start from bootsect-header.asm)
mov cx, greeting_len ; Streng length
mov dx, 0x0010 ; row = 0, col = 16
mov bx, BORDER_COLOR ; bh = 0 (page number); bl = color
int 0x10

; Print the row header (column byte numbers)
mov ax, 0x1301 ; Write String, move cursor mode in al
mov bp, row_header ; String pointer in es:bp
mov cx, row_header_len ; Streng length
mov dx, 0x0110 ; row = 0, col = 16
mov bx, BORDER_COLOR ; bh = 0 (page number); bl = color
int 0x10

; Print the line numbers
; cx = full address for user code
mov cx, USER_CODE_LOC
shl cx, 8
; Set the start row number
mov dh, 0x02
print_line_nums:
    ; Move the cursor to the next line number position
    mov ax, 0x0200
    xor bh, bh
    mov dl, LINE_NUM_COL
    int 0x10

    call print_hex ; print cx
    add cx, 0x10 ; Add 16 bytes to the line number
    inc dh       ; Add one row to the cursor
    cmp dh, END_ROW
    jne print_line_nums

; Set cursor position to the start
mov ax, 0x0200
mov dx, MAIN_TOP_LEFT
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
    mov dl, START_COL

    cmp dh, END_ROW ; if we're at the bottom
    jne set_cursor

    ; if we're at the bottom scroll
    push dx ; save the cursor position
    ; Don't bother saving cx because it is 0

    ; Scroll the user code text area
    mov ax, 0x0601 ; scroll up one line
    mov cx, MAIN_TOP_LEFT
    mov dx, MAIN_BOTTOM_RIGHT
    mov bh, MAIN_COLOR
    int 0x10

    ; Scroll the line numbers
    mov ax, 0x0601 ; scroll up one line
    mov cx, LINE_NUM_TOP_LEFT
    mov dx, LINE_NUM_BOTTOM_RIGHT
    mov bh, BORDER_COLOR
    int 0x10

    pop dx ; restore the cursor position

    ; Set the cursor to the start of the new line number
    dec dh ; Back up one since we scrolled
    mov dl, LINE_NUM_COL
    mov ah, 0x02
    xor bh, bh
    int 0x10

    ; Print the new line number
    mov cx, USER_CODE_LOC
    shl cx, 8
    add cx, di ; Use the current write pointer to know what line we're on
    call print_hex

    mov dl, START_COL ; Set the cursor to the start of the line
    xor cx, cx ; restore the current character (which should be 0 now)

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

greeting: db `Write your x86 (16-bit real mode) hex here:`
greeting_len: equ $-greeting

row_header: db ` 0  1  2  3  4  5  6  7   8  9  A  B  C  D  E  F`
row_header_len: equ $-row_header

NUM_EXTRA_SECTORS: equ ($-start_)/SECTOR_SIZE + 1

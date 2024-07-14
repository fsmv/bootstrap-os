; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andy Kallmeyer <ask@ask.systems>
%define SECTOR_SIZE 0x200

; Segment register value (so the actual start is at the address 0x10*this)
; This is the first sector after the editor's code
%define USER_CODE_LOC (CODE_SEGMENT+(SECTOR_SIZE/0x10)*(NUM_EXTRA_SECTORS+1))
; Max value for di and si in typing_loop (i.e. the user code buffer)
;
; There's a lot of extra memory still after this but I don't want to move around
; the segment register so this is the limit.
;
; Allows 64k of code
%define USER_CODE_MAX 0xFFFF

; Values in ax after the keyboard read BIOS call
; See Figure 4-3 of the 1987 BIOS manual. (page 195)
%define LEFT_ARROW  0x4B00
%define RIGHT_ARROW 0x4D00
%define EOT 0x2004 ; Ctrl+D

%define MAIN_COLOR 0x17
%define BORDER_COLOR 0x91

%define MAIN_TOP_LEFT 0x0210 ; row = 2,  col = 16
%define MAIN_BOTTOM_RIGHT 0x163F ; row = 22, col = 79-16
%define START_ROW 0x02
%define START_COL 0x10
%define END_ROW 0x16
%define END_COL 0x3F

%define LINE_NUM_TOP_LEFT 0x020B ; row = 2,  col = 11
%define LINE_NUM_BOTTOM_RIGHT 0x160F ; row = 22, col = 15
%define LINE_NUM_COL 0x0B

; Bitfield values for ch in typing_loop
%define CURSOR_ON_SECOND_NIBBLE 0b01
%define SAVED_NIBBLE_AT_END 0b10

%include "bootloader/bootsect.asm"

; This is also the start for calculating NUM_EXTRTA_SECTORS
; There must be nothing other than the bootsector above this label
start_:

; Set the border color (by clearing the whole screen)
mov ax, 0x0600
xor cx, cx   ; row = 0,  col = 0
mov dx, 0x184F ; row = 24, col = 79
mov bh, BORDER_COLOR
int 0x10

; Set the background color (by clearing just the middle)
;mov ax, 0x0600
mov cx, MAIN_TOP_LEFT
mov dx, MAIN_BOTTOM_RIGHT
mov bh, MAIN_COLOR
int 0x10

; Print the starting greeting
mov ax, 0x1301 ; Write String, move cursor mode in al
mov bx, BORDER_COLOR ; bh = 0 (page number); bl = color
mov bp, greeting ; String pointer in es:bp (es is at code start from bootsect-header.asm)
mov cx, greeting_len ; Streng length
mov dx, 0x0010 ; row = 0, col = 16
int 0x10

; Print the row header (column byte numbers)
mov bp, row_header ; String pointer in es:bp
mov cx, row_header_len ; Streng length
mov dx, 0x0110 ; row = 1, col = 16
int 0x10

; Print the run instructions
mov bp, run_instr ; String pointer in es:bp
mov cx, run_instr_len ; Streng length
mov dx, 0x1710 ; row = END_ROW+1, col = 16
int 0x10

; Move the cursor for printing the code segment location
mov dh, START_ROW
mov dl, LINE_NUM_COL-4-1 ; Space for "FFFF:"
mov ax, 0x0200
xor bh, bh
int 0x10
; Print the USER_CODE_LOC
mov cx, USER_CODE_LOC
call print_hex
; Print the ":"
mov ah, 0x0E
mov al, ':'
xor bh, bh
int 0x10

; Print the line numbers
; cx = full address for user code
xor cx, cx
print_line_nums:
  ; Move the cursor to the next line number position
  mov ax, 0x0200
  xor bh, bh
  mov dl, LINE_NUM_COL
  int 0x10

  call print_hex ; print cx
  add cx, 0x10 ; Add 16 bytes to the line number

  inc dh     ; Add one row to the cursor
  cmp dh, END_ROW+1
  jne print_line_nums

; Set cursor position to the start
mov ax, 0x0200
mov dx, MAIN_TOP_LEFT
xor bh, bh ; page 0
int 0x10

; --- typing_loop global register variables ---
;
; dx    - cursor position (set above)
; [es:di] - the current position to write to in the user code buffer (di=0 is the beginning)
; [es:si] - the end of the user code buffer (will possibly be only half a byte)
; cl    - storage for the current byte the cursor is on (2 chars per byte)
; ch    - bitfield for state flags about half-bytes (CURSOR_ON_SECOND_NIBBLE | SAVED_NIBBLE_AT_END)
mov ax, USER_CODE_LOC
mov es, ax
xor di, di
xor si, si
xor cx, cx

; Note: all of the jumps in typing_loop loop back here (except for run_code)
;
; I thought it would be fun to save the call and ret instructions since the
; entire program is just this loop (after the setup).
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

  cmp al, `\b` ; Backspace, shift backspace, Ctrl+H
  je move_left

  cmp ax, LEFT_ARROW
  je move_left

  cmp ax, RIGHT_ARROW
  je move_right

  cmp ax, EOT ; Ctrl+D
  je run_code

  jmp typing_loop ; Doesn't match anything above

; ==== typing_loop internal helpers ===

; Takes an ascii char A-F in al then saves and prints it and continues typing_loop
save_and_print_hex_letter:
  mov bl, al
  add bl, 0xA ; bl = nibble value (al = 0x0 if input was 'A' or 'a')
  add al, 'A' ; al = the character to print
  jmp _save_and_print_nibble

; Takes an ascii char 0-9 in al then saves and prints it and continues typing_loop
save_and_print_hex_number:
  mov bl, al  ; bl = nibble value
  add al, '0' ; al = the character to print
  ; fallthrough
; Takes a nibble value in bl and ascii hex character in al
; then saves and prints it and continues typing_loop
_save_and_print_nibble:
  ; Print the nibble ascii char (in al)
  mov ah, 0x0E ; Write teletype character
  xor bh, bh ; Note: cannot clear bl here
  int 0x10

  test ch, CURSOR_ON_SECOND_NIBBLE
  jnz .save_second_nibble
  ; .save_first_nibble
  shl bl, 4 ; Move the nibble into the high half
  and cl, 0x0F ; Clear the high half of the temp byte
  or cl, bl ; Put the nibble into the high half of cl

  cmp di, si
  jne move_right ; move_right if it's not the end
  ; If we're at the end, mark the bit that we have a nibble
  or ch, SAVED_NIBBLE_AT_END
  jmp move_right

  .save_second_nibble:
  ; Save the byte
  and cl, 0xF0 ; clear the low half of the temp byte
  or cl, bl ; put the nibble in the low half of the temp byte

  cmp di, si
  jne move_right ; move_right if we're not at the end
  ; If we're at the end

  cmp si, USER_CODE_MAX
  je typing_loop

  inc si ; move the end of the buffer forward so we can move the cursor (only user typing can do this)
  and ch, ~SAVED_NIBBLE_AT_END ; Clear the bit, we don't have a nibble anymore
  jmp move_right

; Moves the cursor to the value in dx and continues typing_loop
set_cursor_and_continue:
  mov ah, 0x02
  xor bh, bh
  int 0x10 ; dx is the cursor position
  jmp typing_loop

; Moves the cursor left one nibble then continues typing_loop
;  - Saves the byte in cl when moving to a new byte, also loads the existing
;  data into cl if applicable
;  - Calls move_up when moving to the next line
move_left:
  test ch, CURSOR_ON_SECOND_NIBBLE
  jz .previous_byte
  ; We're moving left past the first nibble of a byte,
  ; never need to change lines because of this

  and ch, ~CURSOR_ON_SECOND_NIBBLE
  dec dl
  jmp set_cursor_and_continue

  .previous_byte:
  ; Don't do anything if we're already at the beginning of the buffer
  test di, di
  jz typing_loop

  or ch, CURSOR_ON_SECOND_NIBBLE ; We stepped past the second nibble

  ; store the temp byte in the destination memory
  mov [es:di], cl
  dec di

  mov cl, [es:di] ; Load the previous byte

  cmp dl, 0x29 ; Column of the first char of the 9th byte
  je .extra_space

  cmp dl, START_COL
  je .previous_line

  ; Normal case, the char plus 1 space
  sub dl, 2
  jmp set_cursor_and_continue

  .extra_space: ; Move past the two spaces in the middle
  sub dl, 3
  jmp set_cursor_and_continue

  .previous_line:
  mov dl, END_COL
  jmp _move_up

; Moves the cursor right one nibble then continues typing_loop
;  - Saves the byte in cl when moving to a new byte, also loads the existing
;  data into cl if applicable
;  - Calls move_down when moving to the next line
move_right:
  test ch, CURSOR_ON_SECOND_NIBBLE
  jnz .next_byte
  ; We're moving right to the second nibble
  ; Never have to change lines

  cmp di, si
  jne .move_one_char_forward ; if we're not at the end
  ; we're at the end
  test ch, SAVED_NIBBLE_AT_END
  jnz .move_one_char_forward ; allowed to move one more if there's a nibble
  jmp typing_loop ; this is the end, no move moving right

  .move_one_char_forward:
  or ch, CURSOR_ON_SECOND_NIBBLE ; set the bit
  inc dl
  jmp set_cursor_and_continue

  .next_byte:
  cmp di, si ; note: when the user types a character this is never equal
  je typing_loop ; do nothing if we're already at the end

  ; store the temp byte in the destination memory
  mov [es:di], cl
  inc di

  ; Set the cl value for the next byte
  cmp di, si
  jne .load_byte ; if we moved right and we're not at the end, load the data already entered

  test ch, SAVED_NIBBLE_AT_END ; if we moved to the end and there's a saved nibble there
  jnz .load_byte ; just load the whole byte (the second nibble can be user data if we hit the USER_CODE_MAX)

  ; If we don't take either of the above jumps
  xor cl, cl ; Clear the temp storage for the next byte (maybe not really necessary)
  jmp .move_cursor_to_next_byte

  .load_byte:
  mov cl, [es:di]
  ; fallthrough
  .move_cursor_to_next_byte:
  and ch, ~CURSOR_ON_SECOND_NIBBLE ; Clear this bit in the bitfield

  cmp dl, 0x26 ; The column of the last char of the 8th byte
  je .extra_space

  cmp dl, END_COL
  je .new_line

  ; Normal case, the char printed plus 1 space
  add dl, 2
  jmp set_cursor_and_continue

  .extra_space: ; Put two spaces in the middle
  add dl, 3
  jmp set_cursor_and_continue

  .new_line:
  mov dl, START_COL
  jmp _move_down

; Move the cursor up one line and keep the same column
;  - Does not update the di pointer or cx state
;  - Does not check bounds
;  - Scrolls if necessary
;  - Prints existing data if scolling to a part of the buffer with data
_move_up:
  cmp dh, START_ROW
  je .scroll_down

  dec dh
  jmp set_cursor_and_continue

  .scroll_down:
  mov ah, 0x07 ; BIOS scroll down, means make room at the top
  jmp _scroll_and_continue


; Move the cursor down one line and keep the same column
;  - Does not update the di pointer or cx state
;  - Does not check bounds
;  - Scrolls if necessary
;  - Prints existing data if scolling to a part of the buffer with data
_move_down:
  cmp dh, END_ROW ; if we're at the bottom
  je .scroll_up

  inc dh ; Next row
  jmp set_cursor_and_continue

  .scroll_up:
  mov ah, 0x06 ; BIOS scroll up, means make room at the bottom
  jmp _scroll_and_continue

; Scrolls the text up or down one row printing any existing data in the text
; buffer when it does it
;  - ah = 0x06 for down; ah = 0x07 for up; ah = anything else for hacks
_scroll_and_continue:
  push dx ; save the cursor position
  push di ; save the write pointer
  push cx ; save the current byte storage (must be last)

  mov al, 1 ; scroll one line (shared between the two calls)

  ; Scroll the user code text area
  mov cx, MAIN_TOP_LEFT
  mov dx, MAIN_BOTTOM_RIGHT
  mov bh, MAIN_COLOR
  int 0x10

  ; Scroll the line numbers
  mov cx, LINE_NUM_TOP_LEFT
  mov dx, LINE_NUM_BOTTOM_RIGHT
  mov bh, BORDER_COLOR
  int 0x10

  ; Set the cursor to the start of the new line number
  cmp ah, 0x06
  jne .went_up
  mov dh, END_ROW
  jmp .went_down
  .went_up:
  mov dh, START_ROW
  .went_down:
  mov dl, LINE_NUM_COL
  mov ah, 0x02
  xor bh, bh
  int 0x10

  ; Move the current buffer pos pointer to the beginning of the line
  and di, 0xFFF0

  ; Print the new line number
  mov cx, di
  call print_hex

  ; Move the cursor forward to the start of the line
  add dl, 5
  mov ah, 0x02
  xor bh, bh
  int 0x10

; Prints a whole hex line from the [es:di] up to the end of the line or [es:si]
; Also checks the SAVED_NIBBLE_AT_END flag and prints it
  pop cx ; restore the global cx value (we need the ch flags)
.print_line_loop:
  cmp di, si
  jne .print_whole_byte
  cmp si, USER_CODE_MAX
  je .print_whole_byte ; Just always print the last byte even if the user didn't type there yet
  test ch, SAVED_NIBBLE_AT_END
  jnz .print_one_nibble
  jmp .done

  .print_one_nibble:
  mov al, [es:di]
  shr al, 4
  call print_hex_char
  jmp .done ; no need to skip spaces 

  .print_whole_byte:
  mov al, [es:di]
  shr al, 4
  call print_hex_char

  mov al, [es:di]
  and al, 0x0F
  call print_hex_char

  ;.skip_spaces:
  ; if (di & 0xF) = 7
  mov ax, di
  and ax, 0xF
  cmp al, 7
  jne .one_space
  ;two_spaces:
  inc dl
  ; fallthrough
  .one_space:
  add dl, 3
  mov ah, 0x02
  xor bh, bh
  int 0x10

  ; If we printed the last byte of the line, we're done
  mov ax, di
  and ax, 0xF
  cmp ax, 0xF
  je .done

  inc di
  jmp .print_line_loop

  .done:
  pop di ; restore the write pointer
  pop dx ; restore the cursor position
  jmp set_cursor_and_continue

run_code:
  ; Set video mode, 16 color 80x25 chars
  mov ax, 0x0003
  int 0x10

  ; Reset the data segement to the user buffer
  mov ax, USER_CODE_LOC
  mov ds, ax

  jmp USER_CODE_LOC:0x00

; ==== Data area ====

greeting: db `Write your x86 (16-bit real mode) hex here:`
greeting_len: equ $-greeting

row_header: db ` 0  1  2  3  4  5  6  7   8  9  A  B  C  D  E  F`
row_header_len: equ $-row_header

run_instr: db `Press Ctrl+D to run your code immediately.`
run_instr_len: equ $-run_instr

NUM_EXTRA_SECTORS: equ NUM_SECTORS(start_)

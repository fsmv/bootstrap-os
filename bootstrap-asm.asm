; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andrew Kallmeyer <ask@ask.systems>
%define SECTOR_SIZE 0x200

; Segment register value. Actual location is 0x00500
%define USER_CODE_LOC 0x0050
; Max value for di and si in typing_loop
; Allows ~30k of code.
%define USER_CODE_MAX 0x7C00-0x0500-1 ; Boot code address - code address segment reg offset - 1

; Values in ax after the keyboard read BIOS call
; See Figure 4-3 of the 1987 BIOS manual. (page 195)
%define LEFT_ARROW  0x4B00
%define RIGHT_ARROW 0x4D00
%define EOT 0x2004 ; Ctrl+D

%define MAIN_COLOR 0x17
%define BORDER_COLOR 0x91

%define MAIN_TOP_LEFT 0x0204 ; row = 2,  col = 2
%define MAIN_BOTTOM_RIGHT 0x164B ; row = 22, col = 79-2
%define START_ROW 0x02
%define START_COL 0x04
%define END_ROW 0x16
%define END_COL 0x4B
%define ROW_LENGTH END_COL-START_COL+1

%include "util/bootsect-header.asm"

mov [BOOT_DISK], dl ; Save the boot disk number

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

; Set the keyboard repeat speed
mov ax, 0x0305
;mov bx, 0x0107 ; 500 ms delay before repeat ; 16 characters per second
mov bx, 0x0100 ; 500 ms delay before repeat ; 30 characters per second
int 0x16

; Load the code from the extra sectors
mov ah, 0x02
mov al, NUM_EXTRA_SECTORS
mov bx, SECTOR_SIZE ; es:bx is address to write to. es = cs, so write directly after the boot sector
mov cx, 0x0002 ; Cylinder 0; Sector 2 (1 is the boot sector)
mov dl, [BOOT_DISK]
xor dh, dh ; Head 0
int 0x13

; Check for errors
cmp ax, NUM_EXTRA_SECTORS
je start_

push ax ; push the error code

; Print the error message
mov ax, 0x1301 ; Write String, move cursor mode in al
mov bp, error_msg ; String pointer in es:bp (es is at code start from bootsect-header.asm)
mov cx, error_msg_len ; String length
mov dx, MAIN_TOP_LEFT
mov bx, MAIN_COLOR ; bh = 0 (page number); bl = color
int 0x10

pop cx ; pop the error code
call print_hex ; print the error code

jmp $ ; stop forever

; Prints the ascii hex character which represents the integer value of al
; Only accepts 0x0 <= al <= 0xF, anything else is garbage output
; e.g. al = 12 prints "C"
; clobbers ax, and bx
print_hex_char:
  mov ah, 0x0E
  xor bx, bx
  ; fallthrough
; Also assumes ah = 0x0E and bx = 0
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
; clobbers ax, and bx
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

BOOT_DISK: db 0x00 ; value is filled first thing

%include "util/bootsect-footer.asm"

start_:

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
mov ax, USER_CODE_LOC
mov es, ax
xor di, di
xor si, si

; Note: all of the jumps in typing_loop loop back here (except for run_code)
;
; I thought it would be fun to save the call and ret instructions since the
; entire program is just this loop (after the setup).
typing_loop:
  ; Read keyboard
  mov ah, 0x00
  int 0x16
  ; ah = key code, al = ascii value

  cmp al, ' '
  jl .non_printable
  cmp al, '~'
  jg .non_printable
  jmp save_and_print_char

  .non_printable:

  cmp al, `\b` ; Backspace, shift backspace, Ctrl+H
  je move_left

  cmp al, `\r` ; Enter/Return key
  je save_new_line

  cmp ax, LEFT_ARROW
  je move_left

  cmp ax, RIGHT_ARROW
  je move_right

  jmp typing_loop ; Doesn't match anything above

; ==== typing_loop internal helpers ===

; Takes an ascii char in al then saves and prints it and continues typing_loop
save_and_print_char:
  ; Print the ascii char (in al)
  mov ah, 0x0E ; Write teletype character
  xor bh, bh ; Note: cannot clear bl here
  int 0x10

  mov [es:di], al

  cmp di, si
  jne move_right ; move_right if we're not at the end
  ; If we're at the end

  cmp si, USER_CODE_MAX
  je typing_loop

  mov byte [es:di+1], 0 ; Clear the next byte so we can check for new line in move_right
  inc si ; move the end of the buffer forward so we can move the cursor (only user typing can do this)
  jmp move_right

; Moves the cursor to the value in dx and continues typing_loop
set_cursor_and_continue:
  mov ah, 0x02
  xor bh, bh
  int 0x10 ; dx is the cursor position
  jmp typing_loop

save_new_line:
  ; You can only insert a new line at the end of the buffer for now
  ;
  ; Because insert mode isn't supported yet and I don't want to redraw the
  ; whole screen to support it with replacement mode
  cmp di, si
  jne typing_loop

  cmp di, USER_CODE_MAX
  je typing_loop

  ; Disallow new lines as the first character
  ;
  ; Because in replacement mode we would have to stop the cursor from moving
  ; onto the first line because you wouldn't be able to add any characters
  ; there. This conditional would be non-trivial and it's not worth bothering.
  ; Also if we allowed consecutive new lines that would make this more
  ; complicated.
  test di, di
  jz typing_loop

  ; Disallow consecutive new lines
  ;
  ; Because we might have to scroll the screen more than one line at a time if
  ; we allowed them which means I would have to refactor the scrolling and
  ; printing code. But I'm going to change to non-wrapped lines next and we
  ; won't need that code so it would be a waste to write it.
  cmp byte [es:di-1], `\n`
  je typing_loop

  ; Disallow new lines at the very end of a row
  ;
  ; Because the line has already wrapped. It would create a blank line that
  ; causes the same problems as consecutive new lines and needs some other
  ; code chanes around new line handling.
  cmp dl, END_ROW
  je typing_loop

  mov byte [es:di], `\n`
  inc di
  inc si
  mov byte [es:di], 0 ; Clear the next byte so we can check for \n

  mov dl, START_COL
  mov bp, di
  xor cx, cx
  jmp _move_down

; Moves the cursor left one nibble then continues typing_loop
;  - Saves the byte in cl when moving to a new byte, also loads the existing
;  data into cl if applicable
;  - Calls move_up when moving to the next line
move_left:
  ; Don't do anything if we're already at the beginning of the buffer
  test di, di
  jz typing_loop

  dec di

  cmp byte [es:di], `\n`
  je .prev_line

  ; The line was just wrapped so go up to the next screen line
  cmp dl, START_COL
  je .prev_screen_line

  ; Normal case, just go back one char
  dec dl
  jmp set_cursor_and_continue

  .prev_screen_line:
  ; We know that this was a wrapped line so we know there must be ROW_LENGTH
  ; characters in the buffer to skip past
  ;
  ; If we called .prev_line instead it would actually bounds check but always
  ; get the same answer
  mov dl, END_COL
  mov bp, di
  sub bp, ROW_LENGTH-1 ; set the pointer to print from to the first char of the row
  mov cx, ROW_LENGTH   ; number of characters to print
  jmp _move_up

  .prev_line:
  ; Don't go past the start of the buffer if the first char is \n
  test di, di
  jz typing_loop

  ; Scan backwards to find the length of the previous string
  ;
  ; Setup args for _move_up:
  ;  - bp : pointer to print the new line from
  ;  - cx : number of characters to print
  mov bp, di
  dec di ; Second -=1 for the \n, need to maintain the di state
  xor cx, cx
  .line_length_loop:
  cmp byte [es:bp-1], `\n`
  je .found_length

  inc cx
  dec bp

  test bp, bp
  jz .found_length

  jmp .line_length_loop

  .found_length:
  cmp cx, ROW_LENGTH
  jle .done

  add bp, cx

  ; cx = cx % ROW_LENGTH
  mov ax, cx
  mov cl, ROW_LENGTH
  div cl
  mov cl, ah
  xor ch, ch

  sub bp, cx

  ; fallthrough
  .done:
  ; Set the cursor location on the prev line
  ;
  ; Note: cl is always >= 1 because we disallowed consecutive new lines, so
  ; we'll never set the cursor outside the typing area. This -1 will go away
  ; in insert mode, we'll want the cursor one past the last char not on it.
  mov dl, START_COL-1
  add dl, cl
  jmp _move_up

; Moves the cursor right one char (also wrapping lines) then continues typing_loop
move_right:
  cmp di, si ; note: when the user types a character this is never equal
  je typing_loop ; do nothing if we're already at the end

  inc di

  cmp byte [es:di], `\n`
  je .next_line_skip_newline

  cmp dl, END_COL
  je .next_line

  ; Normal case, just go right one
  ;
  ; When typing the cursor has already moved but we set the cursor anyway so
  ; we can re-use this function for arrow keys
  inc dl
  jmp set_cursor_and_continue

  .next_line_skip_newline:
  ; We know if we found a new line there is space for one more char
  ; i.e. di < si
  inc di
  .next_line:
  mov dl, START_COL
  mov bp, di ; the start of the string to print from on the new line
  xor cx, cx ; _move_down should scan for the \n to find the length
  ; We don't always have to scan the string (if we're not scrolling the screen)
  ; so not scanning the string here saves the work sometimes.
  jmp _move_down

; Move the cursor up one line and keep the same column.
;
;  - Prints existing data if scolling to a part of the buffer with data
;  - Does not change di pointer
;  - Does not check bounds
;  - Scrolls if necessary
;
; Args:
;   dl : final cursor column
;   bp : pointer to start printing from
;   cx : number of characters to print (0 if it should scan for '\n')
_move_up:
  cmp dh, START_ROW
  je .scroll_down

  dec dh
  jmp set_cursor_and_continue

  .scroll_down:
  mov al, 1
  jmp _scroll_and_continue


; Move the cursor down one line and keep the same column
;
;  - Prints existing data if scolling to a part of the buffer with data
;  - Does not update the di pointer
;  - Does not check bounds
;  - Scrolls if necessary
;
; Args:
;   dl : final cursor column
;   bp : pointer to start printing from
;   cx : number of characters to print (0 if it should scan for '\n')
_move_down:
  cmp dh, END_ROW ; if we're at the bottom
  je .scroll_up

  inc dh ; Next row
  jmp set_cursor_and_continue

  .scroll_up:
  mov al, 0
  jmp _scroll_and_continue

; Scrolls the text up or down one row printing any existing data in the text
; buffer when it does it
;
; Args:
;   al : 0 for down, non-zero for up
;   dx : final cursor position
;   bp : pointer to start printing from
;   cx : number of characters to print (0 if it should scan for '\n')
_scroll_and_continue:
  push dx ; cursor position
  push cx ; number of characters to print or 0 if we should scan
  push bp ; string to print
  mov bp, sp

  ; set ah = 6 for going down; 7 for going up
  mov ah, 6; BIOS scroll up, means make room at the bottom
  test al, al
  jz .down
  ; .up:
  inc ah ; ah = 0x7 BIOS scroll down, means make room at the top
  .down:

  ; Scroll the user code text area
  mov al, 1 ; scroll one line
  mov cx, MAIN_TOP_LEFT
  mov dx, MAIN_BOTTOM_RIGHT
  mov bh, MAIN_COLOR ; Also clear bh because it's the page number for write string below
  int 0x10

  ; restore the cursor position and the arguments
  mov dx, [bp+4] ; Need the cursor row for where to print the string
  mov cx, [bp+2]
  mov bp, [bp]

  ; If we're adding on to the end of the buffer skip all the checks below
  cmp bp, si
  je .nothing_to_print

  cmp cx, 0
  jne .already_know_length

  ; Scan the string to find the length to print
  ; Stop if we hit the end of the row or the end of the buffer
  .find_string_length:
  cmp byte [es:bp], `\n`
  je .found_length
  inc cx
  inc bp

  cmp bp, si
  je .found_length
  cmp cx, ROW_LENGTH
  je .found_length

  jmp .find_string_length

  .found_length:

  ; restore the string to print pointer
  mov bp, sp
  mov bp, [bp]

  .already_know_length:
  ; Print the line from the buffer
  ; bp is already the pointer to print from
  ; cx is number of chars to print
  mov dl, START_COL  ; keep the row but move to the start column
  mov bx, MAIN_COLOR ; bh = 0 (page number); bl = color
  mov ax, 0x1300   ; Write String, without moving the cursor
  int 0x10

  .nothing_to_print:
  add sp, 4 ; Clear the two args off the stack
  pop dx ; restore the cursor position
  jmp set_cursor_and_continue

NUM_EXTRA_SECTORS: equ ($-start_)/SECTOR_SIZE + 1

; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andrew Kallmeyer <ask@ask.systems>
%define SECTOR_SIZE 512

; Segment register value. Actual location is 0x00500
%define USER_CODE_LOC 0x0050
; Max value for di and si in typing_loop
; Allows ~30k of code.
%define USER_CODE_MAX (0x7C00-0x0500-1) ; Boot code address - code address segment reg offset - 1

; Values in ax after the keyboard read BIOS call
; See Figure 4-3 of the 1987 BIOS manual. (page 195)
%define LEFT_ARROW  0x4B00
%define RIGHT_ARROW 0x4D00
%define EOT 0x2004 ; Ctrl+D

%define MAIN_COLOR 0x17
%define BORDER_COLOR 0x97

; Note that the code requires that there's at least one character of border
%define MAIN_TOP_LEFT 0x0204 ; row = 2,  col = 2
%define MAIN_BOTTOM_RIGHT 0x164B ; row = 22, col = 79-2
%define START_ROW 0x02
%define START_COL 0x04
%define END_ROW 0x16
%define END_COL 0x4B
%define ROW_LENGTH (END_COL-START_COL+1) ; 0x48, 72
%define NUM_PRINTS_PER_ROW (ROW_LENGTH/5) ; 5 = 4 hex chars + 1 space

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

; Set cursor type to an underline
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

; Prints a hex word in the top margin for debugging purposes
; Supports printing as many as will fit in before it runs into the text area
;
; cx = two bytes to write at current cursor
; clobbers ax, and bx
debug_print_hex:
  push dx ; Save the cursor position

  ; One row above the top of the text area
  mov dl, START_COL
  xor dh, dh

  ; Calculate where to print based on number of words we have printed
  xor ax, ax
  mov byte al, [num_debug_prints]
  mov bl, NUM_PRINTS_PER_ROW
  div bl
  ; al = (num_debug_prints)/(num_per_row); ah = (num_debug_prints) % (num_per_row)

  add dh, al ; row += the row to print on

  cmp dh, START_ROW
  je .no_more_room

  inc byte [num_debug_prints] ; remember that we printed

  ; column += modulus * (hex+" " string length)
  mov al, ah
  xor ah, ah
  mov bl, 5
  mul bl
  add dl, al ; note: the mul result technically could be more than 2^8 (in ax) but we know it is less than ROW_LENGTH

  ; set cursor to the location to print
  mov ah, 0x02
  xor bh, bh
  int 0x10 ; dx is the cursor position

  ; Print cx
  call print_hex
  ; don't bother printing a space because the margin is blank

  ; fallthrough
.no_more_room:
  ; reset the cursor
  pop dx
  mov ah, 0x02
  xor bh, bh
  int 0x10 ; dx is the cursor position

  ret

; === Bootsector data area ===

error_msg: db `Error reading additional sectors from disk: `
error_msg_len: equ $-error_msg

num_debug_prints: db 0x00

BOOT_DISK: db 0x00 ; value is filled first thing

%include "util/bootsect-footer.asm"

; Optionally change the keyboard layout.
;
; To use a layout: look in the file you want for the %ifdef label and -D define
; it on the commandline. Ex: ./boot bootstrap-asm.asm -DDVORAK
;
; TODO: is the default layout qwerty or hardware specific? My map is qwerty->dvorak only
;
; It's easy to make a new layout!
;
; Simply pull up an ascii table (my favorite is `man ascii`) then type it out
; starting from ' ' to `~` using your qwerty keyboard labels using the desired
; layout in your OS (note: both \ and ` must be escaped for the assembler)
%include "keyboard-layouts/dvorak.asm"

start_:

; Set cursor position to the start
mov ax, 0x0200
mov dx, MAIN_TOP_LEFT
xor bh, bh ; page 0
int 0x10

; --- typing_loop global register variables ---
;
; dx      - cursor position (set above)
; [es:di] - the current position to write to in the user code buffer (di=0 is the beginning)
; [es:si] - the end of the user code buffer (will possibly be only half a byte)
;
; Additonally cx,bp are callee save while ax,bx are caller save (clobbered)
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

; ==== typing_loop helpers that use ret ====

%ifdef CONVERT_LAYOUT
; Takes an ascii char in al (typed in querty) and converts it to dvorak
; Assumes al >= ' ' && al <= '~'
convert_keyboard_layout:
  xor bh, bh
  mov bl, al
  sub bl, ' '
  mov al, [bx + keyboard_map]
  ret
%endif

; Scrolls the text up or down one row (leaving a blank line)
;
; Args:
;   al : 0 for down, non-zero for up
scroll_text:
  push dx ; cursor position
  push cx

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

  ; Scroll the left side scroll markers (even if there's nothing there)
  mov al, 1 ; scroll one line
  mov cx, MAIN_TOP_LEFT - 1 ; left one column from the upper left corner (columns are the low bits, so we can save an instruction)
  mov dh, END_ROW
  mov dl, START_COL-1
  mov bh, BORDER_COLOR ; Also clear bh because it's the page number for write string below
  int 0x10

  ; Scroll the right side scroll markers
  ; Note: I reset some registers to the value they should still have
  ;       just in case the BIOS clobbers them
  mov al, 1 ; scroll one line
  mov ch, START_ROW
  mov cl, END_COL+1
  mov dh, END_ROW
  mov dl, END_COL+1
  mov bh, BORDER_COLOR ; Also clear bh because it's the page number for write string below
  int 0x10

  pop cx
  pop dx
  ret

; Print a line from the buffer at the beginning of the current cursor row
;
; Args:
;   bp : pointer to print from
;   cx : number of characters to print
;   dx : cursor position (prints from START_COL in cursor row)
print_line:
  push dx
  ; Print the line from the buffer
  ; bp is already the pointer to print from
  ; cx is number of chars to print
  mov dl, START_COL  ; keep the row but move to the start column
  xor bh, bh ; page number 0
  mov bl, MAIN_COLOR
  mov ax, 0x1300     ; Write String, without moving the cursor
  int 0x10

  pop dx ; restore the cursor position
  ret

; Scan backwards from the current write pointer (di) to the first \n and count
; Assumes di > 0
;
; Returns:
;  - bp : pointer to the start of the current line
;  - cx : number of characters up until the the write pointer (di)
get_line:
  mov bp, di
  xor cx, cx
  .loop:
  cmp byte [es:bp-1], `\n`
  je .done
  inc cx
  dec bp
  test bp, bp
  jnz .loop
  .done:
  ret

; Sets or clears left or right markers for horizontal line scrolling
; Note: This should be called with call
;
; Args:
;   ah : 1 set the marker, 0 clear the marker
;   al : 0 for right side, 1 for left side
;   dx : cursor position (sets the marker on the cursor row)
set_line_scroll_marker:
  push dx
  push cx

  ; Set the column to print at and the character to print based on al
  test al, al
  jz .right_side
  ; .left_side:
  mov dl, START_COL-1
  mov al, '>'
  jmp .check_clear
  .right_side:
  mov dl, END_COL+1
  mov al, '<'
  .check_clear:

  ; Clear the marker depending on ah
  test ah, ah
  jnz .no_clear_marker
  mov al, ' ' ; clear the marker by printing a space
  .no_clear_marker:

  ; Move the cursor to the margin
  mov ah, 0x02
  xor bh, bh ; page number (for both calls)
  int 0x10

  ; Print the character
  ; Note: don't use a single Write String call because we'd have to change es
  mov ah, 0x09
  mov cx, 1 ; number of characters to print
  mov bl, BORDER_COLOR
  int 0x10

  pop cx
  pop dx
  ret

; ==== typing_loop internal helpers that continue the loop ====

; Takes an ascii char in al then saves and prints it and continues typing_loop
save_and_print_char:

; Convert the keyboard layout if one is %included
%ifdef CONVERT_LAYOUT
  call convert_keyboard_layout
%endif

  ; Print the ascii char (in al)
  mov ah, 0x0E ; Write teletype character
  xor bh, bh
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
  test di, di
  jz typing_loop

  mov byte [es:di], `\n`
  dec di ; so that move_right sees our \n after inc di
  inc si ; we're at the end of the buffer and need to make room

  jmp move_right

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

  cmp dl, START_COL
  je .scroll_line_left

  ; Normal case, just go back one char
  dec dl
  jmp set_cursor_and_continue

  .scroll_line_left:
  ; cursor is already where it needs to be

  ; Print the right-side marker if this is the first time we've cut off a char
  ; on the right.
  ;
  ; We're printing the chars at [di+0, di+ROW_LENGTH-1] and we know that
  ; [di+ROW_LENGTH] is the character we're cutting off now. Then if the next
  ; chacter is the new line we know that this is the first time we've cut-off
  ; a character on the right side in this line.
  ;
  ; The first check shortcuts when we're at the end of the buffer. Technically
  ; we only need this to not read past [es:USER_CODE_MAX], because we have
  ; garunteed that si+1 is safe to compare with \n for move_right to work.
  lea bp, [di+ROW_LENGTH]
  cmp bp, si
  je .need_right_marker
  cmp byte [es:di+ROW_LENGTH+1], `\n`
  jne .already_have_right_marker
  .need_right_marker:
  ; Print the marker on the right margin to show the line was cut off
  mov ax, 0x0100
  call set_line_scroll_marker
  .already_have_right_marker:

  ; Set args for print_line
  mov bp, di ; start printing where the cursor is
  ; Print ROW_LENGTH characters
  ;
  ; We know there are this many chars because we hit START_COL and there's no
  ; new line, meaning we scrolled which can only happen when the line is long
  mov cx, ROW_LENGTH

  ; Check if we're at the start of the line and clear the left marker if so
  test di, di
  jz .at_beginning_of_line
  cmp byte [es:di-1], `\n` ; -1 is safe because of the above check
  je .at_beginning_of_line
  jmp .keep_marker
  .at_beginning_of_line:
  ; Clear the marker on the left side
  mov ax, 0x0001
  call set_line_scroll_marker
  .keep_marker:
  call print_line
  jmp set_cursor_and_continue

.prev_line:
  ; Don't go past the start of the buffer if the first char is \n
  test di, di
  jz typing_loop

  call get_line
  dec di ; Skip past the \n that caused the .prev_line call

  ; Scroll the screen (and set the final cursor row position)
  cmp dh, START_ROW
  jne .no_screen_scroll
  ; We have to make room for the previous line
  mov al, 1
  call scroll_text
  jmp .paint_row
  .no_screen_scroll:
  ; Just move the cursor up
  dec dh
  ; If the row is empty, skip it because we're in replacement mode
  test cx, cx
  jz .prev_line
  cmp cx, ROW_LENGTH
  jle .skip_repaint
  ; fallthrough
  ; TODO: can we avoid redoing the above two checks after the fallthrough?

  .paint_row:
  ; If the row is empty, skip it because we're in replacement mode
  test cx, cx
  jz .prev_line
  ; Clip the string to [:min(strlen, ROW_LENGTH)] chars at the end
  ; (the line will be scrolled all the way to the right so we type at the end)
  cmp cx, ROW_LENGTH
  jle .no_clipping
  ; Move the print pointer to the end of the line minus ROW_LENGTH
  add bp, cx
  sub bp, ROW_LENGTH
  mov cx, ROW_LENGTH ; we're printing ROW_LENGTH characters
  ; Set the scroll markers since we have some cut off to the left
  mov ax, 0x0101 ; set to on ; left margin
  call set_line_scroll_marker
  mov ax, 0x0000 ; set to off ; right margin
  call set_line_scroll_marker
  ; fallthrough
  .no_clipping:
  ; Repaint the previous line
  call print_line

  .skip_repaint:

  ; Set the cursor column and finally move it
  ;
  ; Note: cl is always >= 1 because we skipped consecutive new lines, so
  ; we'll never set the cursor outside the typing area. This -1 will go away
  ; in insert mode, we'll want the cursor one past the last char not on it.
  mov dl, START_COL-1
  add dl, cl
  jmp set_cursor_and_continue

; Moves the cursor right one char (also wrapping lines) then continues typing_loop
move_right:
  cmp di, si ; note: when the user types a character this is never equal
  je typing_loop ; do nothing if we're already at the end

  inc di

  cmp byte [es:di], `\n`
  je .next_line

  cmp dl, END_COL
  je .scroll_line_right

  ; Normal case, just go right one
  ;
  ; When typing the cursor has already moved but we set the cursor anyway so
  ; we can re-use this function for arrow keys
  inc dl
  jmp set_cursor_and_continue

  .scroll_line_right:
  lea bp, [di-(ROW_LENGTH-1)] ; print starting from the second char in the row
  mov cx, ROW_LENGTH ; number of chars to print

  ; Note: we know that bp >= 1 here because we scrolled right so we must have
  ; at least ROW_LENGTH characters in the buffer

  ; Print the left-side marker if this is the first time we've cut off a char
  cmp bp, 1 ; if the first char in the line is the first char in the buffer
  je .need_left_marker
  ; Now we know that bp >= 2 because bp != 1
  cmp byte [es:bp-2], `\n` ; if the char before the first char in the line is \n
  jne .already_have_left_marker
  .need_left_marker:
  mov ax, 0x0101 ; set to on ; left margin
  call set_line_scroll_marker
  .already_have_left_marker:

  ; Check if we're at the end of the line and clear the right marker if so
  cmp di, si
  je .at_end_of_buffer
  cmp byte [es:di+1], `\n`
  je .at_end_of_line
  jmp .keep_marker

  ; We need to leave a space for the next character to type
  .at_end_of_buffer:
  ; don't leave a space for typing if we're at the end of the buffer
  ; also protects from overwriting user code with a space
  cmp si, USER_CODE_MAX
  je .at_end_of_line
  ; Put a space in the buffer so we print it and overwrite on the screen the
  ; last char the user typed. This avoids doing a two extra BIOS calls to
  ; clear the character.
  ;
  ; This is 1 past the last char the user typed, and we know that we haven't
  ; run out of buffer space. Also note that di == si here.
  mov byte [es:di], ' '

  ; fallthrough
  .at_end_of_line:
  ; Clear the marker on the right side
  mov ax, 0x0000
  call set_line_scroll_marker
  .keep_marker:
  call print_line
  jmp set_cursor_and_continue

  .next_line:

  ; Reset the current line to show the left side if it's longer than the screen
  call get_line
  cmp cx, ROW_LENGTH
  jl .skip_resetting_line
  ; Note: equal is a special case for save_new_line in replace mode to remove
  ; the extra space when the line is the one the user is typing in. Otherwise
  ; we could just skip_resetting_line when equal.
  je .skip_right_marker
  mov ax, 0x0100 ; set to on ; right margin
  call set_line_scroll_marker
  .skip_right_marker:
  mov ax, 0x0001 ; set to off ; left margin
  call set_line_scroll_marker
  mov cx, ROW_LENGTH
  call print_line
  .skip_resetting_line:

  ;jmp _move_down fallthrough

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
.move_down:
  ; Skip the \n char
  ; We know if we found a new line there is space for one more char
  ; i.e. di < si
  inc di

  cmp dh, END_ROW ; if we're at the bottom
  je .scroll_up

  inc dh ; Next row

  ; Skip over consecutive empty lines because replacement mode
  cmp byte [es:di], `\n`
  je .move_down

  jmp .done

.scroll_up:
  mov al, 0
  call scroll_text

  ; Scan the buffer string to find the length to print on the newly blank line
  ; Stop if we hit the end of the row or line or the end of the buffer
  xor cx, cx
  mov bp, di
  .find_string_length:
  cmp bp, si
  je .found_length
  cmp byte [es:bp], `\n`
  je .found_length
  cmp cx, ROW_LENGTH+1 ; we only need to know it's longer than the row
  je .found_length
  inc cx
  inc bp
  jmp .find_string_length

  .found_length:
  mov bp, di ; restore the start of the string
  cmp cx, ROW_LENGTH
  jle .shorter_than_row

  ; If we had more than a row just print the row
  mov cx, ROW_LENGTH
  ; Set the marker on the right to show we have a cut off string
  mov ax, 0x0100
  call set_line_scroll_marker

  .shorter_than_row:
  ; Skip over consecutive new lines because replacement mode
  cmp byte [es:bp], `\n`
  je .move_down
  .print:
  call print_line

.done:
  mov dl, START_COL
  jmp set_cursor_and_continue

NUM_EXTRA_SECTORS: equ ($-start_-1)/SECTOR_SIZE + 1

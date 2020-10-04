; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andrew Kallmeyer <ask@ask.systems>
%define SECTOR_SIZE 0x200 ; 512 in decimal

; Segment register value (so the actual start is at the address 0x10*this)
; This is the first sector after the editor's code
%define USER_CODE_LOC (CODE_SEGMENT+(SECTOR_SIZE/0x10)*(NUM_EXTRA_SECTORS+1))
; Maxiumum size for the code buffer
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

; Color byte is: bg-intensity,bg-r,bg-g,bg-b ; fg-intensity,fg-r,fg-g,fg-b
%define MAIN_COLOR 0x17
%define BORDER_COLOR 0x97
%define ERROR_COLOR 0x47

; Note that the code requires that there's at least one character of border
%define MAIN_TOP_LEFT 0x0204 ; row = 2,  col = 2
%define MAIN_BOTTOM_RIGHT 0x164B ; row = 22, col = 79-2
%define START_ROW 0x02
%define START_COL 0x04
%define END_ROW 0x16
%define END_COL 0x4B
%define ROW_LENGTH (END_COL-START_COL+1) ; 0x48, 72
%define NUM_PRINTS_PER_ROW (ROW_LENGTH/5) ; 5 = 4 hex chars + 1 space

; Size of the gap buffer when we reset it
; Must be less than USER_CODE_MAX
;
; The user code is stored using a gap buffer to allow inserting at the cursor
; location without having to shift the trailing data in memory. The system
; maintains a single gap in the code buffer at the cursor position which shrinks
; as the user types continuously and must periodically be reset by shifting the
; data. When the user deletes the gap grows. When the cursor hits the end of the
; buffer the gap can be reset for free.
%define GAP_SIZE (8*ROW_LENGTH)

%include "util/bootsect-header.asm"

mov [BOOT_DISK], dl ; Save the boot disk number

; Set video mode, 16 color 80x25 chars
;
; The IBM BIOS manual describes a long procedure for determining which video
; modes are supported and all the possible options for supporting both mono and
; color. Sometimes mono isn't supported if the PC supports color modes. So, I'm
; just going to assume all hardware this is used on supports color modes.
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

; Color byte is now: bg-intensity,bg-r,bg-g,bg-b ; fg-intensity,fg-r,fg-g,fg-b

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
  ja .over_9

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

  ; Reset the count so we overwrite at the beginning
  cmp dh, START_ROW
  jne .no_reset
  mov byte [num_debug_prints], 0
  xor ax, ax
  xor dh, dh
  .no_reset:

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
; This is the start for calculating NUM_EXTRTA_SECTORS
; There must be nothing other than the bootsector above this label
extra_sectors_start:

; Optionally change the keyboard layout.
;
; To use a layout: look in the file you want for the %ifdef label and -D define
; it on the commandline. Ex: ./boot bootstrap-asm.asm -DDVORAK
;
; It's easy to make a new layout!
;
; Simply pull up an ascii table (my favorite is `man ascii`) then type it out
; starting from ' ' to '~' using your qwerty keyboard labels using the desired
; layout in your OS (note: both \ and ` must be escaped for the assembler)
%include "keyboard-layouts/dvorak.asm"
; TODO: is the default layout qwerty or hardware specific? My map is qwerty->dvorak only

no_more_room_msg: db `No more room in the code buffer`
no_more_room_msg_len: equ $-no_more_room_msg

start_:

; Set cursor position to the start
mov ax, 0x0200
mov dx, MAIN_TOP_LEFT
xor bh, bh ; page 0
int 0x10

; --- typing_loop global register variables ---
;
; The user code is an (almost) contiguous null-terminated buffer starting at
; es:0 with a single gap of garbage data in it (which lets us do inserts without
; shifting the tail of the data every time to make space).
;
; dx      - cursor position (set above)
; [es:di] - the first garbage character in the gap (current position to write to)
; [es:si] - the first code character after the gap
;
; Additonally cx,bp are callee save while ax,bx are caller save (clobbered)
mov ax, USER_CODE_LOC
mov es, ax
xor di, di
mov si, GAP_SIZE
mov byte [es:si], 0 ; null-terminate the string

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
  jb .non_printable
  cmp al, '~'
  ja .non_printable
  jmp save_and_print_char

  .non_printable:

  cmp al, `\b` ; Backspace, shift backspace, Ctrl+H
  je backspace

  cmp al, `\r` ; Enter/Return key
  je save_new_line

  cmp ax, LEFT_ARROW
  je move_left

  cmp ax, RIGHT_ARROW
  je move_right

  jmp typing_loop ; Doesn't match anything above

; ==== typing_loop internal helpers that continue the loop ====

; Moves the cursor to the value in dx and continues typing_loop
set_cursor_and_continue:
  mov ah, 0x02
  xor bh, bh
  int 0x10 ; dx is the cursor position
  jmp typing_loop

; Takes an ascii char in al then saves and prints it and continues typing_loop
save_and_print_char:
  cmp di, si ; disallow typing anywhere when we're out of buffer space
  je typing_loop
  cmp di, USER_CODE_MAX ; Leave a \0 at the end
  je typing_loop

; Convert the keyboard layout if one is %included
%ifdef CONVERT_LAYOUT
  call convert_keyboard_layout
%endif

  ; Print the ascii char (in al)
  mov ah, 0x0E ; Write teletype character
  xor bh, bh
  int 0x10

  mov byte [es:di], al ; Write the typed char to the buffer
  inc di

  call maybe_reset_gap

  cmp dl, END_COL
  je _scroll_line_right ; same if we're inserting or at the end of the line
  ; We're inserting in the middle of the row, repaint the tail of the line

  inc dl

  mov bp, si
  call scan_forward

  ; bx = remaining space in the row from the current cursor position
  mov bx, (ROW_LENGTH+START_COL)
  sub bl, dl

  cmp cx, bx
  jbe .not_cut_off

  ; clip the amount to repaint to the current row
  mov cx, bx
  ; Set the marker on the right to show we have a cut off string
  mov ax, 0x0100
  call set_line_scroll_marker
  ; fallthrough
  .not_cut_off:
  call print_line

  ; The cursor did move from the typing interrupt but lets just set it anyway,
  ; if we didn't the cursor breaks when putting in a debug print in some places
  jmp set_cursor_and_continue

save_new_line:
  cmp di, si ; disallow typing anywhere when we're out of buffer space
  je typing_loop
  cmp di, USER_CODE_MAX ; Leave a \0 at the end
  je typing_loop

  ; Write the `\n` into the buffer
  mov byte [es:di], `\n`
  inc di

  call maybe_reset_gap

.reset_current_line:
  cmp di, 1
  je .clear_tail ; first character is a \n, so there's no string to scan
  lea bp, [di-2]
  call scan_backward

  ; Set the scroll markers
  mov ax, 0x0001 ; set to off ; left margin
  call set_line_scroll_marker
  ; Set right to on or off always
  mov ax, 0x0000 ; set to off ; right margin
  cmp cx, ROW_LENGTH
  jbe .right_off
  mov cx, ROW_LENGTH ; also clip the length for printing (reuse the cmp)
  mov ax, 0x0100 ; set to on ; right margin
  .right_off:
  call set_line_scroll_marker

  ; Repaint the line
  mov dl, START_COL
  call print_line
  add dl, cl ; move the cursor to the end of the string (for .clear_tail)

  cmp cx, ROW_LENGTH
  je .move_down ; skip clearing the tail if we just repainted everything
  ; fallthrough

  .clear_tail:
  cmp byte [es:si], `\n`
  je .move_down ; no need to clear if we were at the end of the line already
  cmp byte [es:si], 0
  je .move_down ; nothing to clear at the end of the buffer

  ; Write spaces to clear the rest of the line since it's on the next line now
  ; Note: this call uses the actual set cursor position not dx
  mov ah, 0x09
  mov al, ' '
  xor bh, bh
  mov bl, MAIN_COLOR
  ; cx = 1 + END_COL - curr_cursor_col = num spaces to write
  mov cx, END_COL+1
  sub cl, dl
  int 0x10
  ; fallthrough

  ; Do a partial screen scroll to make room for the new line in the middle
.move_down:
  mov dl, START_COL ; start at the beginning on the next line
  ; If there's no text after our tail, we don't want to scroll anything
  mov bp, si
  call scan_forward ; Note we need to save the return (cx) until .print_new_line

  cmp dh, END_ROW
  jne .make_space_in_middle
  ; Move the text one line up to make an empty line at the bottom
  mov al, 0
  call scroll_text
  jmp .print_new_line

  inc dh
  jmp .print_new_line

  .make_space_in_middle:
  inc dh
  mov bx, cx ; move the line length to bx so we can put the + in the cmp
  cmp byte [es:bx+si], 0 ; if this is the last line, we already have space
  je .no_scroll
  mov al, 1
  call scroll_text
  .no_scroll:
  ; fallthrough

  ; Print the tail of the line after the new \n if there is any
  ; (uses bp,cx from scan_forward above)
  .print_new_line:
  ; re-use the saved result of scan_forward from the check above
  cmp cx, ROW_LENGTH
  jbe .not_cut_off
  mov cx, ROW_LENGTH ; clamp to the row length
  mov ax, 0x0100 ; set to on ; right margin
  call set_line_scroll_marker
  ; fallthrough
  .not_cut_off:
  call print_line

  jmp set_cursor_and_continue

backspace:
  test di, di ; Can't delete past the beginning of the buffer
  jz typing_loop

  cmp byte [es:di-1], `\n` ; check the char we just "deleted" (di-1 is usually the char before the cursor)
  je .join_lines

  ; Don't let the user delete a character they can't see (unless it's \n).
  ; This also avoids requiring us to scroll_line_left inside backspace
  cmp dl, START_COL
  je typing_loop

  ; fallthrough
.delete_mid_line:
  dec di ; delete the char by pushing it into the gap
  dec dl ; Update the cursor position

  ; .repaint_tail:
  mov bp, si
  call scan_forward

  ; bx = maximum chars until the end of the row
  mov bx, (START_COL+ROW_LENGTH)
  sub bl, dl
  cmp cx, bx
  jae .clip_line ; in the equals case we don't want to print an extra space

  ; We're re-painting a line tail that's not cut off currently. So:
  ; Replace the line-ending char with a space for covering up the old last char
  mov bx, si
  add bx, cx ; bx = the line ending char (\n or \0)
  push word [es:bx-1] ; save the line ending char in the high byte (you can only push whole words)
                      ; Note: [es:bx+1] may not be safe to read while -1 is here.
  mov byte [es:bx], ' '
  push bx ; save the pointer to the line ending

  ; Repaint the tail of the line
  inc cx ; print the space too
  call print_line

  ; Restore the line ending
  pop bx
  pop ax
  mov byte [es:bx], ah

  jmp set_cursor_and_continue

  .clip_line:
  ; Set the right marker on, unless our line exactly ends at the end of the row
  mov ax, 0x0000 ; set to off ; right margin
  cmp cx, bx
  je .no_marker
  mov ax, 0x0100 ; set to on ; right margin
  .no_marker:
  call set_line_scroll_marker

  ; Print only up to our maximum (recalculated from above since bx gets clobbered)
  mov cx, END_COL+1
  sub cl, dl
  call print_line

  jmp set_cursor_and_continue

.join_lines:
  ; TODO
  ; dec di ; delete the char by pushing it into the gap

  ; Clear the current line
  ; Scroll the text below up

  ; jmp _prev_line, but we can't skip_repaint when we are shorter than the row
  ; Could detect if we were called from here or move_left with cmp [es:si], `\n`
  ; Also we want to leave the cursor in the middle where it was...

  jmp typing_loop

; Moves the cursor left one nibble then continues typing_loop
;  - Saves the byte in cl when moving to a new byte, also loads the existing
;  data into cl if applicable
;  - Calls move_up when moving to the next line
move_left:
  ; Don't do anything if we're already at the beginning of the buffer
  test di, di
  jz typing_loop

  dec di
  dec si
  mov byte al, [es:di]
  mov byte [es:si], al

  cmp byte [es:si], `\n`
  je _prev_line

  cmp dl, START_COL
  je _scroll_line_left

  ; Normal case, just go back one char
  dec dl
  jmp set_cursor_and_continue

_scroll_line_left:
  ; Just need to repaint the tail of the line and set scroll markers
  mov bp, si
  call scan_forward

  cmp cx, ROW_LENGTH
  jbe .no_clipping
  ; Set the right marker only when this is the first char we've cut off
  cmp cx, ROW_LENGTH+1
  jne .no_set_right_marker
  mov ax, 0x0100 ; set to on ; right margin
  call set_line_scroll_marker
  .no_set_right_marker:

  mov cx, ROW_LENGTH ; Print only up to our maximum
  ; fallthrough
  .no_clipping:
  call print_line

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
  jmp set_cursor_and_continue

_prev_line:
  xor cx, cx ; line length is 0 if we're at di==0
  test di, di ; make room if the first char is \n
  jz .move_up

  lea bp, [di-1] ; scan from the char before the \n
  call scan_backward

  ; Scroll the screen (and set the final cursor row position)
.move_up:
  cmp dh, START_ROW
  jne .no_screen_scroll
  ; We have to make room for the previous line
  mov al, 1
  call scroll_text
  jmp .paint_row
  .no_screen_scroll:
  ; Just move the cursor up
  dec dh
  cmp cx, ROW_LENGTH-1
  jbe .skip_repaint
  ; fallthrough

.paint_row:
  ; Clip the string to [:min(strlen, ROW_LENGTH-1)] chars at the end
  ; (-1 to leave a space for the user to type at the end)
  mov dl, START_COL ; print from the beginning of the line
  cmp cx, 0
  je .skip_repaint
  cmp cx, ROW_LENGTH-1
  jbe .no_clipping
  ; Move the print pointer to the end of the line minus ROW_LENGTH-1
  add bp, cx
  mov cx, ROW_LENGTH ; num chars to print
  sub bp, ROW_LENGTH-1 ; leave a space at the end

  ; Put a space in the buffer so we print it and overwrite on the screen the
  ; last char the user typed.
  mov byte [es:di], ' '

  ; Set the scroll markers since we have some cut off to the left
  mov ax, 0x0101 ; set to on ; left margin
  call set_line_scroll_marker
  mov ax, 0x0000 ; set to off ; right margin
  call set_line_scroll_marker

  call print_line
  dec cx ; so we set the cursor after the last char not after the space
  jmp .skip_repaint

  .no_clipping:
  call print_line
  ; fallthrough

.skip_repaint:
  ; Set the cursor column and finally move it
  mov dl, START_COL
  add dl, cl
  jmp set_cursor_and_continue

; Moves the cursor right one char (also wrapping lines) then continues typing_loop
move_right:
  ; Only happens when we're completely out of buffer space and can't advance si
  cmp di, si
  je typing_loop

  ; Stop the cursor at the end of the user's code
  cmp byte [es:si], 0
  je typing_loop

  mov byte al, [es:si]
  mov byte [es:di], al
  inc di

  ; Pretty sure this will never happen because [es:USER_CODE_MAX] is always \0
  ; when we run out of buffer space. But why depend on having the above check?
  cmp si, USER_CODE_MAX
  je .buffer_running_out
  inc si
  .buffer_running_out:

  ; If we just hit the end for the first time, do a free gap reset
  cmp byte [es:si], 0
  jne .no_reset_gap ; We don't want to copy every time we move the cursor
  call maybe_reset_gap ; This will do the check again and take the no-copy path
  .no_reset_gap:

  ; Move the cursor to the next line once we're past the \n so we can append
  ; (remember, di is garbarge space and di-1 is the character before the cursor)
  cmp byte [es:di-1], `\n`
  je _next_line

  cmp dl, END_COL
  je _scroll_line_right
  ; Normal case, just go right one
  inc dl
  jmp set_cursor_and_continue

_scroll_line_right:
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
  cmp byte [es:si], 0
  je .at_end_of_line
  cmp byte [es:si], `\n` ; check the next char
  je .at_end_of_line
  jmp .not_at_end

  ; We need to leave a space for the next character to type
  .at_end_of_line:
  ; Put a space in the buffer so we print it and overwrite on the screen the
  ; last char the user typed.
  mov byte [es:di], ' '
  jmp .paint_line

  .not_at_end:

  ; If the current char ([es:si]) is the last char in the line, clear the marker
  ; +1 is safe because we know the current char is not an end-of-line char
  cmp byte [es:si+1], 0
  je .clear_right_marker
  cmp byte [es:si+1], `\n` ; check the next char
  je .clear_right_marker
  jmp .keep_right_marker
  .clear_right_marker:
  mov ax, 0x0000 ; set to off ; right side
  call set_line_scroll_marker
  .keep_right_marker:

  ; Copy the next char into the temp space so we have a contiguous buffer
  mov byte al, [es:si]
  mov byte [es:di], al
  ; fallthrough
  .paint_line:
  ; Print starting from the beginning of the row and restore the cursor
  push dx
  mov dl, START_COL
  call print_line
  pop dx
  jmp set_cursor_and_continue

_next_line:

  ; Reset the current line to show the left side if it's longer than the screen.
  cmp di, 1
  je .skip_resetting_line ; The first char was the \n, no string to scan
  lea bp, [di-2] ; scan starting from the char before the \n
  call scan_backward
  mov dl, START_COL ; On the next line we're going to be at the start

  cmp cx, ROW_LENGTH
  jb .skip_resetting_line
  ; Note: equal is a special case to remove the extra space at the end of the
  ; line for the user to type in.
  je .skip_right_marker
  mov ax, 0x0100 ; set to on ; right margin
  call set_line_scroll_marker
  .skip_right_marker:
  mov ax, 0x0001 ; set to off ; left margin
  call set_line_scroll_marker

  mov cx, ROW_LENGTH
  call print_line
  .skip_resetting_line:

  ;fallthrough

; Move the cursor down past any number of new lines
;  - Prints existing data if scolling to a part of the buffer with data
;  - Scrolls the screen if necessary
;  - Assumes it is being called after seeing a \n
.move_down:
  cmp dh, END_ROW ; if we're at the bottom
  je .scroll_up
  inc dh ; Next row
  jmp .done

.scroll_up:
  mov al, 0
  call scroll_text

  mov bp, si
  call scan_forward

  cmp cx, ROW_LENGTH
  jbe .shorter_than_row
  mov ax, 0x0100 ; set to on ; right side
  call set_line_scroll_marker
  mov cx, ROW_LENGTH ; clip to the row
  ; fallthrough
  .shorter_than_row:

  call print_line ; cursor position is already correct
  ; fallthrough
.done:
  jmp set_cursor_and_continue

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
; Also when scrolling up it only scrolls starting at the current line to allow
; for clearing space when inserting a new line in the middle (this could be done
; for up but isn't needed)
;
; Args:
;   al : 0 for down, non-zero for up
scroll_text:
  push dx ; cursor position
  push cx

  ; set ah = 6 for going down; 7 for going up
  mov ah, 6; BIOS scroll up, means make room at the bottom
  mov ch, START_ROW ; all scrolling uses this row for .down
  test al, al
  jz .down
  ; .up:
  inc ah ; set the BIOS command for up
  mov ch, dh ; When scrolling up, start at the cursor row
  .down:

  ; Scroll the user code text area
  mov al, 1 ; scroll one line
  mov cl, START_COL
  mov dx, MAIN_BOTTOM_RIGHT
  mov bh, MAIN_COLOR ; Also clear bh because it's the page number for write string below
  int 0x10

  ; Scroll the left side scroll markers (even if there's nothing there)
  mov al, 1 ; scroll one line
  mov cl, START_COL - 1 ; left one column from the upper left corner (columns are the low bits, so we can save an instruction)
  mov dh, END_ROW
  mov dl, START_COL-1
  mov bh, BORDER_COLOR ; Also clear bh because it's the page number for write string below
  int 0x10

  ; Scroll the right side scroll markers
  ; Note: I reset some registers to the value they should still have
  ;       just in case the BIOS clobbers them
  mov al, 1 ; scroll one line
  mov cl, END_COL+1
  mov dh, END_ROW
  mov dl, END_COL+1
  mov bh, BORDER_COLOR ; Also clear bh because it's the page number for write string below
  int 0x10

  pop cx
  pop dx
  ret

; Print a line from the buffer at the beginning of the current cursor row
; Move the VGA cursor to the end of the print
;
; Args:
;   bp : pointer to print from
;   cx : number of characters to print
;   dx : cursor position to print from
print_line:
  ; Print the line from the buffer
  ; bp is already the pointer to print from
  ; cx is number of chars to print
  ; dx is the cursor position to print at
  xor bh, bh ; page number 0
  mov bl, MAIN_COLOR
  mov ax, 0x1301     ; Write String, with moving the cursor
  int 0x10
  ret

; Scan backwards to the first \n and count the length
;
; Args:
;  - bp : pointer to scan starting from
; Returns:
;  - bp : pointer to the start of the current line
;  - cx : number of characters up until the the write pointer (di)
scan_backward:
  xor cx, cx
  ; Check if we started on a \n
  cmp byte [es:bp], `\n`
  je .done
  inc cx

  .loop:
  test bp, bp
  jz .done
  cmp byte [es:bp-1], `\n`
  je .done
  dec bp
  inc cx
  jmp .loop

  .done:
  ret

; Scan forward counting string length for printing the row
; Stop at '\n' or the end of the buffer
;
; Args:
;  - bp : start pointer to scan from (restored at the end)
; Returns:
;  - cx : min(strlen(bp), ROW_LENGTH+1)
scan_forward:
  push bp
  xor cx, cx
  .find_string_length:
  cmp byte [es:bp], 0
  je .found_length
  cmp byte [es:bp], `\n`
  je .found_length
  inc cx
  inc bp
  jmp .find_string_length

  .found_length:
  pop bp ; restore the start of the string
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

; Reset the buffer gap only if needed.
;
; In the middle of the buffer it does a copy from es:si to the end, but at the
; end we can do it without a copy.
;
; Clobbers bp,cx (this is called at the beginning of typing_loop helpers only)
maybe_reset_gap:
  ; If we're at the end of the buffer we can reset the gap without a copy
  cmp byte [es:si], 0
  jne .not_at_end


  ; Reset the end of the gap to the gap_start+GAP_SIZE clipped to USER_CODE_MAX
  lea si, [di+GAP_SIZE] ; reset to the default gap size (might overflow)
  cmp di, USER_CODE_MAX-GAP_SIZE ; equivalent to cmp si, USER_CODE_MAX but still works if we overflowed
  jbe .still_have_room
  mov si, USER_CODE_MAX
  .still_have_room:

  mov byte [es:si], 0 ; keep the string null-terminated
  jmp .done

.not_at_end:
  ; If we run out of gap, we have to do a copy to reset it
  cmp di, si
  jne .done

  ; Scan forward to find the \0 (we know we're not already on it)
  mov bp, si ; Save the beginning
  .scan_forward:
  inc si
  cmp byte [es:si], 0
  jne .scan_forward

  ; Copy from si to si+GAP_SIZE in a loop in reverse order if there's space
  cmp si, USER_CODE_MAX-GAP_SIZE
  ja .no_more_room ; if equal we still copy because si is on the \0 and we can't loose it

  ; We're copying 2 bytes at a time so we need to fix the alignment
  ; (we know there's >= 2 chars because if it was only \0 or we'd be in the other branch)
  dec si ; start on the char before the last so we can copy 2
  mov bx, si
  sub bx, bp ; bx == num chars - 2 (since the range is [bp, si+1] inclusive)
  test bx, 1
  jz .shift_buffer ; if the number of chars is even, skip the extra byte
  ; Copy the first byte when there's an odd number of characters
  mov byte al, [es:si+1] ; +1 since we started on the second-to-last char
  mov byte [es:si+GAP_SIZE+1], al
  dec si ; now on the third-to-last char (3 is the first odd length)
  .shift_buffer:
  mov word ax, [es:si]
  mov word [es:si+GAP_SIZE], ax
  cmp si, bp
  je .end_shift
  sub si, 2
  jmp .shift_buffer
  .end_shift:

  add si, GAP_SIZE ; set the final position (it was left on the original location)
  jmp .done

.no_more_room:
  ; Print a warning message because we can't allow more typing now
  ; TODO: remove this message on backspace
  push dx
  ; The interrupt reads from es:bp and we need to read the error from the code
  mov ax, cs
  mov es, ax

  mov bp, no_more_room_msg
  mov cx, no_more_room_msg_len
  mov dx, MAIN_TOP_LEFT-0x0100 ; one line above the top left corner
  xor bh, bh ; page number 0
  mov bl, ERROR_COLOR
  mov ax, 0x1301 ; write string without moving the cursor
  int 0x10

  ; Reset es and the cursor position
  mov ax, USER_CODE_LOC
  mov es, ax
  pop dx
  ; fallthrough
  .done:
  ret

NUM_EXTRA_SECTORS: equ ($-extra_sectors_start-1)/SECTOR_SIZE + 1

; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andy Kallmeyer <ask@ask.systems>

; This is a text editor (uses a gap buffer) which calls the run_code function
; when you press ctrl+D. You use it by %define RUN_CODE before you include the
; text editor, and then define your run_code function after that.
;
; You may also define a debug_text label pointing to a null terminated string
; which will be pre-loaded into the editor on startup if DEBUG_TEXT is defined.
;
; Search for run_code: below to see the API for it.


; Color byte is: bg-intensity,bg-r,bg-g,bg-b ; fg-intensity,fg-r,fg-g,fg-b
%define MAIN_COLOR 0x17
%define BORDER_COLOR 0x97
%define ERROR_COLOR 0x47

; Note that the code requires that there's at least one character of border
%define MAIN_TOP_LEFT 0x0204 ; row = 2,  col = 4
%define MAIN_BOTTOM_RIGHT 0x164B ; row = 22, col = 79-4
%define START_ROW 0x02
%define START_COL 0x04
%define END_ROW 0x16
%define END_COL 0x4B

; Segment register value (so the actual start is at the address 0x10*this)
; This is the first sector after the editor's code
;
; This offset won't be exactly a multiple of NEXT_SEGMENT but it is at most
; NEXT_SEGMENT because we don't have a dynamic code segment switching setup so
; NUM_EXTRA_SECTORS is limited to 0x7F. So since there are 6
; non-overlapping complete segments available and we're only using 4 here
; (including the code segment), it's impossible for COMPILER_OUT_LOC to be
; greater than LAST_SEGMENT.
%define USER_CODE_LOC (CODE_SEGMENT+(SECTOR_SIZE/0x10)*(NUM_EXTRA_SECTORS+1))
%define COMPILER_DATA_LOC USER_CODE_LOC+NEXT_SEGMENT
%define COMPILER_OUT_LOC COMPILER_DATA_LOC+NEXT_SEGMENT

; Maxiumum size for the code buffer
;
; There's a lot of extra memory still after this but I don't want to move around
; the segment register so this is the limit.
;
; Allows 64k of code
%define USER_CODE_MAX 0xFFFF

; Values in ax after the keyboard read BIOS call
; See Figure 4-3 of the 1988 BIOS manual. (page 195)
%define LEFT_ARROW  0x4B00
%define RIGHT_ARROW 0x4D00
%define UP_ARROW 0x4800
%define DOWN_ARROW 0x5000
%define EOT 0x2004 ; Ctrl+D

%define ROW_COUNT (END_ROW-START_ROW+1) ; 0x15, 21
%define ROW_LENGTH (END_COL-START_COL+1) ; 0x48, 72
%define NUM_PRINTS_PER_ROW (ROW_LENGTH/5) ; 5 = 4 hex chars + 1 space

; Size of the gap buffer when we reset it
; Must be less than USER_CODE_MAX
;
; The user code is stored using a gap buffer to allow inserting at the cursor
; location without having to shift the trailing data in memory. The system
; maintains a single gap in the code buffer at the cursor position which shrinks
; as the usertypes continuously and must periodically be reset by shifting the
; data. When the user deletes the gap grows. When the cursor hits the end of the
; buffer the gap can be reset for free.
%define GAP_SIZE (8*ROW_LENGTH)

; start_ should be the first bytes in the file because that's where I have
; configured gdb to breakpoint
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

; Set cursor position to the start
mov ax, 0x0200
mov dx, MAIN_TOP_LEFT
xor bh, bh ; page 0
int 0x10

; Memory map:
;
; Each segment gets a full non-overlaping 64k block of memory and we don't move
; the segment so 64k is the limit for each part.
;
; SS: 0x050
;   - Leaves ~30k for the stack between 0x0500 and 0x7C00, the start of the code.
; CS: CODE_SEGMENT (0x7C0)
;   - The assembly code
; ES: USER_CODE_LOC or COMPILER_OUT_LOC if we're printing the output
;   - The code that the user types into the editor
;   - The printing routines all use es so we use es for printing
; DS: COMPILER_DATA_LOC
;   - Extra memory passed to the plugin program that runs the code you typed.
;     Only set when ctrl+D is pressed and passed to the plugin.
;   - Normally the default for memory address reads and our bootloader leaves
;     this set to CODE_SEGMENT for that. But this code alwasy uses cs: to
;     reference strings stored in the binary.

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
; Note: We must have at least one char of gap i.e. si-di >= 1 at all times
;       (because we use it as scratch space when printing sometimes)
;
; Additonally cx,bp are callee save while ax,bx are caller save (clobbered)
mov ax, USER_CODE_LOC
mov es, ax
mov ax, COMPILER_DATA_LOC
mov ds, ax
xor di, di
mov si, GAP_SIZE
mov byte [es:si], 0 ; null-terminate the string

%ifndef DEBUG_TEXT

jmp typing_loop

%else

; Move the debug_text into the segment used for the text editor code
mov di, debug_text
copy_debug_text:
mov al, [cs:di]
mov [es:si], al
cmp byte [cs:di], 0
je done_copy_debug
inc di
inc si
jmp copy_debug_text
done_copy_debug:

; Print it nicely in the editor
mov si, GAP_SIZE
mov di, GAP_SIZE
mov dh, START_ROW
mov cx, ROW_COUNT
call print_text

; Scroll the printed text to the top corner where the cursor starts
cmp cx, 0
je .skip_scolling
mov bx, cx
mov al, 0
mov dx, MAIN_TOP_LEFT
call scroll_text
.skip_scolling:

; Reset and run the typing_loop
xor di, di
mov si, GAP_SIZE
mov dx, MAIN_TOP_LEFT
jmp set_cursor_and_continue

%endif

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

num_debug_prints: db 0x00

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

user_code_start: dw 0

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

  cmp ax, UP_ARROW
  je move_up

  cmp ax, DOWN_ARROW
  je move_down

  cmp ax, EOT ; Ctrl+D
  je prepare_and_run_code

  jmp typing_loop ; Doesn't match anything above

%ifndef RUN_CODE
; This is run when ctrl+D is pressed and passed the code and memory locations
; for compiler/interpreter purposes and finally a separate output text buffer
; which will be displayed in the editor after run_code returns.
;
; Input:
;   [ds:si] - the code to run
;   [es:0]  - the code runner memory block.
;             This memory is preserved between successive runs of run_code.
;   di      - output segment address; starts at 0 offset; null terminated
;             This memory is preserved between successive runs of run_code.
run_code:
  mov byte [es:di], "!"
  ret
%endif

prepare_and_run_code:
  call clear_error ; In case this isn't the first try

  ; Close the gap so we have a contiguous buffer
  .close_gap:
  mov byte al, [es:si]
  mov byte [es:di], al
  ; Check for \0 after moving because we want to copy it over
  cmp byte [es:si], 0
  je .gap_closed
  inc di
  inc si
  jmp .close_gap
  .gap_closed:

  ; Clear the text from the screen
  mov dx, MAIN_TOP_LEFT
  mov bl, END_COL-START_COL
  call scroll_text
  ; Set the cursor position to the start of the text area
  mov ah, 0x02
  xor bh, bh
  int 0x10

  push si

  ; source code is [es:si], dest data is [ds:di]
  ; set the source [es:si] for the assembler
  mov di, COMPILER_OUT_LOC
  mov si, [cs:user_code_start]
  call run_code

  mov ax, COMPILER_OUT_LOC
  mov es, ax
  xor si, si
  xor di, di ; set es:si to the same as es:di so the gap buffer code works
  mov dh, START_ROW
  call print_text

  ; Reset the state to continue the editor
  ;
  ; TODO: make an option to save the input text instead of resetting the buffer
  ; with an empty null terminated string. Not sure what I want the controls to
  ; be. Would probably want to reset the lisp memory too at that point.
  pop si
  mov [cs:user_code_start], si
  mov di, si
  add si, GAP_SIZE
  mov byte [es:si], 0 ; null-terminate the string
  mov ax, USER_CODE_LOC
  mov es, ax

  ; Read keyboard, so they can read the output
  mov ah, 0x00
  int 0x16

  ; Clear the text from the screen
  mov dx, MAIN_TOP_LEFT
  mov bl, END_COL-START_COL
  call scroll_text

  mov dx, MAIN_TOP_LEFT
  jmp set_cursor_and_continue

; Print a body of text into the text editor respecting newlines and the screen
; boundaries. Always starts printing at the left edge.
;
; Args:
;  [es:si] - the null terminated text to print
;  cx      - max lines to print, 0 for unlimited
; Returns:
;  cx - the number of lines remaining in the max
;       (or negative lines printed if cx was 0)
;       i.e. cx = (input cx) - lines_printed
;       So if you set cx non-zero this is the number of lines to scroll up
print_text:
  push cx
  jmp .skip_scroll
.print_loop:
  ; scroll up by one line to make room for the next line to print, do this at
  ; the top so it doesn't happen after the last line
  mov al, 0
  mov bl, 1
  mov dx, MAIN_TOP_LEFT
  call scroll_text
  .skip_scroll:

  ; Find the end of the line
  mov bp, si
  call scan_forward

  ; Check if we found the end of the line, and set the right scroll marker
  mov ax, 0x0000 ; scroll markers: set to off ; right margin
  cmp cx, ROW_LENGTH
  jbe .shorter_than_row
  mov cx, ROW_LENGTH ; only print up to the edge of the screen
  mov ax, 0x0100 ; scroll markers: set to on ; right margin
  ; fallthrough
  .shorter_than_row:

  ; move bp to the end of the next line
  cmp byte [es:bp], 0
  je .end_of_buffer
  inc bp ; skip the \n
  ; fallthrough

  .end_of_buffer:
  ; ax was set above
  mov dh, END_ROW
  call set_line_scroll_marker

  ; swap bp and si
  ; after: bp = beginning of line, si = start of next line
  xor si, bp
  xor bp, si
  xor si, bp

  mov dh, END_ROW
  mov dl, START_COL
  call print_line ; prints cx chars from bp

  ; Keep track of the lines printed
  pop cx
  dec cx

  ; If we have run out of our line limit, stop
  ; If cx was 0 for unlimited then this will be negative and not equal to 0
  cmp cx, 0
  je .end
  ; If we hit the end of the buffer, stop
  cmp byte [es:bp], 0
  je .end
  ; If we're still looping push line_count again so we can pop it above
  push cx
  jmp .print_loop

  .end:
  ret

; ==== typing_loop internal helpers that continue the loop ====

; Moves the cursor to the value in dx and continues typing_loop
set_cursor_and_continue:
  mov ah, 0x02
  xor bh, bh
  int 0x10 ; dx is the cursor position
  jmp typing_loop

; Takes an ascii char in al then saves and prints it and continues typing_loop
save_and_print_char:
  lea bx, [si-1]
  cmp di, bx ; disallow typing anywhere when we're out of buffer space
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
  mov cx, bx ; clip the amount to repaint to the current row

  mov ax, 0x0100 ; set to on ; right margin
  call set_line_scroll_marker
  ; fallthrough
  .not_cut_off:
  mov bp, si
  call print_line

  ; The cursor did move from the typing interrupt but lets just set it anyway,
  ; if we didn't the cursor breaks when putting in a debug print in some places
  jmp set_cursor_and_continue

save_new_line:
  lea bx, [si-1]
  cmp di, bx ; disallow typing anywhere when we're out of buffer space
  je typing_loop
  cmp di, USER_CODE_MAX ; Leave a \0 at the end
  je typing_loop

  ; Write the `\n` into the buffer
  mov byte [es:di], `\n`
  inc di

  call maybe_reset_gap
  ; fallthrough
.reset_current_line:

  ; If we put a \n in the first character, so there's no string to scan
  mov bx, [cs:user_code_start]
  inc bx
  cmp di, bx
  ja .not_empty_first_line
  ; Clear it incase we were scolled. We're not doing a scan_forward to check
  mov ax, 0x0000 ; set to off ; right margin
  call set_line_scroll_marker
  jmp .clear_tail
  .not_empty_first_line:

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
  mov dh, START_ROW ; scroll the whole screen
  mov al, 0
  mov bl, 1 ; scroll one line
  call scroll_text
  mov dh, END_ROW ; restore the cursor row
  jmp .print_new_line

  .make_space_in_middle:
  inc dh
  cmp byte [es:bp], 0 ; if this is the last line, we already have space
  je .no_scroll
  mov al, 1 ; shift the text down to leave a space
  mov bl, 1 ; scroll one line
  call scroll_text
  .no_scroll:
  ; fallthrough

  ; Print the tail of the line after the new \n if there is any
  ; (uses cx from scan_forward above)
  .print_new_line:
  ; re-use the saved result of scan_forward from the check above
  cmp cx, ROW_LENGTH
  jbe .not_cut_off
  mov cx, ROW_LENGTH ; clamp to the row length
  mov ax, 0x0100 ; set to on ; right margin
  call set_line_scroll_marker
  ; fallthrough
  .not_cut_off:
  mov bp, si
  call print_line

  jmp set_cursor_and_continue

backspace:
  cmp di, [cs:user_code_start] ; Can't delete past the beginning of the buffer
  je typing_loop

  lea bx, [si-1]
  cmp di, bx ; disallow typing anywhere when we're out of buffer space
  jne .not_full_buffer
  call clear_error
  ; fallthrough
  .not_full_buffer:

  cmp byte [es:di-1], `\n` ; check the char we just "deleted" (di-1 is usually the char before the cursor)
  je _join_lines

  ; If we're at the beginning of the line we don't have to update the view
  cmp dl, START_COL
  jne .delete_mid_line

  dec di ; delete the unseen char by pushing it into the gap

  ; Remove the scroll marker if we just hit the beginning of the line
  cmp di, [cs:user_code_start]
  je .clear_left_marker
  cmp byte [es:di-1], `\n`
  je .clear_left_marker
  jmp typing_loop

  .clear_left_marker:
  mov ax, 0x0001 ; set to off ; left margin
  call set_line_scroll_marker
  jmp typing_loop

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
  push word [es:bp-1] ; save the line ending char in the high byte (you can only push whole words)
                      ; Note: [es:bp+1] may not be safe to read while -1 is here.
  mov byte [es:bp], ' '
  push bp ; save the pointer to the line ending

  ; Repaint the tail of the line
  inc cx ; print the space too
  mov bp, si
  call print_line

  ; Restore the line ending
  pop bp
  pop ax
  mov byte [es:bp], ah

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
  mov bp, si
  call print_line

  jmp set_cursor_and_continue

_repaint_bottom_line:
  push dx
  ; Note: the first iteration will just be the tail of the new joined line
  mov bp, si
  call scan_forward ; skip to the end of the new combined line
  .next_line_loop:
  cmp dh, END_ROW
  je .at_last_line

  ; If we hit the end of the code before the END_ROW, we didn't have a line cut off
  cmp byte [es:bp], 0
  je .no_line_to_print

  inc bp ; skip the \n to scan the next line
  call scan_forward
  inc dh

  jmp .next_line_loop
  .at_last_line:

  ; Set the right scroll marker
  mov ax, 0x0000 ; set to off ; right margin
  cmp cx, ROW_LENGTH
  jbe .right_off
  mov cx, ROW_LENGTH ; also clip the length for printing (reuse the cmp)
  mov ax, 0x0100 ; set to on ; right margin
  .right_off:
  call set_line_scroll_marker

  sub bp, cx
  call print_line ; if we hit the end row, paint the new last screen line
  ; fallthrough

  .no_line_to_print:
  pop dx
  ret

_join_lines:
  dec di ; delete the char by pushing it into the gap

  ; Scroll the text below up (which clears the current line & markers for free)
  mov al, 0 ; shift the text up to cover the current line
  mov bl, 1 ; scroll one line
  call scroll_text

  dec dh ; Move the cursor onto the prev line (must be after scrolling)

; We might be bringing a line up from the bottom of the screen
  call _repaint_bottom_line

.paint_joined_line:
  ; Need to skip scan_backward if we're at di == 0 because we can't check di-1
  xor cx, cx ; if the check below passes, we have length 0
  cmp di, [cs:user_code_start]
  je .print_the_tail

  ; Setup the new current line
  lea bp, [di-1]
  call scan_backward

  cmp cx, ROW_LENGTH
  jae .shift_line_right
.print_the_tail:
  ; set the cursor column to one past the last char on the prev line
  mov dl, START_COL
  add dl, cl

  ; Find the length of the new tail
  mov bp, si
  call scan_forward

  ; Clip the printing to the end of the row
  ; bx = remaining space in the row from the current cursor position
  mov bx, (ROW_LENGTH+START_COL)
  sub bl, dl
  cmp cx, bx
  jb .no_clipping
  mov cx, bx

  mov ax, 0x0100 ; set to on ; right margin
  call set_line_scroll_marker
  .no_clipping:

  mov bp, si
  call print_line
  jmp set_cursor_and_continue
.shift_line_right:
  ; Copy the last char in the row into the gap so we can print it
  mov byte al, [es:si]
  mov byte [es:di], al

  ; If we joined an empty line, we need to print a space
  ; Note: this is the garbage gap space so no need to restore the ending
  cmp byte [es:di], `\n`
  je .space
  cmp byte [es:di], 0
  je .space
  jmp .no_space
  .space:
  mov byte [es:di], ' '
  ; If this happened we have nothing cut off, otherwise we just keep the
  ; existing marker (we know the line had some cut off on the right before this)
  mov ax, 0x0000 ; set to off ; right margin
  call set_line_scroll_marker
  .no_space:

  mov dl, START_COL ; print from the start column
  mov bp, di
  sub bp, ROW_LENGTH-1
  mov cx, ROW_LENGTH
  call print_line
  mov dl, END_COL ; leave the cursor on the last char (first from the joined line)

  jmp set_cursor_and_continue

; Moves the cursor left one nibble then continues typing_loop
;  - Saves the byte in cl when moving to a new byte, also loads the existing
;  data into cl if applicable
;  - Calls move_up when moving to the next line
move_left:
  ; Don't do anything if we're already at the beginning of the buffer
  cmp di, [cs:user_code_start]
  je typing_loop

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
  mov bp, si
  call print_line

  ; Check if we're at the start of the line and clear the left marker if so
  cmp di, [cs:user_code_start]
  je .at_beginning_of_line
  cmp byte [es:di-1], `\n` ; -1 is safe because of the above check
  je .at_beginning_of_line
  jmp .keep_marker
  .at_beginning_of_line:
  mov ax, 0x0001 ; set to off ; left margin
  call set_line_scroll_marker
  .keep_marker:
  jmp set_cursor_and_continue

_prev_line:
  xor cx, cx ; line length is 0 if we're at di==0
  cmp di, [cs:user_code_start] ; skip scanning if the first char is \n
  je .move_up

  lea bp, [di-1] ; scan from the char before the \n
  call scan_backward
  ; fallthrough

  ; Scroll the screen (and set the final cursor row position)
.move_up:
  cmp dh, START_ROW
  jne .no_screen_scroll
  ; We have to make room for the previous line
  mov al, 1
  mov bl, 1
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
  dec cx ; so we set the cursor after the last char not after the extra space
  jmp .skip_repaint ; done

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
  ; Stop the cursor at the end of the user's code
  cmp byte [es:si], 0
  je typing_loop

  mov byte al, [es:si]
  mov byte [es:di], al
  inc di
  inc si ; we know we are not at USER_CODE_MAX because of the \0

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
  mov bx, [cs:user_code_start]
  inc bx
  cmp bp, bx ; if the first char in the line is the first char in the buffer
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
  mov bx, [cs:user_code_start]
  inc bx
  cmp di, bx
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

.move_down:
  cmp dh, END_ROW ; if we're at the bottom
  je .scroll_up
  inc dh ; Next row
  jmp .done

.scroll_up:
  mov dh, START_ROW ; scroll the whole screen
  mov al, 0
  mov bl, 1
  call scroll_text
  mov dh, END_ROW ; restore the cursor row

  mov bp, si
  call scan_forward

  cmp cx, ROW_LENGTH
  jbe .shorter_than_row
  mov ax, 0x0100 ; set to on ; right side
  call set_line_scroll_marker
  mov cx, ROW_LENGTH ; clip to the row
  ; fallthrough
  .shorter_than_row:

  mov bp, si
  call print_line ; cursor position is already correct
  ; fallthrough
.done:
  jmp set_cursor_and_continue

move_up:
  ; Find the start of the current line from the current gap buffer position
  lea bp, [di-1]
  call scan_backward

  cmp bp, [cs:user_code_start]
  jne .not_at_end
  jmp typing_loop
  .not_at_end:

  ; If the current line is scrolled over, repaint it unscrolled
  ;
  ; check the line prefix length against (cursor_col - START_COL)
  ; because it's possible to be scrolled from moving left but the prefix is less
  ; than ROW_LENGTH and in that case we still need to repaint
  xor bh, bh
  mov bl, dl
  sub bl, START_COL
  cmp cx, bx
  jbe .current_line_not_scrolled
  ; reprint the current line unscrolled
  push dx
  mov cx, ROW_LENGTH
  mov dl, START_COL
  call print_line
  mov ax, 0x0001 ; scroll markers: set to on ; right margin
  call set_line_scroll_marker
  mov ax, 0x0100 ; scroll markers: set to on ; right margin
  call set_line_scroll_marker
  pop dx
  .current_line_not_scrolled:

  ; Move bp to the start of the line above
  dec bp ; move bp to the \n that scan_backward found
  cmp bp, [cs:user_code_start] ; if the first char in the buffer was a newline
  je .empty_line_above
  cmp byte [es:bp-1], `\n`
  je .empty_line_above
  dec bp ; skip back to the last character of the line above
  .empty_line_above: ; scan_backward sets cx = 0 and returns
  call scan_backward ; leaves cx = length of line above

  ; If the cursor is at the top of the editor window
  cmp dh, START_ROW
  je .top_row
  dec dh ; move the cursor up one line
  jmp .set_cursor_and_buffer

  .top_row:
  ; Scroll the screen and paint the new top line
  push dx
  ; scroll the screen down one to make room
  mov dx, MAIN_TOP_LEFT
  mov al, 1
  mov bl, 1
  call scroll_text

  ; Set the scroll markers for printing the above line all the way at the left
  mov ax, 0x0000 ; scroll markers: set to off ; right margin
  cmp cx, ROW_LENGTH
  jbe .shorter_than_row
  mov cx, ROW_LENGTH ; only print up to the edge of the screen
  mov ax, 0x0100 ; scroll markers: set to on ; right margin
  ; fallthrough
  .shorter_than_row:
  call set_line_scroll_marker

  call print_line ; print cx chars from bp at dx (MAIN_TOP_LEFT)

  pop dx ; restore the cursor
  ; fallthrough

  .set_cursor_and_buffer:

  ; Choose the cursor and buffer location on the line above
  ;
  ; by .vars_set:
  ;  Set bp = position to leave the gap for the new cursor
  ;  and cx = length from the start of the above line to the new cursor
  ;  and dx = the position to leave the cursor in
  ; bx = number of characters to left edge of the screen
  mov bx, dx
  xor bh, bh
  sub bx, START_COL
  cmp cx, bx
  ja .above_line_is_long_enough

  ; set the cursor to the end of the shorter line
  mov dl, cl
  add dl, START_COL
  add bp, cx
  jmp .vars_set

  .above_line_is_long_enough:
  ; set to current line prefix length into the above line
  ; leave the cursor column where it is
  add bp, bx
  mov cx, bx
  ; fallthrough
  .vars_set:

  ; Move the gap buffer back to the character (bp) for the new cursor position
  .move_gap_back_loop:
  dec di
  dec si
  mov al, [es:di]
  mov [es:si], al
  cmp di, bp
  ja .move_gap_back_loop

  jmp set_cursor_and_continue

move_down:
  jmp typing_loop

; ==== typing_loop helpers that use ret ====

%ifdef CONVERT_LAYOUT
; Takes an ascii char in al (typed in querty) and converts it to dvorak
; Assumes al >= ' ' && al <= '~'
convert_keyboard_layout:
  xor bh, bh
  mov bl, al
  sub bl, ' '
  mov al, [cs:bx + keyboard_map]
  ret
%endif

; Shifts the text up or down N rows in a block (leaving a blank line),
; starting at the current cursor line (used to clear a line for save_new_line or
; join lines in backspace)
;
; Args:
;   dx : The top left of the block to scroll, goes until the bottom right of the
;        text area
;   al : 0 for up, non-zero for down
;   bl : number of rows to scroll
scroll_text:
  push dx ; cursor position
  push cx

  ; set ah = 6 for going up; 7 for going down
  mov ah, 6; BIOS scroll up, means make room at the bottom
  test al, al
  jz .up
  ; .down:
  inc ah ; set the BIOS command for down
  .up:

  mov ch, dh ; start at the cursor row for all 3 (and go until the bottom)
  mov al, bl ; set the number of rows to scroll

  ; Scroll the user code text area
  mov cl, START_COL
  mov dx, MAIN_BOTTOM_RIGHT
  mov bh, MAIN_COLOR ; Also clear bh because it's the page number for write string below
  int 0x10

  ; Scroll the left side scroll markers (even if there's nothing there)
  mov cl, START_COL - 1 ; left one column from the upper left corner (columns are the low bits, so we can save an instruction)
  mov dh, END_ROW
  mov dl, START_COL-1
  mov bh, BORDER_COLOR ; Also clear bh because it's the page number for write string below
  int 0x10

  ; Scroll the right side scroll markers
  ; Note: I reset some registers to the value they should still have
  ;       just in case the BIOS clobbers them
  mov cl, END_COL+1
  mov dh, END_ROW
  mov dl, END_COL+1
  mov bh, BORDER_COLOR ; Also clear bh because it's the page number for write string below
  int 0x10

  pop cx
  pop dx
  ret

; Print a line from the buffer at the starting at the current cursor position
; Move the VGA cursor to the end of the print
;
; Args:
;   [es:bp] : pointer to print from
;   cx      : number of characters to print
;   dx      : cursor position to print from
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
  cmp bp, [cs:user_code_start]
  je .done
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
;  - bp : start pointer to scan from
; Returns:
;  - cx : the length of the line from bp up to the 0 or \n
;  - bp : pointer to the end of the current line (a 0 or a \n)
scan_forward:
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

  ; Move the cursor back
  ;
  ; Mostly this isn't needed because we just use dx and set the cursor at the
  ; end, but sometimes it was causing bugs. So lets just do it for abstraction.
  ; (also there's one case in save_new_line.reset_current_line where we need it)
  mov ah, 0x02
  xor bh, bh ; page number (for both calls)
  int 0x10

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
  jne .reset_gap_with_move

  ; Reset the end of the gap to the gap_start+GAP_SIZE clipped to USER_CODE_MAX
  lea si, [di+GAP_SIZE] ; reset to the default gap size (might overflow)
  cmp di, USER_CODE_MAX-GAP_SIZE ; equivalent to cmp si, USER_CODE_MAX but still works if we overflowed
  jbe .still_have_room
  mov si, USER_CODE_MAX
  .still_have_room:

  mov byte [es:si], 0 ; keep the string null-terminated

  lea bx, [si-1]
  cmp di, bx
  je .no_more_room
  jmp .done

.reset_gap_with_move:
  ; if the gap is not closed don't reset it yet (this is the maybe part)
  lea bx, [si-1]
  cmp di, bx
  jne .done

  ; Scan forward to find the \0 (we know we're not already on it)
  mov bp, si ; Save the beginning
  .scan_forward:
  inc si
  cmp byte [es:si], 0
  jne .scan_forward

  ; set bx = the gap size we're using (which might be clipped)
  mov bx, GAP_SIZE ; set the default gap size if we don't clip it
  cmp si, USER_CODE_MAX-GAP_SIZE
  jbe .shift_gap ; use GAP_SIZE

  ; Calculate the max amount we can expand the gap
  mov bx, USER_CODE_MAX
  sub bx, si

  ; Stop if we're out of space and can't expand the gap at all
  cmp si, USER_CODE_MAX
  jne .shift_gap
  mov si, bp ; reset the end of the gap to where it was
  jmp .no_more_room

  .shift_gap:

  ; Copy from si to si+GAP_SIZE in a loop in reverse order if there's space
  ; stopping at bp

  ; We're copying 2 bytes at a time so we need to fix the alignment
  ; (we know there's >= 2 chars because if it was only \0 or we'd be in the other branch)
  dec si ; start on the char before the last so we can copy 2
  mov ax, si
  sub ax, bp ; ax == num chars - 2 (since the range is [bp, si+1] inclusive)
  test ax, 1
  jz .shift_buffer ; if the number of chars is even, skip the extra byte
  ; Copy the first byte when there's an odd number of characters
  mov byte al, [es:si+1] ; +1 since we started on the second-to-last char
  mov byte [es:bx+si+1], al
  dec si ; now on the third-to-last char (3 is the first odd length)
  .shift_buffer:
  mov word ax, [es:si]
  mov word [es:bx+si], ax
  cmp si, bp
  je .end_shift
  sub si, 2
  jmp .shift_buffer
  .end_shift:

  ; si == bp == original end of gap location
  add si, bx ; set the final position (it was left on the original location)
  jmp .done

.no_more_room:
  ; Print a warning message because we can't allow more typing now
  mov bp, no_more_room_msg
  mov cx, no_more_room_msg_len
  call print_error
  ; fallthrough
  .done:
  ret

; Print an error message at the top of the screen
;
; Args:
;  - [cs:bp] : pointer to the message (this call manages the segment registers)
;  - cx : number of chars to print
print_error:
  push dx

  ; set es to cs since the interrupt reads from [es:bp]
  mov ax, cs
  mov es, ax

  mov dx, MAIN_TOP_LEFT-0x0100 ; one line above the top left corner
  xor bh, bh ; page number 0
  mov bl, ERROR_COLOR
  mov ax, 0x1301 ; write string without moving the cursor
  int 0x10

  ; reset the es segment
  mov ax, USER_CODE_LOC
  mov es, ax

  pop dx
  ret

; Clears any error message (or anything else) right above the text.
; No args. Does not reset the cursor to the dx location at the end.
clear_error:
  ; Clear the buffer full warning if it was full because we're deleting one
  push dx

  ; Move the screen cursor to the message start
  mov dx, MAIN_TOP_LEFT-0x0100 ; one row above the top left
  mov ah, 0x02
  xor bh, bh
  int 0x10

  ; Write spaces to clear the message
  ; Note: this call uses the actual set cursor position not dx
  mov ah, 0x09
  mov al, ' '
  xor bh, bh
  mov bl, BORDER_COLOR
  mov cx, ROW_LENGTH
  int 0x10

  pop dx
  ret

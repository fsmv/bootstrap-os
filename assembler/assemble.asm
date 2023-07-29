; 16 bit x86 assembler that supports the 8086 instruction set in intel syntax
;
; Args:
;  [ds:si] - the source code pointer (null terminated)
;  [es:di] - the destination to write the x86 bytcode
; Returns:
;  [cs:bx] - pointer to error message or 0 on success
;  cx      - length of error message
;  dx      - line number where error occurred
assemble:
  cmp byte [ds:si], 0
  je no_code_error
  .STACK_SIZE: equ 2+2
  .LINE_NUMBER: equ 0
  .SYMBOL_TABLE: equ 2
  ; Zero the locals and move sp
  push 0
  push 0
  mov bp, sp

  ; TODO: support local labels
  ; TODO: db/dw and equ directives
  ; TODO: expression parser

  ; TODO: support DEFAULT_10
  ; TODO: support FAR_JUMP
  ; TODO: support mov segment registers
  ; TODO: support in/out with the al/ax first parameter

  ; TODO: first pass over each line: build the symbol table
  ;  - save the current non-dot label somewhere so we can handle local labels
  ;  - need to handle contextually using relative addresses instead of absolute

  ; We're writing to symbol table to [es:di] before the code we output.
  ; Put in a 0 at the start so that we can call search_data_table on it.
  mov byte [es:di], 0
  mov [bp+.SYMBOL_TABLE], di ; save the start of the symbol table
  inc di ; track the end of the symbol table in di

  push si ; save the start code position before we do the first pass
  mov word [bp+.LINE_NUMBER], 1 ; keep a line number count for error reporting
; First pass over the code to build the symbol table
_label_loop:
  cmp byte [ds:si], 0
  je .finish_label_loop

  call skip_to_the_code
  test al, al
  jnz .next_line_or_done

  call parse_identifier
  cmp byte [ds:si], ':'
  je .save_label
  ; This wasn't a label. Could be an instruction or a syntax error (if it's an
  ; error we'll catch it in the assembling pass)
  call skip_to_end_of_line
  jmp .next_line_or_done

.save_label:
  ; Now that we've seen the ':' check for the invalid ident (parse_identifier result)
  test bx, bx
  jnz .error_ret

  xor dh, dh ; TODO local labels (dh != 0) we use dx below

  ; TODO: for local labels we'll have to copy the local part into the current
  ; parent label storage area so we have a contiguous buffer for passing to
  ; search_data_table in [ds:si]

  ; Search the symbol table for the label we found in the code
  ;  - Finds the place to insert the new label in the sorted list
  ;  - If the label is already in the tabel it's a duplicate definition error
  push bp
  mov bx, [bp+assemble.SYMBOL_TABLE] ; set the search table pointer to the symbol table
  mov bp, dx ; the length of the symbol we're searching for
  sub si, dx ; reset to the beginning of the identifier for search_data_table
  push si
  mov cx, symbol_table.extra_data_size
  call search_data_table
  pop si
  pop bp

  test cx, cx
  jz .duplicate_label_error ; if we found a match

  ; Move data from [es:bx] to the end of the symbol table (di) forward by the
  ; amount of data we need to insert for the new label in the symbol table (cx)
  mov cx, 1  ; space to save the length of the label string
  add cl, dl ; space for the label string
  add cx, symbol_table.extra_data_size
  call shift_data

  ; Write the length of the label to the symbol table
  mov [es:bx], dl
  inc bx

  ; Write the label string
  push bp
  xor bp, bp
  .write_label_loop:
  mov al, [ds:bp+si]
  mov [es:bx], al
  inc bp
  inc bx
  cmp bp, dx
  jne .write_label_loop
  pop bp

  ; Now for the extra_data section
  ; Note: Make sure to update symbol_table.extra_data_size if this changes

  ; Write symbol_table.location = the line number (since it starts unresolved)
  mov cx, [bp+assemble.LINE_NUMBER]
  mov [es:bx], cx
  add bx, 2

  ; Write symbol_table.is_resolved = false
  mov byte [es:bx], 0
  inc bx

  ; TODO: when we support local labels we will have to restore the real [ds:si]
  ;       here because it will be pointing at the parent label buffer for local
  ;       labels

  call skip_to_end_of_line ; we check for syntax errors after the label in _assemble_loop
  jmp .next_line_or_done

.duplicate_label_error:
  mov bx, duplicate_label_error_str
  mov cx, duplicate_label_error_len
  ; fallthrough
; Used to forward an error (in bx, cx) from a subroutine
.error_ret:
  mov dx, [bp+assemble.LINE_NUMBER] ; return the line count in dx (it was in cx previously)
  pop si ; clear the pointer to the start of the code from the stack
  ret

.next_line_or_done:
  inc word [bp+assemble.LINE_NUMBER] ; count the line we just parsed

  cmp byte [ds:si], 0
  je .finish_label_loop
  ; if we're not at the end of the buffer, then we have a \n to skip
  inc si
  jmp _label_loop

.finish_label_loop: ; Note: assumes the line number has been popped
  pop si ; restore the pointer to the beginning of the code

%ifdef TESTER_CALLBACK
  call save_output_code_start
%endif

  ; fallthrough

; DEBUG: print out the symbol table
%if 0
  ; Directly print out the bytes of the symbol table (in case it's really bad)
  xor dx, dx
  mov si, [bp+assemble.SYMBOL_TABLE]
  .raw_print_loop:
  cmp si, di
  je .raw_loop_done

  mov cl, [es:si]
  call print_hex_byte

  mov ah, 0x0E ; Scrolling teletype BIOS routine (used with int 0x10)
  xor bx, bx ; Clear bx. bh = page, bl = color
  mov al, ' '
  int 0x10

  inc si
  inc dl
  test dl, 0x0010
  jz .same_line
  xor dl, dl
  ; next line
  inc dh
  mov ah, 0x0E ; Scrolling teletype BIOS routine (used with int 0x10)
  mov al, `\n`
  int 0x10
  mov al, `\r`
  int 0x10
  .same_line:
  jmp .raw_print_loop
  .raw_loop_done:
  ; set the cursor position for the parsed table
  add dh, 2
  xor dl, dl

  ; Print out the (parsed) symbol table we generated
  mov di, [bp+assemble.SYMBOL_TABLE]
  mov bp, di ; BIOS print string wants [es:bp] so we'll use bp for the symbol table

  .print_symbol_table:
  xor ch, ch
  mov cl, [es:bp] ; the length of the current entry in the symbol table
  test cx, cx
  jz .end_of_table

  mov ax, 0x1301 ; BIOS print string with moving cursor
  mov bx, 0x000C ; page number and attribute
  inc bp ; move the pointer to the start of the string
  int 0x10 ; print the label string
  add bp, cx ; move the pointer forward to the extra data

  mov ah, 0x0E ; Scrolling teletype BIOS routine (used with int 0x10)
  xor bx, bx ; Clear bx. bh = page, bl = color
  mov al, ' '
  int 0x10

  mov cx, [es:bp] ; symbol_table.location
  call print_hex
  add bp, 2

  mov ah, 0x0E ; Scrolling teletype BIOS routine (used with int 0x10)
  xor bx, bx ; Clear bx. bh = page, bl = color
  mov al, ' '
  int 0x10

  mov cl, [es:bp] ; symbol_table.is_resolved
  call print_hex_byte
  inc bp

  inc dh ; print the next one on the next row
  jmp .print_symbol_table

  .end_of_table:
  jmp $ ; Several registers are clobbered now
%endif

  ; Second pass to parse the instructions
  ; TODO: off by one with the line number when returning errors (but not in the tester)
  mov word [bp+assemble.LINE_NUMBER], 1 ; keep a line number count for error reporting
_assemble_loop:
  cmp byte [ds:si], 0
  je done_assembling

  ; Save the assemble base pointer because during _assemble_loop we need to use
  ; bp for the parse_expression base pointer.
  push bp
  ; But also save the symbol table in bp, which is preserved until we call
  ; parse_arguments, which uses this symbol table address as a parameter.
  mov bp, [bp+assemble.SYMBOL_TABLE]

  call skip_to_the_code
  test al, al
  jnz .next_line_or_done

  call parse_identifier
  cmp byte [ds:si], ':'
  je .skip_and_resolve_label

  xor dh, dh ; clear the flag for whether or not it was a local label
  sub si, dx ; move back to the start of the "identifier" we parsed

  jmp .assemble_instruction ; If there was no ':' it might be an instruction or a syntax error

  .skip_and_resolve_label:
  ; Now that we've seen the ':' check for the invalid first char error
  ; This should have already been caught in _label_loop, but why not
  test bx, bx
  jnz .error_ret

  ; TODO: handle local labels

  push bp ; save the symbol table start
  mov bx, bp ; set the search table pointer to the symbol table
  mov bp, dx ; set the length argument for search_data_table
  xor dh, dh ; clear the flag for whether or not it was a local label
  sub si, dx ; move back to the start of the label we parsed
  mov cx, symbol_table.extra_data_size
  call search_data_table
  pop bp

  test cx, cx
  jnz .internal_symbol_not_found_error

  ; Save the resolved byte offset of the label and mark it done
  mov [es:bx+symbol_table.location], di
  mov byte [es:bx+symbol_table.is_resolved], 1

  inc si ; skip the ':'
  ; Skip any whitespace or comments that might be after the label
  call skip_to_the_code
  test al, al
  jnz .next_line_or_done

  ; If there's more code after the label, we allow instructions on the same line
  ; fallthrough

.assemble_instruction:
  ; We have 3 cases for assembling the instruction: jmp, jcc, and all others
  ; (due to the differences in opcode encoding). So we need to check which case
  ; we're in first.

  ; Used to clear the 0x20 bit on lowercase chars to get the uppercase version
  ; Note: Symbols (which have 0x20 set) will just turn into non-printable chars
  TO_UPPER_MASK: equ ~0x20

  ; Check for jmp in a case-insensitive way, also branch to jcc if we see a J
  mov byte al, [ds:si]
  and al, TO_UPPER_MASK
  cmp al, 'J'
  jne .normal_instruction
  mov byte al, [ds:si+1]
  and al, TO_UPPER_MASK
  cmp al, 'M'
  jne .jcc_instruction
  mov byte al, [ds:si+2]
  and al, TO_UPPER_MASK
  cmp al, 'P'
  jne .jcc_instruction
.jmp:
  add si, 3 ; skip the jmp
  mov dx, JMP_FLAGS
  call parse_arguments
  test bx, bx
  jnz .error_ret

  mov al, JMP_NEAR_OPCODE ; default so near and short can share code
  test byte [bp+parse_arguments.OUT_FLAGS], IS_IMM
  jnz .jmp_imm
  ; TODO: far jump
  ; fallthrough

  .jmp_reg:
  call maybe_write_segment_prefix
  ; Write the opcode
  mov al, JMP_REG_OPCODE
  mov [es:di], al
  inc di
  mov cl, JMP_EXTRA_OPCODE ; this is so write_mem_reg can add the value into the modrm byte
  call write_mem_reg
  jmp .next_line_or_done

  .jmp_imm:
  test byte [bp+parse_arguments.OUT_FLAGS], IS_16
  jz .jmp_near
  ; fallthrough (we have a 16 bit imm)
  .jmp_short:
  mov al, JMP_SHORT_OPCODE
  ; fallthrough
  .jmp_near:
  ; Write the opcode
  mov [es:di], al
  inc di
  call write_imm
  jmp .next_line_or_done

.jcc_instruction:
  call search_jcc_instructions
  test cx, cx
  jnz .instruction_not_found_error

  mov cx, bx ; save the pointer to the extra data in cx (which parse_arguments saves)

  mov dx, JCC_FLAGS
  call parse_arguments
  test bx, bx
  jnz .error_ret

  xor al, al ; clear the opcode value so we can add to it for the 16 bit case
  test byte [bp+parse_arguments.OUT_FLAGS], IS_16
  jz .jcc_near
  ; fallthrough (we have a 16 bit imm)

  .jcc_short: ; 16 bit
  mov byte [es:di], 0x0F ; Write the 16 bit jump prefix
  inc di
  mov al, 0x10 ; the offset for 16 bit jump opcodes
  ; fallthrough
  .jcc_near: ; 8 bit
  ; Get the opcode value
  mov bx, cx ; return pointer from search_instructions
  add al, [cs:bx] ; al += opcode from jcc_instructions
  ; Write the opcode
  mov [es:di], al
  inc di
  call write_imm
  jmp .next_line_or_done

.normal_instruction:
  call search_normal_instructions
  test cx, cx
  jnz .instruction_not_found_error

  ; Save the acceptable argument types in dx
  mov dx, [cs:bx+instructions.supported_args]
  mov cx, bx ; save the pointer to the extra data for after parse_arguments

  ; TODO: somehow getting the wrong bytes output. Is the stack foobar'd?
  call parse_arguments
  test bx, bx
  jnz .error_ret

  ; Note: short_reg can happen for reg, imm instructions
  test byte [bp+parse_arguments.OUT_FLAGS], USE_SHORT_SHIFT
  jnz .short_shift_opcode
  test byte [bp+parse_arguments.OUT_FLAGS], USE_SHORT_REG
  jnz .short_reg_opcode
  test byte [bp+parse_arguments.OUT_FLAGS], IS_IMM
  jnz .imm_opcode
  ; fallthrough

.reg_opcode:
  call maybe_write_segment_prefix
  ; Get the opcode value
  mov bx, cx
  mov al, [cs:bx+instructions.reg_opcode]

  ; If we're using a swapped args we need opcode +2 for 8bit and +3 for 16bit
  test byte [bp+parse_arguments.OUT_FLAGS], USE_SWAP_OPCODE
  jz .not_swap_opcode
  add al, 2
  .not_swap_opcode:

  ; For 16 bit we need opcode+1
  test byte [bp+parse_arguments.OUT_FLAGS], IS_16
  jz .8bit_reg_opcode
  inc al
  .8bit_reg_opcode:

  ; Write the opcode to the output
  mov [es:di], al
  inc di

  test byte [bp+parse_arguments.OUT_FLAGS], IS_NO_ARG
  jnz .next_line_or_done ; if it was a no_arg we have nothing else to write

  mov byte cl, [cs:bx+instructions.extra_opcode]
  call write_mem_reg
  jmp .next_line_or_done

.short_reg_opcode:
  ; Get the opcode value
  mov bx, cx
  mov al, [cs:bx+instructions.short_reg_opcode]
  add al, [bp+parse_arguments.MODRM] ; add the arg in (this is how short reg ops work)

  ; Unless we're in a special case short_reg we are always 16 bit so we don't need +1
  test dx, SHORT_REG_AL_ONLY|SHORT_REG_DX_ONLY
  jz .short_reg_opcode_done
  test byte [bp+parse_arguments.OUT_FLAGS], IS_16
  jz .short_reg_opcode_done
  inc al
  .short_reg_opcode_done:

  ; Write the opcode to the output
  mov [es:di], al
  inc di

  ; Only happens for SHORT_REG_AL_ONLY instructions
  test byte [bp+parse_arguments.OUT_FLAGS], IS_IMM
  jz .next_line_or_done
  call write_imm
  jmp .next_line_or_done

.short_shift_opcode:
  test byte [bp+parse_arguments.OUT_FLAGS], IS_IMM
  jnz .shift_one
  mov al, SHIFT_CL_OPCODE
  jmp .shift_have_opcode
  .shift_one:
  mov al, SHIFT_ONE_OPCODE
  ; fallthrough
  .shift_have_opcode:
  test byte [bp+parse_arguments.OUT_FLAGS], IS_16
  jz .shift_not_16
  inc al
  .shift_not_16:

  ; Write the opcode to the output
  mov [es:di], al
  inc di

  mov bx, cx
  mov cl, [cs:bx+instructions.extra_opcode]
  call write_mem_reg ; also includes the extra opcode
  jmp .next_line_or_done

.imm_opcode:
  call maybe_write_segment_prefix
  ; Get the opcode value
  mov bx, cx
  mov al, [cs:bx+instructions.imm_opcode]
  test byte [bp+parse_arguments.OUT_FLAGS], IS_16
  jz .8bit_imm_opcode
  inc al ; for 16 bit we need opcode+1
  .8bit_imm_opcode:
  ; Write the opcode to the output
  mov [es:di], al
  inc di

  test byte [bp+parse_arguments.OUT_FLAGS], USE_EXTRA_OP
  jz .no_modrm
  ; This happens when we have a reg, imm instruction that's not SHORT_REG
  mov cl, [cs:bx+instructions.extra_opcode]
  call write_mem_reg
  .no_modrm:

  call write_imm
  jmp .next_line_or_done

.next_line_or_done:
  pop bp ; restore the assemble base pointer
  ; Note: the next iteration of _assemble_loop will push it again

%ifdef TESTER_CALLBACK
  mov cx, [bp+assemble.LINE_NUMBER]
  call test_assembled_instruction
%endif

  inc word [bp+assemble.LINE_NUMBER] ; count the line we just parsed

  cmp byte [ds:si], 0
  je done_assembling
  ; if we're not at the end of the buffer, then we have a \n to skip
  inc si
  jmp _assemble_loop

.internal_symbol_not_found_error:
  mov bx, internal_symbol_not_found_error_str
  mov cx, internal_symbol_not_found_error_len
  jmp .error_ret
.instruction_not_found_error:
  mov bx, instruction_not_found_error_str
  mov cx, instruction_not_found_error_len
  ; fallthrough
  ; Used to forward an error (in bx, cx) from a subroutine
.error_ret:
  pop bp ; restore the base pointer so we can get the line number
  mov dx, [bp+assemble.LINE_NUMBER] ; return the line count in dx
  add sp, assemble.STACK_SIZE ; pop the local variables
  ret

done_assembling:
  add sp, assemble.STACK_SIZE ; pop the local variables
  test di, di
  jz no_code_error

  ; TODO: When we add the symbol table: Loop through the unresolved expressions
  ;       and resolve them (and fill in in the values in the code)
  ;       - Once we get here we'll have a resolved value for everything in the
  ;         symbol table
  ;       - We'll need to keep a list of pointers to expressions and pointers to
  ;         the destination in the code for the value
  ;       - We'll write this list in the assemble_loop and probably save it on
  ;         the stack if we can

  xor bx, bx
  ret

no_code_error:
  mov dx, 1 ; just always say the error is the first line
  mov bx, no_code_error_str
  mov cx, no_code_error_len
  ret

; Returns
;  - al : 0 if we're not at the end of the line, 1 if we are
skip_to_the_code:
  ; Skip beginning of line whitespace
  call skip_spaces

  ; Skip blank lines
  cmp byte [ds:si], 0
  je .at_end_of_line
  cmp byte [ds:si], `\n`
  je .at_end_of_line

  ; Skip comment lines
  cmp byte [ds:si], ';'
  jne .at_the_code
  call skip_to_end_of_line
  ; fallthrough

  .at_end_of_line:
  mov al, 1
  ret
  .at_the_code:
  xor al, al
  ret

; Matching nasm:
;  > Valid characters in labels are letters, numbers, _, $, #, @, ~, ., and ?
;  > The only characters which may be used as the first character of an
;  > identifier are letters, . (for local labels), _ and ?
;
; Returns:
;  - dl : Length of the identifier (note: nasm's max is 0x0FFF)
;  - dh : 1 if it was a local label, 0 otherwise
;  - bx : 0 if there was a valid ident, pointer to error otherwise
;  - cx : unchanged or the length of the error string
parse_identifier:
  xor dx, dx ; Start at 0, not a local label
  xor bx, bx ; No error

  ; Check for the first char regex: [A-Za-z._?]
  cmp byte [ds:si], 'A'
  jb .first_char_not_letter
  cmp byte [ds:si], 'Z'
  jbe .next_char
  cmp byte [ds:si], 'a'
  jb .first_char_not_letter ; could be _
  cmp byte [ds:si], 'z'
  jbe .next_char
  jmp .invalid_first_char
  .first_char_not_letter:
  cmp byte [ds:si], '.'
  je .local_label
  cmp byte [ds:si], '_'
  je .next_char
  cmp byte [ds:si], '?'
  je .next_char
  jmp .invalid_first_char

.local_label:
  mov dh, 1 ; set the return flag for this
  ; fallthrough
.loop:
  ; Check if it's a valid middle char [0-9?-Za-z_.$#~]
  cmp byte [ds:si], '0'
  jb .maybe_symbol
  cmp byte [ds:si], '9'
  jbe .next_char
  cmp byte [ds:si], '?' ; ? then @ are before A which are both valid chars
  jb .end_of_ident ; no more valid chars between 9 and ?
  cmp byte [ds:si], 'Z'
  jbe .next_char
  cmp byte [ds:si], 'a'
  jb .maybe_symbol ; could be _
  cmp byte [ds:si], 'z'
  jbe .next_char
  ; fallthrough
  .maybe_symbol: ; ? and @ are taken care of above
  cmp byte [ds:si], ' ' ; technically not needed but I want to skip the branches in this common case
  jbe .end_of_ident ; any whitespace or \0
  cmp byte [ds:si], '_'
  je .next_char
  cmp byte [ds:si], '.'
  je .next_char
  cmp byte [ds:si], '$'
  je .next_char
  cmp byte [ds:si], '#'
  je .next_char
  cmp byte [ds:si], '~'
  je .next_char
  jmp .end_of_ident ; not a valid char

  .next_char:
  inc si ; next char
  inc dl ; keep track of the length
  jmp .loop

  .invalid_first_char:
  ; TODO error? but when building the symbol table we just want to skip here...
  mov bx, invalid_first_ident_char_error_str
  mov cx, invalid_first_ident_char_error_len
  jmp .next_char ; still count out the identifier so we can check for the : in the first pass

  .end_of_ident:
  ret

; Wrapper to call search_data_table for normal instructions
;
; Args:
;  - [ds:si] : the input code to search
; Returns
;  - cx      : 0 if it was a match, 1 otherwise
;  - [cs:bx] : pointer to the extra data on match
;  - [ds:si] : advanced to one char past the end of the matched name
search_normal_instructions:
  ; Save es and set es = cs
  mov ax, es
  push ax
  mov ax, cs
  mov es, ax
  mov bx, instructions ; [es:bx] is the search table
  mov cx, instructions.extra_data_size
  xor bp, bp
  call search_data_table
  ; Restore es
  pop ax
  mov es, ax
  ret

; Wrapper to call search_data_table for jcc instructions
;
; Args:
;  - [ds:si] : the input code to search
; Returns
;  - cx      : 0 if it was a match, 1 otherwise
;  - [cs:bx] : pointer to the extra data on match
;  - [ds:si] : advanced to one char past the end of the matched name
search_jcc_instructions:
  ; Save es and set es = cs
  mov ax, es
  push ax
  mov ax, cs
  mov es, ax
  mov bx, jcc_instructions ; [es:bx] is the search table
  mov cx, jcc_instructions.extra_data_size
  xor bp, bp
  call search_data_table
  ; Restore es
  pop ax
  mov es, ax
  ret

; Search a sorted data table (instructions, jcc_instructions, or the symbol
; table) for a match to the text in the input [ds:si] returning the row number
; and pointer to the data.
;
; The algorithm is to search through the sorted list of (size, str) rows by
; first matching just the first character, then the second, etc. until we find
; a match or hit the end.
;
; Note: we couldn't use binary search and null-terminated strings because we
; need to count the index
;
; Args:
;  - [es:bx] : the instruction list to search
;  - [ds:si] : the input code (which will be advanced by this function)
;  - cx      : constant amount of data after each string in the search list
;              which we skip over
;  - bp      : the length of the search string in the code, or 0 if we should
;              use tolower and stop at the first non-letter char (the first mode
;              is for the symbol table, and the other is for instructions)
; Returns:
;  - cx      : 0 if it was a match, 1 otherwise
;  - [es:bx] : pointer to the extra data on match;
;              pointer to the place to insert alphabetically otherwise
;  - [ds:si] : advanced to one char past the end of the matched name
;              or clobbered if no match was found
; Saves dx (for the return value of parse_identifier mostly)
search_data_table:
  push di ; save the output code location
  push dx

  mov di, 0 ; offset into the data table for the string we're currently checking
  ; dx = size of the current key string in the list
  mov dl, [es:bx]
  xor dh, dh

  test dx, dx
  jz .no_match ; empty search list

; Read a search char out of the source code at the current column and validate
; it, stopping the search if we run out of chars in the key identifier.
.next_char_column:
  mov ah, [ds:si] ; read the current char we're searching out of the code

  test bp, bp
  jnz .symbol_table_mode
  ; fallthrough
  .instruction_search_mode:
  ; If we don't know the length, we do tolower (for instructions) and we stop at
  ; the first non-letter char

  sub ah, 'A'
  cmp ah, 'Z'-'A'
  jbe .is_letter
  ; fallthrough
  sub ah, 'a'-'A' ; Save an add instruction to undo the sub ah, 'A' with algebra
  cmp ah, 'z'-'a'
  jbe .is_letter
  jmp .not_letter

  .is_letter:
  add ah, 'a' ; Turn it back into the lowercase ascii char instead of the letter index
  jmp .valid_char

  .not_letter:
  ; [ds:si] is not a valid char (could be \0 or \n)
  test di, di
  jz .no_match ; if the first char was invalid
  ; If we hit an invalid char but we matched the table key the whole way, then
  ; we found a complete match.
  cmp dx, di ; length of key string == current offset
  je .found_match
  jmp .no_match

  .symbol_table_mode:
  ; The caller told us the length of the symbol in the code already (bp) and has
  ; already validated the character set, so we just check the length here.
  cmp bp, di
  jb .valid_char
  je .found_match

  .valid_char:
  inc di ; increase the offset to read from to skip the size byte
  inc si ; Do this now so we end one past the end of the matched string
  ; fallthrough
; For the current character column we're checking in the key (ah) scan down the
; sorted search list until we pass the key or find a match
.check_nth_char:
  ; If the source code text is longer than the key in the table, skip it
  cmp di, dx
  ja .next_row

  ; If the Nth char matches now we need to check the next one
  cmp ah, [es:bx+di]
  je .next_char_column
  jb .no_match ; If we go past the char we're searching for, stop searching

  ; fallthrough (still searching for the char)
; Skip to the next row of the search list (because we're below where the key
; would be)
.next_row:
  inc bx ; +1 to skip the size byte
  add bx, dx ; Skip the length of the key string
  add bx, cx ; skip the extra data

  ; dx = size of the current key string in the list
  mov dl, [es:bx]

  ; Check for the null terminator at the end of the table
  ; If we hit the end we didn't get a match
  test dx, dx
  jz .no_match

  jmp .check_nth_char

.found_match:
  ; Move bx to the extra data for the return
  inc bx ; +1 to skip the size byte
  add bx, dx ; Skip the string length

  pop dx
  pop di ; restore the output code location
  xor cx, cx ; we matched
  ret

.no_match:
  pop dx
  pop di ; restore the output code location
  mov cx, 1 ; no match
  ; bx is already where we want it (the pointer to the first row past the key)
  ret

; Search either reg_8_addressing or reg_16_addressing for a match in [ds:si].
; Leaves si unchanged.
;
; Args:
;  - [cs:bx] : the register list to search (assumes each row is 2 bytes)
; Returns:
;  - cx : -1 if not found, the index otherwise
search_registers:
  xor cx, cx
  mov word ax, [ds:si]
  .check_reg:
  cmp word [cs:bx], 0
  je .not_found
  cmp word ax, [cs:bx]
  je .found
  inc cx
  add bx, 2
  jmp .check_reg
  .not_found:
  mov cx, -1
  .found:
  ret

; Args:
;  - [es:bx] : the start of the buffer to shift forward
;  - [es:di] : one past the end of the buffer to shift forward
;  - cx      : number of bytes to leave space for at [es:bx]
; Returns:
;  - [es:di] : the new end pointer after the shift (di+cx from the args)
; Saves dx and bx, clobbers cx
shift_data:
  ; TODO: deduplicate with .shift_gap in bootstrap-asm.asm (uses different registers)

  push bx
  ; Save the new end pointer for after we do the shift
  mov ax, di
  add ax, cx
  push ax

  mov ax, di
  sub ax, bx ; ax = end - start = num chars
  mov bx, cx ; bx = shift size
  mov cx, ax ; cx = buffer length

  ; We're copying 2 bytes at a time so we need to fix the alignment
  ; (we know there's >= 2 chars because if it was only \0 or we'd be in the other branch)
  sub di, 2 ; start on the char before the last so we can copy 2
  test cx, 1
  jz .shift_buffer ; if the number of chars is even, skip the extra byte
  ; Copy the first byte when there's an odd number of characters
  mov byte al, [es:di+1] ; +1 since we started on the second-to-last char
  mov byte [es:bx+di+1], al
  dec cx
  jz .only_one_byte
  dec di ; now on the third-to-last char (3 is the first odd length)
  .shift_buffer:
  mov word ax, [es:di]
  mov word [es:bx+di], ax
  sub di, 2
  sub cx, 2
  test cx, cx
  jnz .shift_buffer

  .only_one_byte:
  pop di
  pop bx
  ret

; This function cannot call any additional functions or use the stack because it
; relies on return values in the dead stack frame of parse_arguments
write_imm:
  mov ax, [bp+parse_arguments.IMMEDIATE]
  ; x86 is little endian so write the low byte first
  mov [es:di], al
  inc di
  test dx, SHIFT
  jnz .only_one_byte ; shift is always one imm byte even if IS_16 is 1
  test byte [bp+parse_arguments.OUT_FLAGS], IS_16
  jz .only_one_byte
  mov [es:di], ah
  inc di
  .only_one_byte:
  ret

; Write the segment prefix to the output if we have one
;
; This function cannot call any additional functions or use the stack because it
; relies on return values in the dead stack frame of parse_arguments
maybe_write_segment_prefix:
  mov byte al, [bp+parse_arguments.SEGMENT_PREFIX]
  test al, al
  jz .no_segment_prefix
  mov byte [es:di], al
  inc di
  .no_segment_prefix:
  ret

; Arguments:
;  - [bp]    : the output from parse_arguments
;  - cl      : the extra_opcode byte (will check if we need it inside)
;  - [es:di] : the location to write the modrm byte and optional displacement (gets incremented)
;
; This function cannot call any additional functions or use the stack because it
; relies on return values in the dead stack frame of parse_arguments
write_mem_reg:
  mov al, [bp+parse_arguments.MODRM] ; read the modRM byte
  ; Add the extra opcode into the modRM byte if we need to
  test byte [bp+parse_arguments.OUT_FLAGS], USE_EXTRA_OP
  jz .no_extra_op
  shl cl, 3 ; The extra_opcode goes in the middle reg/opcode field (after the low 3 bits which is the R/M field)
  or al, cl ; Add it into the modRM byte
  .no_extra_op:

  ; Write the modRM byte
  mov [es:di], al
  inc di

  ; Write the displacement if needed
  mov bx, [bp+parse_arguments.DISPLACEMENT]
  and al, 0xC7 ; Clear the middle 3 bits reg field
  cmp al, MODRM_ABSOLUTE_ADDRESS
  je .write_disp_16
  and al, 0xC0 ; Clear everything but the mode field
  cmp al, MOD_MEM_DISP8
  je .write_disp_8
  cmp al, MOD_MEM_DISP16
  je .write_disp_16
  ret ; no displacement

  .write_disp_8:
  mov [es:di], bl
  inc di
  ret
  .write_disp_16:
  mov [es:di], bx
  add di, 2
  ret

; TODO: extra references to the intel manual
;  - Instruction format 2.1 Fig 2-1
;    - ModRM 2.1.3
;  - ModRM Table 2.1.5 Fig 2-1
;  - How to read the tables in the instruction reference 3.1. Especially 3.1.1.3

; parse_arguments return flags
IS_16: equ 0x01 ; This bit is 0 for 8 bit and 1 for 16 bit (if 16 bit add 1 to the opcode)
IS_IMM: equ 0x02 ; This bit is 0 for reg_opcodes and 1 for immediate_opcodes
USE_EXTRA_OP: equ 0x04 ; If this bit is 1, we need to add the extra_opcode into the modRM byte
USE_SHORT_REG: equ 0x08 ; If this bit is 1, the modRM byte is just the register code to add to short_reg_opcodes
USE_SWAP_OPCODE: equ 0x10 ; Set to 1 if it's a [reg, reg/mem] instruction (add 2 to the opcode value). Note: IS_IMM must be 0.
USE_SHORT_SHIFT: equ 0x20 ; If 1, then this is a shift [r/m, 1] or [r/m, cl]. The correct opcode is in the first byte of immediate.
IS_FAR_JUMP: equ 0x40 ; TODO
IS_NO_ARG: equ 0x80

; Parse and verify the arguments based on the args flags in dx
;
; Args:
;  - dx : the supported_args flags for this instruction (left unchanged)
;  - [ds:si] : the input code (which will be advanced by this function)
;  - [es:di] : the output pointer (we might write a segment prefix and advance)
;  - bp : the address of the start of the symbol table
; Returns:
;  - bp : pointer on the stack to the args encoding in the order of:
;         modRM, immediate, displacement. Which are present depends on dx.
;  - bx : 0 if sucessful, pointer to error string if not
;  - cx : (opcode index) unchanged if sucessful, error string length if not
parse_arguments:
  ; TODO: need to pass through the unresolved symbol flag in the return so that
  ; we can save the unresolved address locations on the stack in _assemble_loop
  .STACK_SIZE: equ 1+1+1+2+2+2+2 +1 ; out_flags, segment-prefix, modRM, immediate, displacement, symbol table, cx +1 for word alignment
  .SIZE_DETERMINED: equ 0 ; just a boolean for if IS_16 cannot change anymore
  .OUT_FLAGS: equ 1
  .SEGMENT_PREFIX: equ 2
  .MODRM: equ 3
  .IMMEDIATE: equ 4
  .DISPLACEMENT: equ 6
  .INPUT_SYMBOL_TABLE: equ 8
  ; Zero the memory and move the stack pointer
  push bp ; symbol table address
  push 0 ; displacement
  push 0 ; immediate
  push 0 ; modrm, segment_prefix
  push 0 ; out_flags, size_determined
  mov bp, sp ; save the base to reference the variables from
  push cx ; save the instruction index
  ; Note: no need to dec bp because it's all 0 and we want it word aligned anyway

  call skip_spaces

  ; Check if we have "byte" or "word" specified
  ; We can check 2 at a time here because we know we have a line ending char
  cmp word [ds:si], "by"
  je .maybe_byte
  cmp word [ds:si], "wo"
  je .maybe_word
  jmp .first_arg

  .maybe_byte:
  ; In these helpers we have to check one at a time in case we hit the line end
  cmp byte [ds:si+2], 't'
  jne .first_arg
  cmp byte [ds:si+3], 'e'
  jne .first_arg

  add si, 4 ; skip over the parsed text
  mov byte [bp+.SIZE_DETERMINED], 1

  call skip_spaces
  jmp .first_arg

  .maybe_word:
  cmp byte [ds:si+2], 'r'
  jne .first_arg
  cmp byte [ds:si+3], 'd'
  jne .first_arg

  add si, 4 ; skip over the parsed text
  mov byte [bp+.SIZE_DETERMINED], 1
  or byte [bp+.OUT_FLAGS], IS_16

  call skip_spaces
  ; fallthrough

.first_arg:
  cmp byte [ds:si], 0
  je .no_arg
  cmp byte [ds:si], `\n`
  je .no_arg

  cmp byte [ds:si], ';'
  jne .not_comment
  call skip_to_end_of_line
  jmp .no_arg
  .not_comment:

  ; Check for memory addressing mode
  cmp byte [ds:si], '['
  je .first_mem

  xor ax, ax
  call parse_reg_arg
  cmp bx, 0
  jne .error_ret
  cmp cx, -1
  jne .first_reg

; TODO: if the mov segment flag is set, check for segment registers

  push bp
  mov bp, [bp+.INPUT_SYMBOL_TABLE]
  call parse_expression
  pop bp
  cmp bx, 0
  je .one_imm
  ; TODO: check for unresolved expression when we have a symbol table
  jmp .error_ret

.first_mem:
  push bp
  mov bp, [bp+.INPUT_SYMBOL_TABLE]
  call parse_mem_arg
  pop bp
  test cx, cx ; check for errors
  jnz .invalid_memory_deref
  ; We can only have the first arg be a memory arg if we're one of these
  test dx, (ONE_REG|TWO_REG|REG_IMM|SHIFT)
  jz .invalid_argument_error
  ; Save the results from parse_mem_arg to the output
  mov byte [bp+.SEGMENT_PREFIX], al
  mov byte [bp+.MODRM], ah
  mov word [bp+.DISPLACEMENT], bx
  ; No out_flags for this, we just check ModRM and for 0 in the segment byte
  jmp .maybe_second_arg

.first_reg:
  add si, 2 ; skip the register name
  ; Make sure we're allowed to have a first arg as a register
  test dx, (ONE_REG|TWO_REG|REG_IMM|SWAP_TWO_REG|SHORT_REG|SHORT_REG_AL_ONLY|SHORT_REG_DX_ONLY|SHIFT)
  jz .invalid_argument_error

  ; Set the ModRM byte to Register mode and the r/m field to the register
  ; Note if we see a memory arg after this, the reg code will be moved up to the reg/opcode field
  mov al, MOD_REG
  or al, cl
  mov byte [bp+.MODRM], al
  ; Leave IS_IMM on 0
  jmp .maybe_second_arg

.one_imm:
  test dx, ONE_IMM
  jz .invalid_argument_error

  ; Infer the operand size if the instruction only supports 16 bit (nasm does this too)
  test dx, IMM_8
  jnz .not_16bit_only

  ; If they wrote "byte" return an error
  cmp byte [bp+.SIZE_DETERMINED], 0
  je .imm16_only_size_okay
  test byte [bp+.OUT_FLAGS], IS_16
  jnz .imm16_only_size_okay
  jmp .invalid_argument_error
  .imm16_only_size_okay:

  mov byte [bp+.SIZE_DETERMINED], 1
  or byte [bp+.OUT_FLAGS], IS_16

  .not_16bit_only:

  mov word [bp+.IMMEDIATE], ax ; write out the immediate value
  or byte [bp+.OUT_FLAGS], IS_IMM
  test ax, 0xFF00 ; check if the upper bits are 0
  jz .zero_high_bits

  ; It's an error if we specified "byte" and gave a 16 bit immediate
  cmp byte [bp+.SIZE_DETERMINED], 0
  je .imm_size_okay
  test byte [bp+.OUT_FLAGS], IS_16
  jnz .imm_size_okay
  mov bx, operand_size_mismatch_str
  mov cx, operand_size_mismatch_len
  jmp .error_ret
  .imm_size_okay:

  or byte [bp+.OUT_FLAGS], IS_16
  ; fallthrough
  .zero_high_bits:
  jmp .maybe_second_arg ; need to error if there's another arg and skip the tail

.no_arg:
  ; "byte" and "word" prefixes aren't allowed for NO_ARG
  cmp byte [bp+.SIZE_DETERMINED], 0
  jnz .syntax_error

  test dx, NO_ARG
  jz .invalid_argument_error
  mov byte [bp+.OUT_FLAGS], IS_NO_ARG
  jmp .done

.maybe_second_arg:
  cmp byte [ds:si], ','
  je .second_arg

  ; Has only one arg, finish up.
  ;  - Do the final supported_args checks
  ;  - Switch to SHORT_REG if we can
  ;  - Special case for SHORT_REG_DX_ONLY (in and out instructions)

  test byte [bp+.OUT_FLAGS], IS_IMM
  jnz .is_one_imm
  ; .is_one_reg:

  ; Infer IS_16 if it's a mem instruction that only supports REG_16.
  ; So we don't need the "word" specfier in this case. Nasm does this too.
  mov al, byte [bp+.MODRM]
  and al, 0xC0 ; we only want to check the first 2 bits (the mode field)
  cmp al, MOD_REG
  je .no_infer_mem16
  test dx, REG_8
  jnz .no_infer_mem16

  ; It's an error if we specified "byte" but this is a mem op that doesn't support 8 bit
  cmp byte [bp+.SIZE_DETERMINED], 0
  je .reg_size_okay
  test byte [bp+.OUT_FLAGS], IS_16
  jnz .reg_size_okay
  mov bx, operand_size_mismatch_str
  mov cx, operand_size_mismatch_len
  jmp .error_ret
  .reg_size_okay:

  ; It's a mem op that only supports 16 bit
  or byte [bp+.OUT_FLAGS], IS_16
  mov byte [bp+.SIZE_DETERMINED], 1

  .no_infer_mem16:

  cmp byte [bp+.SIZE_DETERMINED], 0
  je .operand_size_not_specified_error

  test byte [bp+.OUT_FLAGS], IS_16
  jnz .one_reg16
  ; .one_reg8
  test dx, REG_8
  jz .invalid_argument_error
  jmp .not_short_reg ; short reg is 16bit only
  .one_reg16:

  test dx, SHORT_REG_DX_ONLY ; check this first because it won't have REG_16
  jnz .short_reg_dx_only
  test dx, REG_16
  jz .invalid_argument_error
  jmp .check_short_reg

  .is_one_imm:
  ; Note: already checked .SIZE_DETERMINED in .one_imm
  test byte [bp+.OUT_FLAGS], IS_16
  jnz .one_imm16
  ; .one_imm8
  test dx, IMM_8
  jz .invalid_argument_error
  jmp .finish_one_arg
  .one_imm16:
  test dx, IMM_16
  jz .invalid_argument_error
  jmp .finish_one_arg

  .check_short_reg:
  ; The instruction must support short reg
  test dx, SHORT_REG
  jz .not_short_reg
  ; Short reg cannot be a memory arg
  mov al, [bp+.MODRM]
  and al, 0xC0 ; Clear everything but the Mode field for checking
  cmp al, MOD_REG
  jne .not_short_reg

  or byte [bp+.OUT_FLAGS], USE_SHORT_REG ; Set the out flag for short reg
  and byte [bp+.MODRM], 0x07 ; Clear everything but the r/m field so we can add this byte to the short_reg_opcode
  jmp .finish_one_arg

  .not_short_reg:
  or byte [bp+.OUT_FLAGS], USE_EXTRA_OP ; One register arg instructions need the extra op
  jmp .finish_one_arg

  .short_reg_dx_only:

  ; in/out allow only an argument of dx
  cmp byte [bp+.MODRM], MOD_REG|0x02 ; 2 is the index for dx
  jne .invalid_argument_error

  or byte [bp+.OUT_FLAGS], USE_SHORT_REG
  mov byte [bp+.MODRM], 0 ; clear this so it can be added to the short_reg_opcodes

  jmp .finish_one_arg

  .finish_one_arg:
  ; Clear the rest of the line and move on
  call skip_spaces
  cmp byte [ds:si], 0
  je .done
  cmp byte [ds:si], `\n`
  je .done
  cmp byte [ds:si], ';'
  ; TODO: special error message
  jne .syntax_error ; if we're not at the end of the line and it's not a comment, we have junk at the end of the line
  call skip_to_end_of_line
  jmp .done

.second_arg:
  inc si ; skip the comma

  call skip_spaces
  cmp byte [ds:si], 0
  je .syntax_error
  cmp byte [ds:si], `\n`
  je .syntax_error

  ; Check for memory addressing mode
  cmp byte [ds:si], '['
  je .second_mem

  ; ax = 0 if this instruction isn't shift, non-zero otherwise
  mov ax, dx
  and ax, SHIFT
  call parse_reg_arg
  cmp bx, 0
  jne .error_ret
  cmp cx, -1
  jne .second_reg

  ; TODO: if the mov segment flag is set, check for segment registers

  push bp
  mov bp, [bp+.INPUT_SYMBOL_TABLE]
  call parse_expression
  pop bp
  cmp bx, 0
  je .second_imm
  ; TODO: check for unresolved expression when we have a symbol table
  jmp .error_ret

.second_mem:
  ; We can only have a second mem arg when we have a swapped order instruction
  test dx, SWAP_TWO_REG
  jz .invalid_argument_error
  ; The first argument must be a register, not memory
  mov byte al, [bp+.MODRM]
  and al, 0xC0 ; clear everything but the mode field (first 2 bits)
  cmp al, MOD_REG
  jne .invalid_argument_error

  ; For les, lds, les we only allow the first arg to be 16 bit
  test dx, LOAD_ADDR_16_ONLY
  jz .not_load_addr
  test byte [bp+.OUT_FLAGS], IS_16
  jz .invalid_argument_error
  .not_load_addr:

  ; Save the reg argument bits for later (just reuse this space)
  and byte [bp+.MODRM], 0x07 ; clear everything but the r/m field
  shl byte [bp+.MODRM], 3 ; move the r/m field into the middle r field

  call parse_mem_arg
  test cx, cx ; check for errors
  jnz .invalid_memory_deref

  cmp byte [bp+.SIZE_DETERMINED], 0
  je .operand_size_not_specified_error

  ; Save the results from parse_mem_arg to the output
  mov byte [bp+.SEGMENT_PREFIX], al
  or byte [bp+.MODRM], ah ; combine the parse_mem_arg modrm with the existing reg argument
  mov word [bp+.DISPLACEMENT], bx
  ; Set the out_flags and finish
  or byte [bp+.OUT_FLAGS], USE_SWAP_OPCODE
  jmp .done

.second_reg:
  add si, 2 ; skip the register name

  test dx, SHIFT
  jz .not_shift_cl
  or byte [bp+.OUT_FLAGS], USE_SHORT_SHIFT|USE_EXTRA_OP
  jmp .done ; don't write the cl arg into ModRM (we need the extra op there)
  .not_shift_cl:

  test dx, TWO_REG
  jz .invalid_argument_error

  shl cl, 3 ; move the register value to the middle reg field
  or byte [bp+.MODRM], cl ; add in the second register argument (mode & r/m is already set)

  jmp .done

.second_imm:
  test dx, REG_IMM|SHIFT|SHORT_REG_AL_ONLY
  jz .invalid_argument_error

  ; Set the out flags
  or byte [bp+.OUT_FLAGS], USE_EXTRA_OP|IS_IMM

  ; Use the short shift opcode if we're just doing shift r/m, 1
  test dx, SHIFT
  jz .not_shift_1
  cmp ax, 1
  jne .not_shift_1
  or byte [bp+.OUT_FLAGS], USE_SHORT_SHIFT|USE_EXTRA_OP
  .not_shift_1:

  ; Note: 16 bit imm does not specfiy the operand size in nasm so we don't do it
  cmp byte [bp+parse_arguments.SIZE_DETERMINED], 0
  je .operand_size_not_specified_error

  ; Use the short reg opcode if we can
  test dx, SHORT_REG_AL_ONLY
  jz .not_short_reg_al
  ; Allowed when we have a MOD_REG arg and the register is al or ax
  mov bl, [bp+.MODRM]
  and bl, 0xC0 ; clear everything but the mode bits
  cmp bl, MOD_REG
  jne .not_short_reg_al
  test byte [bp+.MODRM], 0x7
  jnz .not_short_reg_al ; zero in the r/m field means al or ax
  or byte [bp+.OUT_FLAGS], USE_SHORT_REG
  mov byte [bp+.MODRM], 0 ; clear the MODRM so we can add it to the short_reg_opcode
  .not_short_reg_al:

  mov word [bp+.IMMEDIATE], ax

  test ax, 0xFF00 ; check if the upper bits are 0
  jz .done ; if we're 8 bit we don't need to check size or anything (16 bit means sign extend)

  ; Shift only supports 8bit immedatie
  test dx, SHIFT
  jnz .invalid_argument_error

  ; It's an error if the specified size is 8 bit but we have a 16 bit imm
  test byte [bp+.OUT_FLAGS], IS_16
  jnz .done
  mov bx, operand_size_mismatch_str
  mov cx, operand_size_mismatch_len
  jmp .error_ret

.done:
  ; bp is already the output we want
  pop cx ; restore the instruction index
  xor bx, bx ; no error
  add sp, .STACK_SIZE-2
  ret

.invalid_argument_error:
  mov bx, invalid_argument_error_str
  mov cx, invalid_argument_error_len
  add sp, .STACK_SIZE
  ret

.invalid_memory_deref:
  mov bx, invalid_memory_deref_error_str
  mov cx, invalid_memory_deref_error_len
  add sp, .STACK_SIZE
  ret

.operand_size_not_specified_error:
  mov bx, operand_size_not_specfied_error_str
  mov cx, operand_size_not_specfied_error_len
  add sp, .STACK_SIZE
  ret

.syntax_error:
  mov bx, syntax_error_str
  mov cx, syntax_error_len
.error_ret:
  add sp, .STACK_SIZE
  ret

; Helper for parse_arguments to parse register-direct argument
;
; Arguments
;  - [ds:si] - the current source code pointer
;  - bp : the base pointer from parse_arguments
;  - ax : (non-zero) if this is a second reg shift instruction, 0 otherwise
; Returns
;  - bx : 0 if successful, pointer to error string otherwise
;  - cx : error length, or -1 if no register was found or the register index (regardless of whether it's 16 bit)
;  - [bp+parse_arguments.OUT_FLAGS] : sets IS_16 if needed
;  - [bp+parse_arguments.SIZE_DETERMINED] : checked and set if it wasn't already
;
; Note: When we accept the base pointer from parse_arguments we are in a deeper
; stack frame so it is safe to use the stack in this function.
parse_reg_arg:
  push dx ; Save dx (instruction flags)
  mov dx, ax ; Use dx for the ax arg now (simplifies when we pop)

  ; Check for an 8 bit register name
  mov bx, reg_8_addressing
  call search_registers
  cmp cx, -1
  jne .8bit
  ; Check for a 16 bit register name
  mov bx, reg_16_addressing
  call search_registers
  cmp cx, -1
  jne .16bit
  jmp .success

.8bit:
  cmp dx, 0 ; check the argument that tells us this is the second arg of a SHIFT
  je .not_shift_cl
  cmp cl, 1 ; the index for cl (defined by x86). Note: We already checked that this isn't a 16 bit reg above
  jne .invalid_argument_error

  ; If we have a `shift mem/reg, cl` instruction then cl doesn't determine the
  ; size of the op. This is the last arg so size must be determined here.
  cmp byte [bp+parse_arguments.SIZE_DETERMINED], 0
  jne .success
  pop dx ; restore the instruction flags (clear the stack)
  mov bx, operand_size_not_specfied_error_str
  mov cx, operand_size_not_specfied_error_len
  ret

  .not_shift_cl:

  cmp byte [bp+parse_arguments.SIZE_DETERMINED], 0
  jne .check_size_match
  mov byte [bp+parse_arguments.SIZE_DETERMINED], 1
  jmp .success ; just leave IS_16 at 0

  .check_size_match:
  ; Size was already determined so we have to match 8 bit now
  test byte [bp+parse_arguments.OUT_FLAGS], IS_16
  jnz .size_mismatch_error
  jmp .success

.16bit:
  ; Shift is only allowed cl as the second arg
  cmp dx, 0 ; check the argument that tells us this is the second arg of a SHIFT
  jne .invalid_argument_error

  cmp byte [bp+parse_arguments.SIZE_DETERMINED], 0
  je .set_is_16
  ; Size was already determined so we have to match 16 bit now
  test byte [bp+parse_arguments.OUT_FLAGS], IS_16
  jz .size_mismatch_error
  ; fallthrough
  .set_is_16:
  or byte [bp+parse_arguments.OUT_FLAGS], IS_16

  mov byte [bp+parse_arguments.SIZE_DETERMINED], 1
  ; fallthrough

.success:
  xor bx, bx ; success
  ; fallthrough
  .ret:
  pop dx ; restore the instruction flags
  ret

.size_mismatch_error:
  mov bx, operand_size_mismatch_str
  mov cx, operand_size_mismatch_len
  jmp .ret
.invalid_argument_error:
  mov bx, invalid_argument_error_str
  mov cx, invalid_argument_error_len
  jmp .ret

; Parse a memory addressing argument. Assumes you've already checked for '['.
;
; Args
;  - [ds:si] - the current source code pointer
;  - bp : the address of the start of the symbol table
; Returns
;  - al : the segment prefix or 0 if there isn't one
;  - ah : the modRM byte
;  - bx : the displacement if there is any
;  - cx : 0 if success, 1 if error
parse_mem_arg:
  push dx ; save the instruction flags
  inc si ; skip the [
  cmp byte [ds:si], 0
  je .error

  ; Check if we have a segment register prefix
  xor ax, ax ; clear the segment and modrm output
  mov bx, segment_prefixes-3
  .check_segment_loop:
  add bx, 3
  mov word dx, [cs:bx]
  test dx, dx
  jz .no_segment_prefix ; We hit the end of the list
  cmp word dx, [ds:si]
  jne .check_segment_loop
  ; We found a segment register
  add si, 2 ; Skip over the segment register name
  ; Check for and skip the required colon
  cmp byte [ds:si], ':'
  jne .error
  inc si ; skip the colon
  mov byte al, [cs:bx+2] ; Lookup and write out the prefix byte
  .no_segment_prefix:
  ; ax must now be saved

  ; Search the list of valid memory derefrence operands
  mov cx, -1
  mov bx, mem_addressing-1
  mov ax, si ; Save the beginning so we can reset
  .check_next_mem_string:
  mov si, ax ; Reset to the beginning of the mem arg for the next element
  inc bx ; skip to the next mem operand to check
  inc cx ; count the index for when we get a match
  cmp byte [cs:bx], 0
  je .try_expression ; Hit the end of the list
  .check_mem_string:

  ; Skip spaces around the + char
  cmp byte [ds:si-1], '+'
  je .skip_whitespace ; skip spaces after the +
  cmp byte [cs:bx], '+'
  jne .whitespace_not_allowed ; skip spaces before the +
  ; fallthrough
  .skip_whitespace:
  call skip_spaces
  .whitespace_not_allowed:

  mov byte dl, [ds:si]
  cmp dl, 0
  jz .next_mem_string ; End of the search string means no match
  cmp byte dl, [cs:bx]
  jne .next_mem_string
  inc si
  inc bx
  cmp byte [cs:bx], 0
  jne .check_mem_string ; keep checking for a match
  jmp .found_mem_match
  ; skip to the end of the current mem_addressing string and try the next one
  .next_mem_string:
  cmp byte [cs:bx], 0
  je .check_next_mem_string
  inc bx
  jmp .next_mem_string

  .try_expression:
  mov dx, ax ; save the segment prefix and modrm output
  call parse_expression ; bp is already the symbol table
  cmp bx, 0 ; TODO: check for unresolved expression
  jne .error

  mov bx, ax ; save the parse_expression result in the displacement output
  mov ax, dx ; restore the segment prefix and modrm output
  mov ah, MODRM_ABSOLUTE_ADDRESS ; write the modrm value for this

  call skip_spaces ; finish doesn't do this because we don't want to do it twice in .found_mem_match
  jmp .finish

  .found_mem_match:
  or ah, cl ; Write the r/m field (last 3 bits) with the arg we found
  ; The mode field (upper 2 bits) is 0 for MOD_MEM so don't bother setting it

  ; TODO: allow skipping spaces before this + and - should be allowed here

  ; Check for displacement
  xor bx, bx ; clear the displacement output if there isn't any
  cmp byte [ds:si], '+'
  jne .no_displacement
  inc si ; skip the +
  call skip_spaces

  mov dx, ax ; save the segment prefix and modrm output
  call parse_expression ; bp is already the symbol table
  cmp bx, 0
  jne .error ; TODO: support unresolved expressions
  mov bx, ax ; write the displacement output
  mov ax, dx ; restore the segment prefix and modrm output
  test bx, 0xFF00
  jz .disp_8
  .disp_16:
  or ah, MOD_MEM_DISP16 ; set the mode in the modRM byte
  jmp .no_displacement
  .disp_8:
  or ah, MOD_MEM_DISP8 ; set the mode in the modRM byte
  ; fallthrough
  .no_displacement:

  ; Special case for the argument [bp]: we must encode 0 displacement for this
  ; because with no displacement this R/M field value is used for the absolute
  ; address dereference argument.
  cmp cl, 6 ; The R/M field value for [bp]
  jne .no_zero_displacement
  test ah, 0xC0 ; if the mode field is 0, then there was no displacement
  jnz .no_zero_displacement
  ; Change the mode to displacement so we actually encode [bp+0]
  or ah, MOD_MEM_DISP8
  .no_zero_displacement:
  ; fallthrough

.finish:
  cmp byte [ds:si], ']'
  jne .error ; End square brace is required
  inc si ; skip the ]

  pop dx ; restore the instruction flags
  xor cx, cx ; success
  ret

.error:
  pop dx ; restore the instruction flags
  mov cx, 1
  ret

; Parse an expression, which produces an absolute address.
;
; Accepts: hex literals, char literals, and code-defined labels
;
; Args:
;  - [ds:si] - the current source code pointer
;  - bp : the address of the start of the symbol table
; Returns:
;  - ax : the 8 bit or 16 bit value,
;         or pointer in the input buffer to the unresolved expression
;  - bx : 0 if success, 1 if the expression is unresolved, pointer to error string otherwise
;  - cx : length of error string
parse_expression:
  cmp word [ds:si], '0x' ; note: because of the \0 we can compare one past the end
  je .hex_literal

  cmp byte [ds:si], "'"
  je .char_literal
  cmp byte [ds:si], '"'
  je .char_literal

  call parse_identifier
  test bx, bx
  jz .identifier
  ret ; parse_identifier error

.identifier:
  ; TODO: handle local labels

  xor dh, dh ; clear the flag for whether or not it was a local label
  sub si, dx ; move back to the start of the label we parsed
  mov bx, bp ; set the search table pointer to the symbol table
  mov bp, dx ; set the length argument for search_data_table
  mov cx, symbol_table.extra_data_size
  call search_data_table
  sub si, dx

  test cx, cx
  jnz .symbol_not_found_error

  cmp byte [es:bx+symbol_table.is_resolved], 0
  jne .resolved_ident
  ; Unresolved ident
  mov ax, si
  mov bx, 1
  ret
  .resolved_ident:
  mov ax, [es:bx+symbol_table.location]
  mov bx, 0
  ret
  .symbol_not_found_error:
  mov bx, symbol_not_found_error_str
  mov cx, symbol_not_found_error_len
  ret

.char_literal:
  mov bl, [ds:si] ; save the string start char to match with the ending
  inc si ; skip the string starter char

  ; If we see the string ending, it's an empty string
  cmp [ds:si], bl
  je .invalid_str_len

  ; Save the first char of the string
  mov ah, [ds:si]
  inc si

  ; If we see the ending now, we only had one char
  cmp [ds:si], bl
  je .one_char

  ; Save the second char of the string
  mov al, [ds:si]
  inc si

  ; Now if we don't see the ending it's an error
  cmp [ds:si], bl
  jne .invalid_str_len
  jmp .done_char_literal

  .one_char:
  shr ax, 8 ; move the first char to the low bits
  ; fallthrough
  .done_char_literal:
  inc si ; skip the string ending
  xor bx, bx ; success
  ret

  .invalid_str_len:
  mov bx, invalid_string_length_str
  mov cx, invalid_string_length_len
  ret

.hex_literal:
  add si, 2 ; skip the 0x
  xor ax, ax ; clear the output
  mov cl, 3*4 ; the shift position for the current char (starts with the most significant bits, and one nibble is 4 bits)

  .next_hex_char:
  mov byte bl, [ds:si]

  sub bl, 'a'
  cmp bl, 'f'-'a'
  jbe .hex_letter ; jump if al is between 'a' and 'f'
  sub bl, 'A'-'a' ; -'a' compensates for the sub al, 'a' instruction above
  cmp bl, 'F'-'A'
  jbe .hex_letter
  sub bl, '0'-'A'
  cmp bl, '9'-'0'
  jbe .hex_number

  add cl, 4 ; uncount the non-hex char so we can shift correctly in .done
  ; If it isn't a valid char we're just done with the value, we don't want an
  ; error because there's lots of chars it could be if it's in an expression etc
  jmp .done

  .hex_letter:
  add bl, 0xA ; it's the hex letter - 'A' so it's the value - 0xA
  ; fallthrough
  .hex_number: ; it's actually already the value
  cmp cl, 0
  jl .hex_too_big ; signed compare lets us know if we went too far

  ; write the nibble value to our output
  xor bh, bh ; clear the upper bits so we can shift into them
  shl bx, cl ; move the bits for the nibble into the right position
  or ax, bx ; write the char in the right place in the output

  inc si ; increment the read pointer only at the end, we want to stop one after the hex char above
  ; Count the char we just wrote
  sub cl, 4 ; next char will be 4 bits lower
  jmp .next_hex_char ; we want to know if it's a hex char or not so just always check

  .hex_too_big:
  ; 16 bit assembly means max 4 hex chars
  mov bx, hex_constant_too_big_str
  mov cx, hex_constant_too_big_len
  ret

  .done:
  shr ax, cl ; if we didn't read 4 chars, we have to shift it over
  xor bx, bx
  ret

skip_spaces:
  cmp byte [ds:si], ' '
  jne .skipped_spaces
  inc si
  jmp skip_spaces
  .skipped_spaces:
  ret

skip_to_end_of_line:
  cmp byte [ds:si], 0
  je .at_end
  cmp byte [ds:si], `\n`
  je .at_end
  inc si
  jmp skip_to_end_of_line
  .at_end:
  ret

no_code_error_str: db "There's no assembly code in the text."
no_code_error_len: equ $-no_code_error_str

not_implemented_error_str: db "Not implemented yet."
not_implemented_error_len: equ $-not_implemented_error_str

instruction_not_found_error_str: db "Invalid instruction in the code."
instruction_not_found_error_len: equ $-instruction_not_found_error_str

invalid_argument_error_str: db "Arguments provided are not valid for this instruction."
invalid_argument_error_len: equ $-invalid_argument_error_str

syntax_error_str: db "Invalid syntax in instruction arguments."
syntax_error_len: equ $-syntax_error_str

invalid_memory_deref_error_str: db "Invalid memory deref operand."
invalid_memory_deref_error_len: equ $-invalid_memory_deref_error_str

hex_constant_too_big_str: db "Hex constants can only be 16 bit or 4 chars."
hex_constant_too_big_len: equ $-hex_constant_too_big_str

invalid_string_length_str: db "Character literals must be exactly 1 or 2 chars."
invalid_string_length_len: equ $-invalid_string_length_str

operand_size_mismatch_str: db "Operand sizes (16bit vs 8bit) don't match."
operand_size_mismatch_len: equ $-operand_size_mismatch_str

operand_size_not_specfied_error_str: db 'Operand size could not be inferred. Specify "byte" or "word".'
operand_size_not_specfied_error_len: equ $-operand_size_not_specfied_error_str

invalid_first_ident_char_error_str: db 'Labels can only start with letters or . _ ?'
invalid_first_ident_char_error_len: equ $-invalid_first_ident_char_error_str

duplicate_label_error_str: db 'Found a second definition of this label. Labels must be unique.'
duplicate_label_error_len: equ $-duplicate_label_error_str

internal_symbol_not_found_error_str: db "Internal error: label that we should be resolving was not in the symbol table even though we already parsed the labels."
internal_symbol_not_found_error_len: equ $-internal_symbol_not_found_error_str

symbol_not_found_error_str: db "Label not defined."
symbol_not_found_error_len: equ $-symbol_not_found_error_str

%include "assembler/data.asm"

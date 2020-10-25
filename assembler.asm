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

  ; TODO: db/dw and equ directives
  ; TODO: expression parser

  ; TODO: first pass over each line: build the symbol table
  ;  - save the line number the label is on (skipping comment only lines)
  ;  - save the current non-dot label somewhere so we can handle local labels
  ;  - have a resolved/unresolved flag we can flip as we go and fill in values
  ;  - just write the data right before the assembled code

  ; Second pass to parse the instructions
  mov cx, 1 ; keep a line number count for error reporting
assemble_loop:
  cmp byte [ds:si], 0
  je done_assembling

  ; Skip beginning of line whitespace
  call skip_spaces
  cmp byte [ds:si], 0
  je done_assembling

  ; Skip blank lines
  cmp byte [ds:si], `\n`
  jne .not_blank_line
  inc si ; skip the \n
  inc cx ; count the line
  jmp assemble_loop
  .not_blank_line:

  ; Skip comment lines
  cmp byte [ds:si], ';'
  jne .not_comment
  call skip_to_end_of_line
  cmp byte [ds:si], 0
  je done_assembling
  inc si ; skip the \n
  inc cx ; count the new line
  jmp assemble_loop
  .not_comment:

  push cx ; save the line count so we can use cx

  ; TODO: parse labels and fill in values for line numbers as we hit them

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
  mov cx, JMP_EXTRA_INDEX ; this is so write_mem_reg can add the value into the modrm byte
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
  mov bp, jcc_instructions ; if we matched J we might need to search the jcc list
  call search_instructions
  test bx, bx
  jnz .error_ret

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
  mov bx, jcc_opcodes
  add bx, cx
  add byte al, [cs:bx] ; al += jcc_opcodes[instruction_index]
  ; Write the opcode
  mov byte [es:di], al
  inc di
  call write_imm
  jmp .next_line_or_done

.normal_instruction:
  mov bp, instructions ; the default list to search
  call search_instructions
  test bx, bx
  jnz .error_ret

  ; Lookup the acceptable argument types and parse the args
  mov bx, cx
  shl bx, 1 ; multiply the index by 2 because the flags are 2 bytes
  add bx, supported_args
  mov word dx, [cs:bx]
  call parse_arguments
  test bx, bx
  jnz .error_ret

  ; Note: short_reg can happen for reg, imm instructions
  test byte [bp+parse_arguments.OUT_FLAGS], USE_SHORT_REG
  jnz .short_reg_opcode
  test byte [bp+parse_arguments.OUT_FLAGS], IS_IMM
  jnz .imm_opcode
  ; fallthrough

  .reg_opcode:
  call maybe_write_segment_prefix
  ; Get the opcode value
  mov bx, reg_opcodes
  add bx, cx
  mov byte al, [cs:bx] ; al = reg_opcodes[instruction_index]

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
  mov byte [es:di], al
  inc di

  test byte [bp+parse_arguments.OUT_FLAGS], IS_NO_ARG
  jnz .next_line_or_done ; if it was a no_arg we have nothing else to write

  call write_mem_reg
  jmp .next_line_or_done

  .short_reg_opcode:
  mov bx, short_reg_opcodes
  add bx, cx
  mov byte al, [cs:bx] ; al = short_reg_opcodes[instruction_index]
  add byte al, [bp+parse_arguments.MODRM] ; add the arg in (this is how short reg ops work)

  ; Unless we're in a special case short_reg we are always 16 bit so we don't need +1
  test dx, SHORT_REG_AL_ONLY|SHORT_REG_DX_ONLY
  jz .short_reg_opcode_done
  test byte [bp+parse_arguments.OUT_FLAGS], IS_16
  jz .short_reg_opcode_done
  inc al
  .short_reg_opcode_done:

  ; Write the opcode to the output
  mov byte [es:di], al
  inc di

  ; Only happens for SHORT_REG_AL_ONLY instructions
  test byte [bp+parse_arguments.OUT_FLAGS], IS_IMM
  jz .next_line_or_done
  call write_imm
  jmp .next_line_or_done

  .imm_opcode:
  ; Get the opcode value
  mov bx, immediate_opcodes
  add bx, cx
  mov byte al, [cs:bx] ; al = reg_opcodes[instruction_index]
  test byte [bp+parse_arguments.OUT_FLAGS], IS_16
  jz .8bit_imm_opcode
  inc al ; for 16 bit we need opcode+1
  .8bit_imm_opcode:
  ; Write the opcode to the output
  mov byte [es:di], al
  inc di

  test byte [bp+parse_arguments.OUT_FLAGS], USE_EXTRA_OP
  jz .no_modrm
  ; This happens when we have a reg, imm instruction that's not SHORT_REG
  mov byte al, [bp+parse_arguments.MODRM]
  ; Write the extra opcode into ModRM (copy pasted from .reg_opcode)
  mov bx, extra_opcodes
  add bx, cx
  mov byte ah, [cs:bx] ; ah = extra_opcodes[instruction_index]
  shl ah, 3 ; This goes in the middle reg/opcode field (after the low 3 bits which is the R/M field)
  or al, ah ; Add it into the modRM byte
  ; Write the ModRM to the output
  mov byte [es:di], al
  inc di
  .no_modrm:

  call write_imm
  jmp .next_line_or_done

.next_line_or_done:
  pop cx ; restore the line count

  cmp byte [ds:si], 0
  je done_assembling
  ; if we're not at the end of the buffer, then we have a \n to skip
  inc si
  jmp assemble_loop

; Used to forward an error (in bp, cx) from a subroutine
.error_ret:
  pop dx ; return the line count in dx
  ret

done_assembling:
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
  mov bp, no_code_error_str
  mov cx, no_code_error_len
  ret

invalid_argument_error:
  ; TODO: Add which args are allowed to the string?
  pop dx ; return the line number
  mov bp, invalid_argument_error_str
  mov cx, invalid_argument_error_len
  ret

; Search an instruction list (either instructions or jcc_instructions) for a
; match to the text in the input [ds:si] returning the instruction index
;
; The algorithm is to search through the sorted list of (size, str) rows by
; first matching just the first character, then the second, etc. until we find
; a match or hit the end.
;
; Note: we couldn't use binary search and null-terminated strings because we
; need to count the index
;
; Args:
;  - [cs:bp] : the instruction list to search
;  - [ds:si] : the input code (which will be advanced by this function)
; Returns:
;  - bx : 0 if successful, pointer to error message otherwise
;  - cx : the instruction index (or error message length on error)
;  - [ds:si] : advanced to one char past the end of the instruction name
search_instructions:
  push di ; save the output code location

  mov di, 0 ; offset into each instruction we're currently checking
  xor cx, cx ; the instruction index
  xor bx, bx ; the length of the current instruction string in the list

.next_char:
  inc di ; increase the offset to read from (start at 1)
  mov byte ah, [ds:si] ; save the current char we're searching

  ; Convert to lowercase and throw an error if we found a non-letter char
  sub ah, 'A'
  cmp ah, 'Z'-'A'
  jbe .is_letter
  ; fallthrough
  sub ah, 'a'-'A' ; Save an add instruction to undo the sub ah, 'A' with algebra
  cmp ah, 'z'-'a'
  jbe .is_letter
  ; [ds:si] is not a letter (could be \0 or \n)

  dec di
  jz .error
  cmp bx, di ; if the last one had the same length as the source code, then we're done
  je .found_instruction

  .is_letter:
  add ah, 'a' ; Turn it back into the ascii char instead of the letter number

  inc si ; End one past the end of the instruction
  ; fallthrough
.check_nth_char:
  ; bx = size of the current instruction string in the list
  mov byte bl, [cs:bp]

  test bl, bl ; if we hit the end of the list we didn't get a match
  jnz .not_end
  jmp .error
  .not_end:

  ; If the source code instruction is longer than the one in the list, skip it
  cmp di, bx
  ja .next_instruction
  ; If the Nth char matches now we need to check the next one
  cmp byte [cs:bp+di], ah
  je .next_char

  ; fallthrough (no match)
  .next_instruction:
  add bp, bx ; Keep going to the next instruction in the list
  inc bp ; +1 to skip the size byte
  inc cx
  jmp .check_nth_char

  .found_instruction:
  pop di ; restore the output code location
  xor bx, bx
  ret
  .error:
  pop di ; clear the stack
  mov bx, instruction_not_found_error_str
  mov cx, instruction_not_found_error_len
  ret

; Search either reg_8_addressing or reg_16_addressing for a match in [ds:si].
; Leaves si unchanged.
;
; Args:
;  - bx : the register list to search (assumes each row is 2 bytes)
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

write_imm:
  mov word dx, [bp+parse_arguments.IMMEDIATE]
  ; x86 is little endian so write the low byte first
  mov byte [es:di], dl
  inc di
  test byte [bp+parse_arguments.OUT_FLAGS], IS_16
  jz .only_one_byte
  mov byte [es:di], dh
  inc di
  .only_one_byte:
  ret

; Write the segment prefix to the output if we have one
maybe_write_segment_prefix:
  mov byte al, [bp+parse_arguments.SEGMENT_PREFIX]
  test al, al
  jz .no_segment_prefix
  mov byte [es:di], al
  inc di
  .no_segment_prefix:
  ret

write_mem_reg:
  mov byte al, [bp+parse_arguments.MODRM] ; read the modRM byte
  ; Add the extra opcode into the modRM byte if we need to
  test byte [bp+parse_arguments.OUT_FLAGS], USE_EXTRA_OP
  jz .no_extra_op
  mov bx, extra_opcodes
  add bx, cx
  mov byte ah, [cs:bx] ; ah = extra_opcodes[instruction_index]
  shl ah, 3 ; This goes in the middle reg/opcode field (after the low 3 bits which is the R/M field)
  or al, ah ; Add it into the modRM byte
  .no_extra_op:

  ; Write the modRM byte
  mov byte [es:di], al
  inc di

  ; Write the displacement if needed
  mov word dx, [bp+parse_arguments.DISPLACEMENT]
  and al, 0xC0 ; Clear everything but the mode field
  cmp al, MOD_MEM_DISP8
  je .write_disp_8
  cmp al, MOD_MEM_DISP16
  je .write_disp_16
  ret ; no displacement

  .write_disp_8:
  mov byte [es:di], dl
  inc di
  ret
  .write_disp_16:
  mov word [es:di], dx
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
; Returns:
;  - bp : pointer on the stack to the args encoding in the order of:
;         modRM, immediate, displacement. Which are present depends on dx.
;  - bx : 0 if sucessful, pointer to error string if not
;  - cx : (opcode index) unchanged if sucessful, error string length if not
parse_arguments:
  .STACK_SIZE: equ 1+1+1+2+2+2 +1 ; out_flags, segment-prefix, modRM, immediate, displacement, cx +1 for word alignment
  .OUT_FLAGS: equ 0
  .SEGMENT_PREFIX: equ 1
  .MODRM: equ 2
  .IMMEDIATE: equ 3
  .DISPLACEMENT: equ 5
  ; Zero the memory and move the stack pointer
  push 0 ; displacement
  push 0 ; immediate
  push 0 ; modrm, segment_prefix
  push 0 ; out_flags, 0x00
  mov bp, sp ; save the base to reference the variables from
  push cx ; save the instruction index
  ; Note: no need to dec bp because it's all 0 and we want it word aligned anyway

  ; TODO: check for "word" or "byte"

.first_arg:
  call skip_spaces

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

  ; Check for an 8 bit register name
  mov bx, reg_8_addressing
  call search_registers
  cmp cx, -1
  jne .first_reg ; Leave IS_16 0
  ; Check for a 16 bit register name
  mov bx, reg_16_addressing
  call search_registers
  cmp cx, -1
  je .not_first_reg
  or byte [bp+.OUT_FLAGS], IS_16 ; set the 16 bit flag
  jmp .first_reg
  .not_first_reg:

; TODO: if the mov segment flag is set, check for segment registers

  call parse_expression
  cmp bx, 0
  je .one_imm
  ; TODO: check for unresolved expression when we have a symbol table
  jmp .error_ret

.first_mem:
  call parse_mem_arg
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

  mov word [bp+.IMMEDIATE], ax ; write out the immediate value
  or byte [bp+.OUT_FLAGS], IS_IMM
  test ax, 0xFF00 ; check if the upper bits are 0
  jz .8bit_one_imm
  ; TODO: error if we asked for "byte"
  or byte [bp+.OUT_FLAGS], IS_16
  .8bit_one_imm: ; just leave IS_16 0 (or 1 if the "word" keyword was used)
  jmp .maybe_second_arg ; need to error if there's another arg and skip the tail

.no_arg:
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
  or byte [bp+.OUT_FLAGS], IS_16
  .no_infer_mem16:

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
  test dx, SHORT_REG
  jz .not_short_reg
  or byte [bp+.OUT_FLAGS], USE_SHORT_REG ; Set the out flag for short reg
  and byte [bp+.MODRM], 0x07 ; Clear everything but the r/m field so we can add this byte to the short_reg_opcode
  jmp .finish_one_arg
  .not_short_reg:
  or byte [bp+.OUT_FLAGS], USE_EXTRA_OP ; One register arg instructions need the extra op
  jmp .finish_one_arg

  .short_reg_dx_only:
  ; TODO requires the byte or word prefix

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

  ; TODO: error checking if we have reg, reg and the sizes don't match
  ; Check for an 8 bit register name
  mov bx, reg_8_addressing
  call search_registers
  cmp cx, -1
  jne .second_reg ; Leave IS_16 0
  ; Check for a 16 bit register name
  mov bx, reg_16_addressing
  call search_registers
  cmp cx, -1
  je .not_second_reg
  ; Shift can only use cl even if it is a word shift
  test dx, SHIFT
  jnz .invalid_argument_error

  or byte [bp+.OUT_FLAGS], IS_16 ; set the 16 bit flag
  jmp .second_reg
  .not_second_reg:

  ; TODO: if the mov segment flag is set, check for segment registers

  call parse_expression
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
  ; Shift can only use cl as the second reg arg
  cmp cl, 1 ; the index for cl (defined by x86). Note: We already checked that this isn't a 16 bit reg above
  jne .invalid_argument_error
  or byte [bp+.OUT_FLAGS], USE_SHORT_SHIFT

  ; TODO: requires "byte" or "word" if the first arg was mem
  .not_shift_cl:

  test dx, TWO_REG|SHIFT
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
  or byte [bp+.OUT_FLAGS], USE_SHORT_SHIFT
  .not_shift_1:

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
  jz .8bit_second_imm
  ; Shift only supports 8bit immedatie
  test dx, SHIFT
  jnz .invalid_argument_error
  ; TODO: error if we asked for "byte"
  or byte [bp+.OUT_FLAGS], IS_16
  .8bit_second_imm: ; just leave IS_16 0 (or 1 if the "word" keyword was used)

  jmp .done

.done:
  ; bp is already the output we want
  pop cx ; restore the line count
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

.syntax_error:
  mov bx, syntax_error_str
  mov cx, syntax_error_len
.error_ret:
  add sp, .STACK_SIZE
  ret

; Parse a memory addressing argument. Assumes you've already checked for '['.
;
; Returns
;  - al : the segment prefix or 0 if there isn't one
;  - ah : the modRM byte
;  - bx : the displacement if there is any
;  - cx : 0 if success, 1 if error
parse_mem_arg:
  push bp ; save the parse_arguments pointer
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
  mov bp, si ; Save the beginning so we can reset
  .check_next_mem_string:
  mov si, bp ; Reset to the beginning of the mem arg for the next element
  inc bx ; skip to the next mem operand to check
  inc cx ; count the index for when we get a match
  cmp byte [cs:bx], 0
  je .error ; Hit the end of the list
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

  .found_mem_match:
  or ah, cl ; Write the r/m field (last 3 bits) with the arg we found
  ; The mode field (upper 2 bits) is 0 for MOD_MEM so don't bother setting it

  ; TODO [disp16] i.e. absolute address
  ; TODO: allow skipping spaces before this + and - should be allowed here

  ; Check for displacement
  xor bx, bx ; clear the displacement output if there isn't any
  cmp byte [ds:si], '+'
  jne .no_displacement
  inc si ; skip the +
  call skip_spaces

  mov dx, ax ; save the segment prefix and modrm output
  call parse_expression
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

  ; [bp] must have 1 byte of 0 displacement because with no displacement
  cmp cl, 6 ; [bp]
  jne .no_zero_displacement
  test ah, 0xC0 ; first 2 bits, the mode field of the modRM byte
  jnz .no_zero_displacement
  ; if we're using [bp] and the first 2 bits are zero then we're in mem mode
  or ah, MOD_MEM_DISP8 ; we need to write a 0 byte for displacement
  .no_zero_displacement:

  cmp byte [ds:si], ']'
  jne .error ; End square brace is required
  inc si ; skip the ]

  pop dx ; restore the instruction flags
  pop bp ; restore the parse_arguments pointer
  xor cx, cx ; success
  ret

  .error:
  pop dx ; restore the instruction flags
  pop bp ; restore the parse_arguments pointer
  mov cx, 1
  ret

; Returns:
;  - ax : the 8 bit or 16 bit value,
;         or pointer in the input buffer to the unresolved expression
;  - bx : 0 if success, 1 if the expression is unresolved, pointer to error string otherwise
;  - cx : length of error string
parse_expression:
  ; TODO: char literals
  ; TODO: check the symbol table

  cmp word [ds:si], '0x' ; note: because of the \0 we can compare one past the end
  je .hex_literal

  mov bx, not_implemented_error_str
  mov cx, not_implemented_error_len
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

; MOD_* constants and *_addressing constants come from:
; Vol 2. Chapter 2.1.5 Table 2-1

; The first 2 bits of the ModRM byte
MOD_MEM:        equ 0x00
MOD_MEM_DISP8:  equ 0x40
MOD_MEM_DISP16: equ 0x80
MOD_REG:        equ 0xC0

; In order. Count to get the 3 bit R/M field value
mem_addressing:
db 'bx+si',0
db 'bx+di',0
db 'bp+si',0
db 'bp+di',0
db 'si',0
db 'di',0
db 'bp',0
db 'bx',0
db 0

; In order. Count to get the 3 bit R/M field value
reg_8_addressing:
db 'al'
db 'cl'
db 'dl'
db 'bl'
db 'ah'
db 'ch'
db 'dh'
db 'bh'
dw 0

; In order. Count to get the 3 bit R/M field value
reg_16_addressing:
db 'ax'
db 'cx'
db 'dx'
db 'bx'
db 'sp'
db 'bp'
db 'si'
db 'di'
dw 0

; The segment register names and their prefix byte values
;  - Prefix byte values are from Vol 2. Chapter 2.1.1
;  - These are in order of the Sreg arg (for mov) from Vol 2. Chapter 3.1.1.3
segment_prefixes:
db 'es', 0x26
db 'cs', 0x2E
db 'ss', 0x36
db 'ds', 0x3E
dw 0

; These are only the original 8086 instructions.
; List from Vol 3. Chapter 20.1.3
instructions:
db 3,'aaa'
db 3,'aad'
db 3,'aam'
db 3,'aas'
db 3,'adc'
db 3,'add'
db 3,'and'
db 4,'call'
db 3,'cbw'
db 3,'clc'
db 3,'cld'
db 3,'cmc'
db 3,'cmp'
db 3,'cwd'
db 3,'daa'
db 3,'das'
db 3,'dec'
db 3,'div'
db 3,'hlt'
db 4,'idiv'
db 4,'imul'
db 2,'in'
db 3,'inc'
db 4,'insb'
db 4,'insw'
db 3,'int'
db 4,'into'
db 4,'iret'
db 4,'lahf'
db 3,'lds'
db 3,'lea'
db 3,'les'
db 4,'lock'
db 4,'loop'
db 5,'loope'
db 6,'loopne'
db 3,'mov'
db 3,'mul'
db 3,'neg'
db 3,'nop'
db 3,'not'
db 2,'or'
db 3,'out'
db 5,'outsb'
db 5,'outsw'
db 3,'pop'
db 4,'popf'
db 4,'push'
db 5,'pushf'
db 3,'rcl'
db 3,'rcr'
db 3,'ret'
db 4,'retf'
db 3,'rol'
db 3,'ror'
db 4,'sahf'
db 3,'sal'
db 3,'sar'
db 3,'sbb'
db 3,'shl'
db 3,'shr'
db 3,'stc'
db 3,'std'
db 3,'sub'
db 4,'test'
db 4,'xchg'
db 5,'xlatb'
db 3,'xor'
db 0

; Conditional jumps (jcc) are in a separate ID space from the main instructions
; because there's a different pattern for 8/16 bit immediate opcodes and we
; don't need the args type flags
jcc_instructions:
db 2,'ja'
db 3,'jae'
db 2,'jb'
db 3,'jbe'
db 2,'jc'
db 4,'jcxz'
db 2,'je'
db 2,'jg'
db 3,'jge'
db 2,'jl'
db 3,'jle'
db 3,'jna'
db 4,'jnae'
db 3,'jnb'
db 4,'jnbe'
db 3,'jnc'
db 3,'jne'
db 3,'jng'
db 4,'jnge'
db 3,'jnl'
db 4,'jnle'
db 3,'jno'
db 3,'jnp'
db 3,'jns'
db 3,'jnz'
db 2,'jo'
db 2,'jp'
db 3,'jpe'
db 3,'jpo'
db 2,'js'
db 2,'jz'
db 0

; These are the short (8bit) jcc opcodes.
; To get the near (16bit) jcc opcodes: prefix with 0F and add 0x10
jcc_opcodes:
db 0x77, ; ja
db 0x73, ; jae
db 0x72, ; jb
db 0x76, ; jbe
db 0x72, ; jc
db 0xE3, ; jcxz    Note: Special because it has no "near" jump opcode
db 0x74, ; je
db 0x7F, ; jg
db 0x7D, ; jge
db 0x7C, ; jl
db 0x7E, ; jle
db 0x76, ; jna
db 0x72, ; jnae
db 0x73, ; jnb
db 0x77, ; jnbe
db 0x73, ; jnc
db 0x75, ; jne
db 0x7E, ; jng
db 0x7C, ; jnge
db 0x7D, ; jnl
db 0x7F, ; jnle
db 0x71, ; jno
db 0x7B, ; jnp
db 0x79, ; jns
db 0x75, ; jnz
db 0x70, ; jo
db 0x7A, ; jp
db 0x7A, ; jpe
db 0x7B, ; jpo
db 0x78, ; js
db 0x74, ; jz

; Argument format options for the instructions (bit flags you can OR together)
; Note: These aren't in the intel manual, it's just a classification I made up
NO_ARG:       equ 0x0001
REG_8:        equ 0x0002
REG_16:       equ 0x0004
ONE_REG:      equ REG_8|REG_16 ; reg/mem (might have a short_reg_opcode)
IMM_16:       equ 0x0008 ; 16 bit immediate only
IMM_8:        equ 0x0010 ; 8 bit immediate only
ONE_IMM:      equ IMM_8|IMM_16
TWO_REG:      equ 0x0020 ; reg/mem, reg
REG_IMM:      equ 0x0040 ; reg/mem, 8 bit or 16 bit (might have a short_reg_opcode)
SWAP_TWO_REG: equ 0x0080 ; reg, mem (encoded with opcode+=2)
; Less common argument options
SHORT_REG:         equ 0x0100 ; A 16 bit reg arg can be added into the short_reg_opcode to save the modRM byte
SHORT_REG_AL_ONLY: equ 0x0200 ; only al/ax, imm
SHORT_REG_DX_ONLY: equ 0x0400 ; only allows a single arg of dx
DEFAULT_10:        equ 0x0800 ; aad and aam have a special default arg of 10 and 8bit imm otherwise
LOAD_ADDR_16_ONLY: equ 0x1000 ; lea, lds, les all are reg16, mem only
SHIFT:             equ 0x2000 ; reg/mem, imm8 or reg/mem, 1 or reg/mem, cl
FAR_JUMP:          equ 0x4000
MOV_SEGMENT:       equ 0x8000

; All information below is copied out of Vol 2 and the pages for each instruction

SHIFT_ONE_OPCODE: equ 0xD0
SHIFT_CL_OPCODE:  equ 0xD2
JMP_SHORT_OPCODE: equ 0xEB
JMP_NEAR_OPCODE:  equ 0xE9
JMP_REG_OPCODE:   equ 0xFF
READ_SEGMENT_OPCODE:  equ 0x8C
WRITE_SEGMENT_OPCODE: equ 0x8E

JCC_FLAGS: equ ONE_IMM
JMP_FLAGS: equ ONE_IMM|REG_16|FAR_JUMP
JMP_EXTRA_INDEX: equ byte JMP_EXTRA_ADDR-extra_opcodes ; Note: the opcodes are one byte each

supported_args:
dw NO_ARG, ; aaa
dw IMM_8|DEFAULT_10, ; aad
dw IMM_8|DEFAULT_10, ; aam
dw NO_ARG, ; aas
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; adc
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; add
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; and
dw IMM_16|REG_16|FAR_JUMP, ; call
dw NO_ARG, ; cbw
dw NO_ARG, ; clc
dw NO_ARG, ; cld
dw NO_ARG, ; cmc
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; cmp
dw NO_ARG, ; cwd
dw NO_ARG, ; daa
dw NO_ARG, ; das
dw ONE_REG|SHORT_REG, ; dec
dw ONE_REG, ; div
dw NO_ARG, ; hlt
dw ONE_REG, ; idiv
dw ONE_REG, ; imul Note: the other 2 forms weren't on the 8086
dw IMM_8|SHORT_REG_DX_ONLY, ; in `in al/ax, imm8` or `in al/ax, dx` are the only forms
dw ONE_REG|SHORT_REG, ; inc
dw NO_ARG, ; insb
dw NO_ARG, ; insw
dw IMM_8, ; int
dw NO_ARG, ; into
dw NO_ARG, ; iret
dw NO_ARG, ; lahf
dw SWAP_TWO_REG|LOAD_ADDR_16_ONLY, ; lds
dw SWAP_TWO_REG|LOAD_ADDR_16_ONLY, ; lea
dw SWAP_TWO_REG|LOAD_ADDR_16_ONLY, ; les
dw NO_ARG, ; lock
dw IMM_8, ; loop
dw IMM_8, ; loope
dw IMM_8, ; loopne
dw TWO_REG|SWAP_TWO_REG|REG_IMM|SHORT_REG|MOV_SEGMENT, ; mov
dw ONE_REG, ; mul
dw ONE_REG, ; neg
dw NO_ARG, ; nop
dw ONE_REG, ; not
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; or
dw IMM_8|SHORT_REG_DX_ONLY, ; out
dw NO_ARG, ; outsb
dw NO_ARG, ; outsw
dw REG_16|SHORT_REG, ; pop   Note: there are special segment register opcodes we don't support
dw NO_ARG, ; popf
dw REG_16|IMM_16|SHORT_REG, ; push   Note: there are special segment register opcodes plus it has IMM_8 just not following the pattern
dw NO_ARG, ; pushf
dw SHIFT, ; rcl
dw SHIFT, ; rcr
dw NO_ARG|IMM_16, ; ret
dw NO_ARG|IMM_16, ; retf
dw SHIFT, ; rol
dw SHIFT, ; ror
dw NO_ARG, ; sahf
dw SHIFT, ; sal
dw SHIFT, ; sar
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; sbb
dw SHIFT, ; shl
dw SHIFT, ; shr
dw NO_ARG, ; stc
dw NO_ARG, ; std
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; sub
dw REG_IMM|TWO_REG|SHORT_REG_AL_ONLY, ; test
dw TWO_REG, ; xchg Note: also has a short-reg with just al we don't support because it's different
dw NO_ARG, ; xlatb
dw REG_IMM|TWO_REG|SWAP_TWO_REG|SHORT_REG_AL_ONLY, ; xor

; TODO: should we combine these lookup tables and just make each row a tuple?
;       note: 8086 mode doesn't support [ebx+4*ecx]
;       also takes up the same amount of space either way

reg_opcodes:
db 0x37, ; aaa
db 0x00, ; aad
db 0x00, ; aam
db 0x3F, ; aas
db 0x10, ; adc
db 0x00, ; add (base value is 0)
db 0x20, ; and
db 0xFE, ; call
db 0x98, ; cbw
db 0xF8, ; clc
db 0xFC, ; cld
db 0xF5, ; cmc
db 0x38, ; cmp
db 0x99, ; cwd
db 0x27, ; daa
db 0x2F, ; das
db 0xFE, ; dec
db 0xF6, ; div
db 0xF4, ; hlt
db 0xF6, ; idiv
db 0xF6, ; imul
db 0x00, ; in
db 0xFE, ; inc
db 0x6C, ; insb
db 0x6D, ; insw
db 0x00, ; int
db 0xCE, ; into
db 0xCF, ; iret
db 0x9F, ; lahf
db 0xC5-3, ; lds Note: -3 because these are reg16, mem only, which does opcode+3
db 0x8D-3, ; lea
db 0xC4-3, ; les
db 0xF0, ; lock
db 0x00, ; loop
db 0x00, ; loope
db 0x00, ; loopne
db 0x88, ; mov
db 0xF6, ; mul
db 0xF6, ; neg
db 0x90, ; nop
db 0xF6, ; not
db 0x08, ; or
db 0x00, ; out
db 0x6E, ; outsb
db 0x6F, ; outsw
db 0x8F, ; pop
db 0x9D, ; popf
db 0xFF, ; push
db 0x9C, ; pushf
db 0x00, ; rcl
db 0x00, ; rcr
db 0xC3, ; ret
db 0xCB, ; retf
db 0x00, ; rol
db 0x00, ; ror
db 0x9E, ; sahf
db 0x00, ; sal
db 0x00, ; sar
db 0x18, ; sbb
db 0x00, ; shl
db 0x00, ; shr
db 0xF9, ; stc
db 0xFD, ; std
db 0x28, ; sub
db 0x84, ; test
db 0x86, ; xchg
db 0xD7, ; xlatb
db 0x30, ; xor

immediate_opcodes:
db 0x00, ; aaa
db 0xD5, ; aad
db 0xD4, ; aam
db 0x00, ; aas
db 0x80, ; adc
db 0x80, ; add
db 0x80, ; and
db 0xE7, ; call (opcode-1 since it's 16bit only)
db 0x00, ; cbw
db 0x00, ; clc
db 0x00, ; cld
db 0x00, ; cmc
db 0x80, ; cmp
db 0x00, ; cwd
db 0x00, ; daa
db 0x00, ; das
db 0x00, ; dec
db 0x00, ; div
db 0x00, ; hlt
db 0x00, ; idiv
db 0x00, ; imul
db 0xE4, ; in
db 0x00, ; inc
db 0x00, ; insb
db 0x00, ; insw
db 0xCD, ; int
db 0x00, ; into
db 0x00, ; iret
db 0x00, ; lahf
db 0x00, ; lds
db 0x00, ; lea
db 0x00, ; les
db 0x00, ; lock
db 0xE2, ; loop
db 0xE1, ; loope
db 0xE0, ; loopne
db 0xC6, ; mov
db 0x00, ; mul
db 0x00, ; neg
db 0x00, ; nop
db 0x00, ; not
db 0x80, ; or
db 0x00, ; out
db 0x00, ; outsb
db 0x00, ; outsw
db 0x00, ; pop
db 0x00, ; popf
db 0x67, ; push (opcode-1 since it's 16bit only)
db 0x00, ; pushf
db 0xC0, ; rcl
db 0xC0, ; rcr
db 0xC1, ; ret (opcode-1 since it's 16bit only)
db 0xC9, ; retf (opcode-1 since it's 16bit only)
db 0xC0, ; rol
db 0xC0, ; ror
db 0x00, ; sahf
db 0xC0, ; sal
db 0xC0, ; sar
db 0x80, ; sbb
db 0xC0, ; shl
db 0xC0, ; shr
db 0x00, ; stc
db 0x00, ; std
db 0x80, ; sub
db 0xF6, ; test
db 0x00, ; xchg
db 0x00, ; xlatb
db 0x80, ; xor

; This is the "/digit" shown in the intel manual. It goes in the register bits
; in the ModRM byte for instructions with only 1 register argument sometimes.
extra_opcodes:
db 0x00, ; aaa
db 0x00, ; aad
db 0x00, ; aam
db 0x00, ; aas
db 0x02, ; adc
db 0x00, ; add  Note: actually needs the value 0
db 0x04, ; and
db 0x02, ; call
db 0x00, ; cbw
db 0x00, ; clc
db 0x00, ; cld
db 0x00, ; cmc
db 0x07, ; cmp
db 0x00, ; cwd
db 0x00, ; daa
db 0x00, ; das
db 0x01, ; dec
db 0x06, ; div
db 0x00, ; hlt
db 0x00, ; idiv
db 0x00, ; imul
db 0x00, ; in
db 0x00, ; inc (has value 0)
db 0x00, ; insb
db 0x00, ; insw
db 0x00, ; int
db 0x00, ; into
db 0x00, ; iret
db 0x00, ; lahf
db 0x00, ; lds
db 0x00, ; lea
db 0x00, ; les
db 0x00, ; lock
db 0x00, ; loop
db 0x00, ; loope
db 0x00, ; loopne
db 0x00, ; mov
db 0x04, ; mul
db 0x03, ; neg
db 0x00, ; nop
db 0x02, ; not
db 0x01, ; or
db 0x00, ; out
db 0x00, ; outsb
db 0x00, ; outsw
db 0x00, ; pop
db 0x00, ; popf
db 0x06, ; push
db 0x00, ; pushf
db 0x02, ; rcl
db 0x03, ; rcr
db 0x00, ; ret
db 0x00, ; retf
db 0x00, ; rol (value 0)
db 0x01, ; ror
db 0x00, ; sahf
db 0x04, ; sal
db 0x07, ; sar
db 0x03, ; sbb
db 0x04, ; shl
db 0x05, ; shr
db 0x00, ; stc
db 0x00, ; std
db 0x05, ; sub
db 0x00, ; test (value 0)
db 0x00, ; xchg
db 0x00, ; xlatb
db 0x06, ; xor
; Hack: Put jmp at the end even though it's not in the search list,
;       so that we can re-use write_mem_write to get this value
JMP_EXTRA_ADDR:
db 0x04, ; jmp

short_reg_opcodes:
db 0x00, ; aaa
db 0x00, ; aad
db 0x00, ; aam
db 0x00, ; aas
db 0x14, ; adc
db 0x04, ; add
db 0x24, ; and
db 0x00, ; call
db 0x00, ; cbw
db 0x00, ; clc
db 0x00, ; cld
db 0x00, ; cmc
db 0x3C, ; cmp
db 0x00, ; cwd
db 0x00, ; daa
db 0x00, ; das
db 0x48, ; dec
db 0x00, ; div
db 0x00, ; hlt
db 0x00, ; idiv
db 0x00, ; imul
db 0xEC, ; in
db 0x40, ; inc
db 0x00, ; insb
db 0x00, ; insw
db 0x00, ; int
db 0x00, ; into
db 0x00, ; iret
db 0x00, ; lahf
db 0x00, ; lds
db 0x00, ; lea
db 0x00, ; les
db 0x00, ; lock
db 0x00, ; loop
db 0x00, ; loope
db 0x00, ; loopne
db 0xB8, ; mov Note: there's also an 8bit short-reg op for this but we don't support it
db 0x00, ; mul
db 0x00, ; neg
db 0x00, ; nop
db 0x00, ; not
db 0x0C, ; or
db 0x00, ; out
db 0x00, ; outsb
db 0x00, ; outsw
db 0x58, ; pop
db 0x00, ; popf
db 0x50, ; push
db 0x00, ; pushf
db 0x00, ; rcl
db 0x00, ; rcr
db 0x00, ; ret
db 0x00, ; retf
db 0x00, ; rol
db 0x00, ; ror
db 0x00, ; sahf
db 0x00, ; sal
db 0x00, ; sar
db 0x1C, ; sbb
db 0x00, ; shl
db 0x00, ; shr
db 0x00, ; stc
db 0x00, ; std
db 0x2C, ; sub
db 0xA8, ; test
db 0x00, ; xchg
db 0x00, ; xlatb
db 0x34, ; xor

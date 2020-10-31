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
  
  ; TODO: support "byte" and "word" prefixes
  ; TODO: support DEFAULT_10
  ; TODO: support FAR_JUMP

  ; TODO: first pass over each line: build the symbol table
  ;  - save the line number the label is on (skipping comment only lines)
  ;  - save the current non-dot label somewhere so we can handle local labels
  ;  - have a resolved/unresolved flag we can flip as we go and fill in values
  ;  - just write the data right before the assembled code

  ; Second pass to parse the instructions
  ; TODO: off by one with the line number when returning errors (but not in the tester)
  mov cx, 1 ; keep a line number count for error reporting
assemble_loop:
  cmp byte [ds:si], 0
  je done_assembling
  push cx ; save the line count so we can use cx

  ; Skip beginning of line whitespace
  call skip_spaces
  ; Skip blank lines
  cmp byte [ds:si], 0
  je .next_line_or_done
  cmp byte [ds:si], `\n`
  je .next_line_or_done

  ; Skip comment lines
  cmp byte [ds:si], ';'
  jne .not_comment
  call skip_to_end_of_line
  jmp .next_line_or_done
  .not_comment:

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
  mov byte [es:di], al
  inc di

  call write_mem_reg ; also includes the extra opcode
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

%ifdef TESTER_CALLBACK
  call test_assembled_instruction
%endif

  inc cx ; count the line we just parsed

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
  mov bx, no_code_error_str
  mov cx, no_code_error_len
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
  or byte [bp+.OUT_FLAGS], USE_SHORT_SHIFT|USE_EXTRA_OP
  ; TODO: requires "byte" or "word" if the first arg was mem
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
  ; TODO: requires "byte" or "word" if the first arg was mem
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
  ; TODO: check the symbol table

  cmp word [ds:si], '0x' ; note: because of the \0 we can compare one past the end
  je .hex_literal

  cmp byte [ds:si], "'"
  je .char_literal
  cmp byte [ds:si], '"'
  je .char_literal

  mov bx, not_implemented_error_str
  mov cx, not_implemented_error_len
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

%include "assembler/data.asm"

; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andrew Kallmeyer <ask@ask.systems>
;
; Many thanks to the BSD licensed
; tinylisp <https://github.com/Robert-van-Engelen/tinylisp>
;
; This code was heavily inspired by tinylisp. Mainly the datastructure, a stack
; of typed objects, and the way that the logic is built around using that
; datastructure internally. I have made some important changes though, a
; possibly not exhaustive list:
;
;  - I'm using integers not doubles and I didn't do the NaN trick to pack the
;    type information in, and I decided to just use two words instead of
;    splitting the bits of one word and having smaller integers.
;  - I didn't use the heap side of the memory block at all, I just read the
;    strings out of the code directly instead of copying them. This meant I
;    needed to add new type tags.
;  - I decided to set the bit values of my types differently so my flag checking
;    code is different. I used separate bits and some have two bits if the code
;    for one type should also apply to another.
;  - I simplified a lot of things in parsing. For one thing I didn't use a
;    buffer for the tokens, I just point into the code. Several of the functions
;    in tinylisp are just inlined in my code. etc.

%include "bootloader/bootsect.asm"

; This is the start for calculating NUM_EXTRTA_SECTORS
; There must be nothing other than the bootsector above this label
extra_sectors_start:

%define RUN_CODE
%include "text-editor.asm"

; Run the lisp interpreter!
;
; Input:
;
; [ds:si] - the code to run
; [es:di] - an unused memory block
;
; Output:
;
; [es:di] - The evaluated output buffer (changed from the input)
;
; Global register variables:
;
; [ds:si] - the code input pointer (null terminated)
; [es:di] - The object stack pointer, holds the typed objects. The low end of the es memory block.
;
; The object stack is a list of 2 word objects. The first word is the type and
; the second word is the value.
;
; Segments:
;
; All of the lisp REPL functions required all 4 memory segments to be
; unmodified. There is no more available address space to use more memory
; without using extended pointers in the lisp stack and moving the es segment
; around as needed.
;
; Each segment is limited to 0xFFFF bytes or 64k
;
;  es - The lisp memory, a stack of type tagged objects with internal pointers
;  ds - the input code, we point directly into the code for the symbol strings
;       instead of copying them into the lisp memory
;  cs - The code segment, builtin strings are stored here (and the interpreter)
;  ss - the usual stack, needed for function calls in the interpreter
run_code:
  ; set the output location for print just after the lisp env data
  mov ax, es
  add ax, 0x0100
  mov [cs:.output_seg], ax

  cmp word [cs:env], 0
  jne .keep_env
  call setup_env
  .keep_env:

  .repl:

  call scan

  cmp cx, 0
  ja .not_end
  ; set the output buffer as the returned output [es:di]
  mov es, [cs:.output_seg]
  mov di, [cs:.output_addr]
  mov byte [es:di], 0 ; null terminate the output
  xor di, di
  ret
  .not_end:

  call parse

  push word [cs:env]
  push type.CONS
  call eval
  add sp, 4

  push di
  call print ; print the eval result to the end of the lisp stack
  ; TODO: check for overflow in print

  ; Restore di and calculate chars printed
  mov cx, di
  pop di
  sub cx, di

  push ds ; save ds which we'll use for the output buffer
  push si ; save the code input pointer
  mov ds, [cs:.output_seg]
  mov si, [cs:.output_addr]

  ; Copy cx chars from [es:di] into [ds:si], and don't clobber di
  mov bx, di
  .copy_print_buf:
  test cx, cx
  jz .end_print_buf
  mov al, [es:bx]
  mov [ds:si], al
  inc bx
  inc si
  dec cx
  jmp .copy_print_buf
  .end_print_buf:

  ; Add a new line at the end
  mov byte [ds:si], `\n`
  inc si
  mov [cs:.output_addr], si ; save the end of the output

  ; restore the code input location
  pop si
  pop ds

  jmp .repl

; Use the code segment instead of stack locals like C would do because I didn't
; preserve the bp register like the C calling convention
.output_seg: dw 0
.output_addr: dw 0

objsize: equ 4

objarg1:
.value: equ 4
.type: equ 2

objarg2:
.value: equ 8
.type: equ 6

; Objects on the lisp stack are two words, the value and then the type.
; This is the type enum.
;
; The values are bit flags and some flags apply to multiple types. For example:
; a CLOS counts as a CONS, so car and cdr work on CLOS.
type:
.ATOM:    equ 0x0001 ; String pointer on ds, eg [ds:val]; dh is set to the length.
.CSATOM:  equ 0x0011 ; String pointer on cs, eg [cs:val]; null terminated.
.CONS:    equ 0x0002 ; Pointer on es; eg [es:val-4] is the second elm [es:val-8] is the first elm
.NIL:     equ 0x0004 ; Value is always 0
.INT:     equ 0x0008 ; Value is a 16 bit integer
.PRIM:    equ 0x0020 ; Function pointer on cs; eg call [cs:val]
.CLOS:    equ 0x0042 ; A CONS object that should be run as a function with an env

; The global environment of defined symbols in the lisp interpreter.
; A list of CONS pairs e.g. (("gcd" . {CLOS}) . ((t . t) . nil))
;
; This is just the value part of the object, it's assumed to be type.CONS
;
; Define expressions add symbols to the global environment and local
; environments created by extending env with bound variables are used for
; closures.
env: dw 0

; Builtin atoms, I just fill the lisp object values every time I use them
; They have type.CSATOM and the value is the pointer to the string
atoms:
.t: db 't', 0
.err: db 'ERROR', 0
.lookup_err: db 'no-matching-symbol-error', 0
.not_fn_err: db 'not-a-function-error', 0
.quote: equ atom_quote

; Data array of null terminated function names then the function pointer.
; The array itself is then null terminated.
primitives:
;db 'cons', 0
;dw prim_cons
db 'car', 0
dw prim_car
db 'cdr', 0
dw prim_cdr
; Note: the CSATOM label for quote must be the exact same string as in the
; primitives so we can avoid writing another string equals function. This is
; because when parsing 'test inserts (quote . (test . nil)) using quote_atom.
atom_quote: db 'quote',0
dw car ; this primitive is the only one that doesn't need eval or anything!
db 0

; TODO should there be an error if there's more than one argument?
prim_car:
  mov bp, sp
  call car ; get the first argument
  push word [bp+objarg1.value]
  push word [bp+objarg1.type]
  call eval
  add sp, objsize
  jmp car ; run car on the argument

prim_cdr:
  mov bp, sp
  call car ; get the first argument
  push word [bp+objarg1.value]
  push word [bp+objarg1.type]
  call eval
  add sp, objsize
  jmp cdr

; Set the initial state of the lisp global environment
;  - Adds the true symbol (so it doesn't need to be quoted)
;  - Adds all the primitive function handlers
;
; Saves in the global env pointer to a type.CONS
setup_env:
  ; Start the global environment with ((t . t) . nil)
  push word 0
  push word type.NIL
  push word atoms.t
  push word type.CSATOM
  mov ax, atoms.t
  mov dx, type.CSATOM
  call add_env
  add sp, 2*objsize ; clear the stack

  ; Add each primitive function to the env
  push si
  mov si, primitives
.prim_loop:
  cmp byte [cs:si], 0
  jz .end_prim

  ; env arg
  push ax
  push dx
  ; the name arg
  mov ax, si
  mov dx, type.CSATOM
  ; Move si forward to the end of the string
.endstr_loop:
  cmp byte [cs:si], 0
  jz .endstr
  inc si
  jmp .endstr_loop
.endstr:
  ; value arg, the actual primitive function pointer
  push word [cs:si+1]
  push type.PRIM
  call add_env
  add sp, 2*objsize ; clear the stack
  add si, 3 ; null terminator plus the 2 byte function pointer
  jmp .prim_loop
.end_prim:
  pop si

  mov word [cs:env], ax ; save the new head of the env list
  ret

; Creates a pair of elements, the fundamental data type of lisp.
;
; The notation for a cons pair is (a . b) and it is used to create linked lists
; that look like: (a . (b . (c. ()))) for the list (a b c).
;
; Internally the elements are any of the typed objects. A CONS type is just the
; two object elements pushed onto a lisp stack and then a returned pointer to
; those two elements wrapped in an object with type CONS.
;
; The new CONS object is not itself pushed onto the lisp stack.
;
; Input:
;   ax,dx      - first object element
;   push, push - second object element (push ax, then push dx)
;
; Output:
;   ax,dx - the new type.CONS object pointing to the pair
cons:
  mov bp, sp
; An internal version used by higher-order functions to forward their stack to
; cons instead of pushing it again
;
; Input:
;   ax,dx          - first object element
;   [bp+4], [bp+2] - second object element
_cons:
  ; Push the two values of the CONS pair
  mov word [es:di+0], ax
  mov word [es:di+2], dx
  mov ax, [bp+objarg1.value]
  mov dx, [bp+objarg1.type]
  mov word [es:di+4], ax
  mov word [es:di+6], dx
  add di, 8

  mov ax, di
  mov dx, type.CONS
  ret

; The same as cons but takes the arguments in the opposite order. That way you
; don't have to do a complicated swap sequence to call it.
;
; Input:
;   push  - first element object value
;   push  - first element object type
;   ax,dx - second object element
reverse_cons:
  mov bp, sp
  ; Push the two values of the CONS pair
  mov word [es:di+4], ax
  mov word [es:di+6], dx
  mov ax, [bp+objarg1.value]
  mov dx, [bp+objarg1.type]
  mov word [es:di+0], ax
  mov word [es:di+2], dx
  add di, 8

  mov ax, di
  mov dx, type.CONS
  ret

_err:
  mov ax, atoms.err
  mov dx, type.CSATOM
  ret

; Return the first element of a linked list or cons pair
; Returns an error if the type is not one of those.
;
; Must not clobber cx for some callers
;
; Input: ax, dx - the cons or linked list (which is also a cons)
; Output: ax, dx - the lisp object that was the first element
car:
  test dx, type.CONS
  jz _err

  mov bx, ax
  mov ax, [es:bx-8]
  mov dx, [es:bx-6]
  ret

; Return the second element cons, which is the tail of a linked list after
; cutting off the first element.
; Returns an error if the type is not cons or a linked list.
;
; Input: ax, dx - the cons or linked list (which is also a cons)
; Output: ax, dx - the lisp object that was the second element
cdr:
  test dx, type.CONS
  jz _err

  mov bx, ax
  mov ax, [es:bx-4]
  mov dx, [es:bx-2]
  ret

; Input:
;   push, push - left hand side
;   ax, dx - right hand side
; Output: bx non-zero if true, zero if false
;
; Registers:
;  - does not clobber cx, or bp
obj_equal:
  mov bx, sp
  cmp [ss:bx+objarg1.value], ax
  jne .not_equal
  cmp [ss:bx+objarg1.type], dx
  jne .not_equal
  mov bx, 1
  ret
  .not_equal:
  xor bx, bx
  ret

; TODO: the environment will contain ATOM and CSATOM when we support define,
; so we need this function to type check and maybe swap the args or have two
; versions of the atom_equal to jump to
;
; I think it wil be CSATOM vs ATOM and ATOM vs ATOM only.
; Hopefully no ATOM vs CSATOM. This is only used by lookup_env.
;
; If it's ATOM vs ATOM, still no obj_equal. Have to do string compare. Since it
; might be the same string from two different parts of the code.
atom_equal:
  mov bp, sp
  mov bx, [bp+objarg1.type] ; memory arg type

  ; Check the basic ATOM bit first, which CSATOM fills
  test dx, type.ATOM
  jz .not_atom
  test bx, type.ATOM
  jz .not_atom

  cmp dx, type.CSATOM
  je .reg_csatom
  ; dx is an ATOM, not a CSATOM
  ret
  ; TODO: this happens when there's user-defined symbols
  ;test bx, type.CSATOM
  ;jnz _atom_csatom_equal ; TODO Should I just swap the args?
  ;jmp _atom_atom_equal

  .reg_csatom:
  cmp bx, type.CSATOM
  je obj_equal ; both CSATOMs, they always have the same pointer for the string
  jmp _csatom_atom_equal

  .not_atom:
  ; TODO return error
  ret


; Does a string match to compare a CSATOM and a ATOM.
; Assumes the types were already checked.
;
; Input:
;   push, push - type.ATOM
;   ax, dx - type.CSATOM
; Output: bx non-zero if true, zero if false
;
; Registers:
;  - does not clobber cx (needed by lookup_env)
;  - clobbers ax, dx
_csatom_atom_equal:
  mov bp, sp
  mov bx, [bp+objarg1.value] ; the ATOM pointer
  mov dx, [bp+objarg1.type] ; the ATOM length, in dh (and type in dl)
  mov bp, ax ; the CSATOM pointer

  .loop:
  test dh, dh
  jz .end_of_atom
  cmp byte [cs:bp], 0
  je .not_equal ; since it was not the end of the ATOM

  mov byte ah, [cs:bp] ; CSATOM char
  mov byte al, [ds:bx] ; ATOM char
  cmp ah, al
  jne .not_equal

  inc bx
  inc bp
  dec dh
  jmp .loop

  .end_of_atom:
  cmp byte [cs:bp], 0
  je .equal
  ;fallthrough
  .not_equal:
  xor bx, bx
  ret

  .equal:
  mov bx, 1
  ret

; Adds a defined pair to an environment, which is a list of pairs.
; Effectively: takes 3 objects and returns a cons object of ((a.b).c)
;
; For the environment the 3 objects are ((name . value) . env)
;
; Input:
;   push, push - The env object (push ax then dx)
;   push, push - The value object (push ax then dx, after the env obj)
;   ax,dx      - The name object
;
; Output: ax, dx - a CONS object for the new env
add_env:
  mov bp, sp
  call _cons
  add bp, objsize
  jmp _cons ; takes over the stack frame

; Perform a key-value lookup in the env
;
; Input:
;   push, push - the env to lookup in
;   ax, dx - the key to lookup
;
; Output: ax, dx - the looked-up value from the key
lookup_env:
  ; Unfortunately we have to swap the input args.
  ; If we didn't swap here we'd have to swap in both places it is called. I want
  ; to keep the env on the stack as an implicit arg.
  mov bp, sp
  push ax
  push dx
  mov ax, [bp+objarg1.value]
  mov dx, [bp+objarg1.type]

  ; Search the env, linked list of pairs
  .loop:
  test dx, type.CONS
  jz .not_found

  mov cx, ax ; save the env pointer (car and obj_equal don't use cx)

  call car ; first pair in env
  call car ; first key in env
  call atom_equal ; key == search_key

  ; restore the env object
  mov ax, cx
  mov dx, type.CONS ; type was clobbered by car(car(env)) we tested it was cons

  test bx, bx
  jnz .found_match ; end if the key matched

  ; key didn't match, try the next pair in the env
  call cdr ; env = cdr(env)

  jmp .loop

  .found_match:
  call car ; the matching pair
  call cdr ; the value of the pair
  add sp, objsize ; remove the key from the stack
  ret

  .not_found:
  ; The lookup key is still on the stack from the swap at the beginning
  mov ax, atoms.lookup_err
  mov dx, type.CSATOM
  call cons ; return (lookup_err . key)
  add sp, objsize ; remove the key from the stack
  ret

; Advances the code input and reads a lisp token which is:
;  - (
;  - )
;  - '
;  - a symbol string
;
; Skips all whitespace, which is not part of the token.
; Length of the token may be 0 if it was the end of the code.
;
; Input:
;   [ds:si] - the code input
;
; Output:
;   [ds:bx] - the token string
;   cx      - length of the token
scan:
  xor cx, cx

  .skip_spaces:
  cmp byte [ds:si], ' '
  ja .past_spaces ; all ascii chars less than ' ' are whitespace
  cmp byte [ds:si], 0
  je .at_end
  inc si
  jmp .skip_spaces

  .past_spaces:
  mov bx, si

  cmp byte [ds:si], '('
  je .special_char
  cmp byte [ds:si], ')'
  je .special_char
  cmp byte [ds:si], "'"
  je .special_char

  .symbol_loop:
  inc cx
  inc si
  cmp byte [ds:si], '('
  je .symbol_done
  cmp byte [ds:si], ')'
  je .symbol_done
  cmp byte [ds:si], ' '
  jbe .symbol_done
  jmp .symbol_loop
  .symbol_done:
  ret

  .special_char:
  mov cx, 1
  inc si
  ret

  .at_end:
  mov bx, si
  ret

; Recursively parse an entire cons pair or a linked list.
; Assumes that the starting ( was already scanned, and immediately skips it.
;
; Input:
;   [ds:bx] - The scanned ( token
;   cx      - 1, the length of the scanned ( token
;
; Output:
;   ax, dx - The object pointing to the parsed list or cons
_parse_list:
  call scan ; read the element
  cmp byte [ds:bx], ')'
  jne .not_nil
  mov ax, 0
  mov dx, type.NIL
  ret
  .not_nil:

  cmp byte [ds:bx], '.'
  jne .not_cons
  cmp cx, 1
  jne .not_cons
  call scan ; skip the .
  call parse ; parse the second element
  ; skip the ) ; TODO: error if it's not a )
  jmp scan ; return the parse result which scan doesn't mess up
  .not_cons:

  ; Parse an element of the list
  call parse
  push ax
  push dx
  call _parse_list ; Parse the rest of the list
  call reverse_cons ; return (first_element . rest_of_the_list)
  add sp,  4
  ret

; Parses a single lisp expression from the input code. Returns the lisp object
; for the top-level of the parse tree. E.g. for (a b c) it returns the CONS
; object that is the start of the linked list.
;
; Expects that scan was already called to consume the first token. It sometimes
; recursively calls itself with or without scan. It parses a full expression,
; scanning when needed internally, not just the first token.
;
; Input:
;   [ds:bx] - The first token to parse (result from scan)
;   cx      - The length of the first token (result from scan)
;
; Output:
;   ax, dx - The object pointing to the parsed lisp expression (not always a cons)
parse:
  cmp byte [ds:bx], ')'
  jne .not_end_error
  ret ; TODO: error
  .not_end_error:

  cmp byte [ds:bx], '('
  jne .not_list
  jmp _parse_list ; takes over our stack frame
  .not_list:

  cmp byte [ds:bx], "'"
  jne .not_quote
  ; Return (quote . (parse() . nil)) i.e. (quote parse())
  ; Inner pair (parse() . nil)
  call scan ; skip the '
  call parse
  push word 0
  push word type.NIL
  call cons
  add sp, objsize
  ; Outer pair (quote . above-result)
  push ax
  push dx
  mov ax, atoms.quote
  mov dx, type.CSATOM
  call cons
  add sp, objsize
  ret
  .not_quote:

  cmp byte [ds:bx], '0'
  jb .not_int
  cmp byte [ds:bx], '9'
  ja .not_int
  ret ; TODO
  .not_int:

  cmp cx, 0xFF
  jbe .not_range_error
  ret ; TODO error symbol too long
  .not_range_error:

  mov ax, bx
  mov dh, cl
  mov dl, type.ATOM
  ret

; Run a function on arguments, it can be a closure or a primitive
;
; Input:
;   ax, dx - the function to apply
;   push, push - the arguments to apply the function to
;   push, push - the env to evaluate in
; Output: ax, dx - the result of the function call
apply:
  mov bp, sp

  cmp dx, type.PRIM
  jne .not_primitive

  mov bx, ax ; the primitive function pointer
  ; the arguments are the first arg to the primitive
  mov ax, [bp+objarg2.value]
  mov dx, [bp+objarg2.type]
  ; the env is the second arg and already on the stack
  jmp bx ; call the prim function ; call ret

  .not_primitive:

  cmp dx, type.CLOS
  jne .not_closure

  ret ; todo

  ; todo save and reset ax,dx
  call cdr
  cmp dx, type.NIL
  je .use_global_env
  ; todo: use ax, dx for the env in the bind() call
  jmp .env_set
  .use_global_env:

  .env_set:

  ; argument names?
  call car
  call car
  ; todo save

  ; Evaluate each of the argument values
  mov ax, [bp+objarg2.value]
  mov dx, [bp+objarg2.type]
  ; TODO forward env arg
  ;call eval_each

  ; closure body
  call car
  call cdr

  call eval ; evaluate the bound closure

  .not_closure:

  mov ax, atoms.not_fn_err
  mov dx, type.CSATOM
  ret

;
;
; Input:
;   push,push - the env object
;   ax,dx     - the object to eval
;
; Output:
;   ax,dx - the evaluated result object
eval:
  test dx, type.ATOM
  jnz .atom
  test dx, type.CONS
  jnz .cons
  ret ; return the input for all other types

.atom:
  jmp lookup_env ; call ret

.cons: ; all cons are treated as function calls
  ; So we can use the two local variables.
  ; We only need it before we call eval again so no need to save it
  mov bp, sp

  ; Save the original input object
  .orig: equ 4
  push ax
  push dx

  ; Extract the arguments of function call
  call cdr
  .args: equ 8 ; unused, just to remember what it is
  push ax
  push dx

  ; Extract and evaluate the function name
  mov ax, [bp-.orig+2]
  mov dx, [bp-.orig]
  call car
  ; Copy the env arg for eval, we have other vars on the stack above it
  push word [bp+objarg1.value]
  push word [bp+objarg1.type]
  call eval ; evaluate the function name to get the definition (bp is clobbered now)
  call apply ; run the function on the arguments
  add sp, 3*objsize ; remove the locals we pushed
  ret

; Print a 16 bit integer in hex to the output buffer
;
; Input:
;   ax      - the word to print
;   [es:di] - the output buffer
_write_hex_word:
  mov cl, ah
  shr cl, 4
  call .write_hex_char
  mov cl, ah
  and cl, 0x0F
  call .write_hex_char
  mov cl, al
  shr cl, 4
  call .write_hex_char
  mov cl, al
  and cl, 0x0F
  ; fallthrough

; arg is cl which must be less than 0x10
.write_hex_char:
  cmp cl, 9
  ja .over_9

  add cl, '0'
  mov byte [es:di], cl
  inc di
  ret

  .over_9:
  sub cl, 10
  add cl, 'A'
  mov byte [es:di], cl
  inc di
  ret

; Prints a lisp list or cons object (ax, dx as always)
_print_list:
  mov byte [es:di], '('
  inc di

  .loop:
  ; Print the first element of the list
  push ax
  push dx
  call car
  call print
  pop dx
  pop ax

  ; Handle the remaining elements
  call cdr
  test dx, type.NIL
  jnz .done

  test dx, type.CONS
  jnz .next_element

  mov word [es:di], " ."
  mov byte [es:di+2], ' '
  add di, 3
  call print
  jmp .done

  .next_element:
  mov byte [es:di], ' '
  inc di
  jmp .loop

  .done:
  mov byte [es:di], ')'
  inc di
  ret

; Writes a string version of a lisp object
;
; This still requires all 4 segment registers to be unmodified. So the output
; buffer must be at the end of the lisp memory. Print needs to be able to find
; the strings and walk the list pointers which are spread among the segments.
;
; Input:
;   ax, dx  - the lisp object to print
;   [es:di] - the output string location
;
; Output:
;   [es:di] - one past the end of the string printed
print:
  cmp dx, type.NIL
  jne .not_nil
  mov word [es:di], "()"
  add di, 2
  ret
  .not_nil:

  cmp dx, type.CSATOM
  jne .not_csatom
  mov bx, ax
  .copy_cs_str:
  mov al, [cs:bx]
  cmp al, 0
  je .end_cs_str
  mov [es:di], al
  inc di
  inc bx
  jmp .copy_cs_str
  .end_cs_str:
  ret
  .not_csatom:

  test dx, type.ATOM
  jz .not_atom
  mov bx, ax
  .copy_str_len:
  test dh, dh
  jz .end_str_len
  mov al, [ds:bx]
  mov [es:di], al
  inc di
  inc bx
  dec dh
  jmp .copy_str_len
  .end_str_len:
  ret
  .not_atom:

  cmp dx, type.PRIM
  jne .not_prim
  mov byte [es:di], '{'
  inc di
  call _write_hex_word
  mov byte [es:di], '}'
  inc di
  ret
  .not_prim:

  cmp dx, type.CONS
  jne .not_cons
  jmp _print_list ; call ret
  .not_cons:

  ret

NUM_EXTRA_SECTORS: equ NUM_SECTORS(extra_sectors_start)

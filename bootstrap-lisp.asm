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
;    buffer for the tokens, I just point into the code. Several of the functons
;    in tinylisp are just inlined in my code. etc.

%include "bootloader/bootsect.asm"

; This is the start for calculating NUM_EXTRTA_SECTORS
; There must be nothing other than the bootsector above this label
extra_sectors_start:

%define RUN_CODE
%include "text-editor.asm"

; Segments
;  es - the lisp memory
;  ds - the input code

run_code:
  ; set the output location for print just after the lisp env data
  mov ax, es
  add ax, 0x0100
  mov [cs:.output_seg], ax

  call setup_lisp

  .repl:

  call scan

  cmp cx, 0
  ja .not_end
  ; set the output buffer as the returned output [es:di]
  mov es, [cs:.output_seg]
  mov di, [cs:.output_addr]
  mov byte [es:di], 0 ; null termate the output
  xor di, di
  ret
  .not_end:

  call parse
  call eval

  push di
  call print ; print the eval result to the end of the lisp stack
  ; TODO: check for overflow in print

  ; Restore di and calculate chars printed
  mov cx, di
  pop di
  sub cx, di

  push ds ; save ds which we'll use for the output buffer
  push si ; save the code input ptr
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

.output_seg: dw 0
.output_addr: dw 0

; Global variables:
;
; [ds:si] - the code input pointer (null terminated)
; [es:di] - The object stack pointer, holds the typed objects. The low end of the es memory block.
; TODO is this needed? [es:bx] - The heap pointer, holds the symbol strings. The high end of the es memory block.
;
; - The heap is raw data, mostly just strings that the objects may point into.
; - The object stack is a list of 2 word objects. The first word is the type and
;   the second word is the value.

type:
.ATOM:    equ 0x0001 ; Pointer on ds, eg [ds:val]; dh is set to the length of the string.
.CSATOM:  equ 0x0011 ; Pointer on cs, eg [cs:val]. Also has the ATOM bit.
.CONS:    equ 0x0002 ; Pointer on es; eg [es:val-4] is the second elm [es:val-8] is the first elm
.NIL:     equ 0x0004
.INT:     equ 0x0008
.PRIM:    equ 0x0020 ; Pinter on cs; eg call [cs:val]
.CLOS:    equ 0x0042 ; A CONS object that should be run as a function with an env

; The global environment of defined symbols in the lisp interpreter.
; A list of CONS pairs e.g. (("gcd" . {CLOS}) . ((t . t) . nil))
;
; This is just the value part of the object, it's assumed to be type.CONS
env: dw 0

t_str: db 't', 0
quote_str: db 'quote', 0
err_str: db 'ERROR', 0

primitives:
db 'cons', 0
dw prim_cons
db 'car', 0
dw prim_car
db 'cdr', 0
dw prim_cdr
db 0

setup_lisp:
  ; Start the global environment with ((t . t) . nil)
  ; TODO: Why is (t . t) in the env?
  push word 0
  push word type.NIL
  push word t_str
  push word type.CSATOM
  mov ax, t_str
  mov dx, type.CSATOM
  call add_env
  add sp, 8 ; clear the stack

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
  add sp, 8 ; clear the stack
  add si, 3
  jmp .prim_loop
.end_prim:
  pop si

  mov word [cs:env], ax ; save the new head of the env list
  ret

cons:
  mov bp, sp
; input:
;   ax,dx          - first object element
;   [bp+2], [bp+4] - second object element
_cons:
  ; Push the two values of the CONS pair
  mov word [es:di+0], ax
  mov word [es:di+2], dx
  mov dx, [bp+2]
  mov ax, [bp+4]
  mov word [es:di+4], ax
  mov word [es:di+6], dx
  add di, 8

  mov ax, di
  mov dx, type.CONS
  ret

; input:
;   push  - first element object value
;   push  - first element object type
;   ax,dx - second object element
reverse_cons:
  mov bp, sp
  ; Push the two values of the CONS pair
  mov word [es:di+4], ax
  mov word [es:di+6], dx
  mov dx, [bp+2]
  mov ax, [bp+4]
  mov word [es:di+0], ax
  mov word [es:di+2], dx
  add di, 8

  mov ax, di
  mov dx, type.CONS
  ret

_err:
  mov ax, err_str
  mov dx, type.CSATOM
  ret

car:
  test dx, type.CONS
  jz _err

  mov bx, ax ; TODO: save bx if we need the heap
  mov ax, [es:bx-8]
  mov dx, [es:bx-6]
  ret

cdr:
  test dx, type.CONS
  jz _err

  mov bx, ax ; TODO: save bx if we need the heap
  mov ax, [es:bx-4]
  mov dx, [es:bx-2]
  ret

; Adds a defined pair to an environment, which is a list of pairs.
; Effectively: takes 3 objects and returns a cons object of ((a.b).c)
;
; For the environment the 3 objects are ((name . value) . env)
;
; Input:
;   push, push - The env object (push ax then dx)
;   push, push - The value object (push ax then dx)
;   ax,dx      - The name object
;
; Returns: a CONS object for the new env in ax,dx
add_env:
  mov bp, sp
  call _cons
  add bp, 4
  jmp _cons ; takes over the stack frame

prim_cons:
  ret

prim_car:
  ret

prim_cdr:
  ret

; advances the input ds:si and reads a lisp token which is:
;  - (
;  - )
;  - '
;  - a symbol string
; returns a token in ds:bx, cx is the length
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

parse_list:
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
  call parse_list ; Parse the rest of the list
  call reverse_cons ; return (first_element . rest_of_the_list)
  add sp,  4
  ret

; TODO: document
parse:
  cmp byte [ds:bx], ')'
  jne .not_end_error
  ret ; TODO: error
  .not_end_error:

  cmp byte [ds:bx], '('
  jne .not_list
  jmp parse_list ; takes over our stack frame
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
  add sp, 4
  ; Outer pair (quote . above-result)
  push ax
  push dx
  mov ax, quote_str
  mov dx, type.CSATOM
  call cons ; takes over the stack frame
  add sp, 4
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
  ret ; TODO error sybol too long
  .not_range_error:

  mov ax, bx
  mov dx, cx
  shl dx, 8
  or dx, type.ATOM
  ret

; arg is cl which must be less than 0x10
_write_hex_char:
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

_write_hex_word:
  mov cl, ah
  shr cl, 4
  call _write_hex_char
  mov cl, ah
  and cl, 0x0F
  call _write_hex_char
  mov cl, al
  shr cl, 4
  call _write_hex_char
  mov cl, al
  and cl, 0x0F
  call _write_hex_char
  ret

print_list:
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

  ; Handle th remaining elements
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

; Writes to [es:di] and increments di as it goes
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
  jmp print_list
  .not_cons:

  ret

  ; TODO: env arg on stack?
  ; TODO: why is there an env arg?
eval:
  ret ; TODO

  test dx, type.ATOM
  jnz .atom
  test dx, type.CONS
  jnz .cons

.atom:
  ;call assoc
  ret

.cons:
  mov bp, sp

  .orig: equ 4
  push ax
  push dx

  call cdr
  .args: equ 8
  push ax
  push dx

  mov ax, [bp-.orig]
  mov dx, [bp-.orig+2]
  call car
  ; TODO: env arg
  call eval

  ; TODO: .args arg, maybe just on the stack is fine?
  ;call apply

  add sp, 16 ; remove the locals we pushed

  ret

NUM_EXTRA_SECTORS: equ NUM_SECTORS(extra_sectors_start)


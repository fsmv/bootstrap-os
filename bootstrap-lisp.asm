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

%include "bootloader/bootsect.asm"

; This is the start for calculating NUM_EXTRTA_SECTORS
; There must be nothing other than the bootsector above this label
extra_sectors_start:

%define RUN_CODE
%include "text-editor.asm"

run_code:
  call setup_lisp
  call parse
  call eval

  ; set the output location for print just after the lisp env data
  ; use [ds:di] because we no longer need the input code or the end of the
  ; stack. We just need es to stay the same so list pointers work!
  mov cx, es
  shr di, 4
  add cx, di
  inc cx
  mov ds, cx
  xor di, di

  call print
  mov byte [ds:di], 0 ; null terminate the output

  ; now that we don't need the lisp state anymore set the output location
  ; in [es:di]
  mov cx, ds
  mov es, cx
  xor di, di
  ret

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
.ATOM:    equ 0x0001 ; Pointer on ds, eg [ds:val]
.CSATOM:  equ 0x0011 ; Pointer on cs, eg [cs:val]. Also has the ATOM bit.
.CONS:    equ 0x0002
.NIL:     equ 0x0004
.INT:     equ 0x0008
.PRIM:    equ 0x0020
.CLOS:    equ 0x0042

; The global environment of defined symbols in the lisp interpreter.
; A list of CONS pairs e.g. (("gcd" . {CLOS}) . ((t . t) . nil))
;
; This is just the value part of the object, it's assumed to be type.CONS
env: dw 0

t_str: db 't', 0
err_str: db 'ERROR', 0

nil: equ 0
t: equ 4
err: equ 8

primitives:
db 'cons', 0
dw prim_cons
db 'car', 0
dw prim_car
db 'cdr', 0
dw prim_cdr
db 0

setup_lisp:
  ; Push the builtin objects
  mov word [es:di+0], 0
  mov word [es:di+2], type.NIL
  mov word [es:di+4], t_str
  mov word [es:di+6], type.CSATOM
  mov word [es:di+8], err_str
  mov word [es:di+10], type.CSATOM
  add di, 12

  ; Start the global environment with ((t . t) . nil)
  ; TODO: do I really need to push t as a builtin atom? This will push two
  ; copies.
  ; TODO: Why is (t . t) in the env?
  push word [es:nil+0]
  push word [es:nil+2]
  push word [es:t+0]
  push word [es:t+2]
  mov ax, [es:t+0]
  mov dx, [es:t+2]
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

; input:
;   ax,dx          - first object element
;   [bp+2], [bp+4] - second object element
cons:
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

_err:
  mov ax, [es:err]
  mov dx, [es:err+2]
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
  call cons
  add bp, 4
  call cons
  ret

prim_cons:
  ret

prim_car:
  ret

prim_cdr:
  ret

parse:
  ret

; arg is cl which must be less than 0x10
_write_hex_char:
  cmp cl, 9
  ja .over_9

  add cl, '0'
  mov byte [ds:di], cl
  inc di
  ret

  .over_9:
  sub cl, 10
  add cl, 'A'
  mov byte [ds:di], cl
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
  mov byte [ds:di], '('
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

  mov word [ds:di], " ."
  mov byte [ds:di+2], ' '
  add di, 3
  call print
  jmp .done

  .next_element:
  mov byte [ds:di], ' '
  inc di
  jmp .loop

  .done:
  mov byte [ds:di], ')'
  inc di
  ret

print:
  test dx, type.NIL
  jz .not_nil
  mov word [ds:di], "()"
  add di, 2
  ret
  .not_nil:

  test dx, type.CSATOM
  jz .not_csatom
  mov bx, ax
  .copy_cs_str:
  mov al, [cs:bx]
  cmp al, 0
  je .end_cs_str
  mov byte [ds:di], al
  inc di
  inc bx
  jmp .copy_cs_str
  .end_cs_str:
  ret
  .not_csatom:

  test dx, type.PRIM
  jz .not_prim
  mov byte [ds:di], '{'
  inc di
  call _write_hex_word
  mov byte [ds:di], '}'
  inc di
  ret
  .not_prim:

  test dx, type.CONS
  jz .not_cons
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

  ret

NUM_EXTRA_SECTORS: equ NUM_SECTORS(extra_sectors_start)


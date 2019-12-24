; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andrew Kallmeyer <ask@ask.systems>
; Jump past the BIOS Parameter block, which is needed for USB boots
; Apparently some BIOS check for this, so I think we can't set cs here
jmp start
nop
times 8 db 0 ; OEM Name. I've seen 'MSDOS5.0' here, not sure if that's needed
; BIOS parameter block, would need data if we were on a floppy
; In USB boots the BIOS fills in fake floppy information
times 0x33 db 0 
start:

; Set the code segment to 0x7C0, optional if there's no absolute jumps
; The bootsector code is always loaded at 0x7C00 by the BIOS
jmp 0x7C0:start2
start2:

; Set up the stack
mov ax, 0x7E0  ; It is not physically possible to set ss directly (on the 8086)
mov ss, ax     ; Stack top at 0x7E00, right after the boot code
mov sp, 0x2000 ; 8K stack, push subtracts from this (torwards ss). 0x9E00 start.
mov bp, sp

; Set the data segment (and es) start at 0x7C00 so you can write your assembly at org 0
mov ax, 0x7C0
mov ds, ax
mov es, ax

cld ; Set the direction flag to forward (for string instructions)

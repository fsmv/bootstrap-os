; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andrew Kallmeyer <ask@ask.systems>

; All the voodoo required to boot on modern hardware. Mostly it is spaces and
; magic numbers for compatibilty with various extensions to BIOS that became
; de-facto standard over time.
;
; A lot of this came from https://wiki.osdev.org/Problems_Booting_From_USB_Flash

%define CODE_SEGMENT 0x7C0 ; This is where the BIOS always loads the code

; Jump past the BIOS Parameter block, which is needed for USB boots
;
; Apparently some BIOS implementations check for this instruction, so I think
; we can't just set the cs register here right away
jmp start
nop ; Take up the remaning byte before the BIOS parameter block

; The BIOS parameter block. This stores information about the floppy disk the
; code is on. In USB boots like we're set up for, the BIOS just overwrites this
; section in memory with the right data.
times 8 db 0 ; OEM Name. I've seen 'MSDOS5.0' here, not sure if that's needed
; Space for the BPB
times 0x33 db 0 
start:

; Set the code segment to 0x7C0, optional if there's no absolute jumps
; The bootsector code is always loaded at 0x7C00 by the BIOS
;
; This has never changed and is documented in the BIOS manual under int 19H
jmp CODE_SEGMENT:start2
start2:

; Set up the stack
mov ax, 0x050 ; It is not physically possible to set ss directly (on the 8086)
mov ss, ax ; Stack top at 0x0500, the start of the 30k block after the BIOS Data Area
mov sp, 0x7BFF-0x500 ; ~30K stack, push subtracts from this (torwards ss). 0x7BFF start (right before the CODE_SEGMENT)
mov bp, sp

; Set the data segment (and es) start at 0x7C00 so you can write your assembly at org 0
mov ax, CODE_SEGMENT
mov ds, ax
mov es, ax

cld ; Set the direction flag to forward (for string instructions)

; Must not overwrite dl which is the boot drive number

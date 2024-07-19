; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andy Kallmeyer <ask@ask.systems>

; All the voodoo required to boot on modern hardware. Mostly it is spaces and
; magic numbers for compatibilty with various extensions to BIOS that became
; de-facto standard over time.
;
; A lot of this came from https://wiki.osdev.org/Problems_Booting_From_USB_Flash

; Set the stack to the ~30k area between the reserved BIOS Data Area and the
; automatically loaded bootsector segment. i.e. between 0x500 and 0x7C00.
;
; The stack pointer is set to the highest available address so you know when the
; stack is out of space when sp is zero.
;
; If you change this you also need to change how sp is set. I expect if you're
; changing this you're not putting the ss between the BDA and the CODE_SEGMENT
; so it's not possible to write code below that automatically sets sp correctly
; if you change this. If you have a whole 64k segment free set sp to 0xFFFF.
%define STACK_SEGMENT 0x050
; This is where the BIOS always loads the code, you can't change that.
;
; In bootsect.asm there's code to load additional sectors from disk immediately
; after this first one in memory that is always loaded.
;
; This bootsect-header.asm file contains code that set the cs register to this
; value so that you can write your code at org 0. So technically you can change
; this constant because this is just what we set the cs register to.
%define CODE_SEGMENT 0x7C0
; The distance between non-overlapping segment in the segment register address
; space (i.e. it's 0x10000 bytes but you only add 0x1000 to the segment register)
%define NEXT_SEGMENT 0x1000
; The last segment in the unmapped memary block after 0x7C0 that can be used
; with the full 16bit address range (0x7FFFF is the last safe address before the
; Extended BIOS Data Area).
;
; If you set a segment register higher than this value you might overflow into
; the reserved memory areas.
;
; See: https://wiki.osdev.org/Memory_Map_(x86)
%define LAST_SEGMENT 0x7000
; The last valid sp value given that we set ss to 0x0050
;
; This puts the stack in the ~30k area of free memory between the BIOS data and
; the start of this code (0x7C00)
%define MAX_STACK_POINTER 0x2BFF

; The size of a hard drive sector in bytes
%define SECTOR_SIZE 0x200 ; 512 in decimal

; Calculate the number of sectors a given span of code takes up from the passed
; in label to the line NUM_SECTORS is applied on. This is useful for calculating
; how much code to load off disk in your bootloader.
;
; The -1 is because we don't want the next sector until we have 512+1 bytes
; e.g. for exactly 512 bytes we want 1 extra sector not 2.
; The +1 is because int division does floor() and we want ceil().
%define NUM_SECTORS(label) (($-label-1)/SECTOR_SIZE + 1)

; Jump past the BIOS Parameter block, which is needed for USB boots
;
; Apparently some BIOS implementations check for this instruction, so I think
; we can't just set the cs register here right away
jmp start
nop ; Take up the remaning byte before the BIOS parameter block

; The BIOS parameter block. This stores information about the floppy disk the
; code is on. In USB boots like we're set up for, the BIOS just overwrites this
; section in memory with the right data.
db '  ASK  ',0 ; OEM Name (8 bytes). I've seen 'MSDOS5.0' here in some bootloaders.
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
mov ax, STACK_SEGMENT ; It is not physically possible to set ss directly (on the 8086)
mov ss, ax ; Stack top at 0x0500, the start of the 30k block after the BIOS Data Area
mov sp, 0x7BFF-0x500 ; ~30K stack, push subtracts from this (torwards ss). 0x7BFF start (right before the CODE_SEGMENT)
mov bp, sp

; Set the data segment (and es) start at 0x7C00 so you can write your assembly at org 0
mov ax, CODE_SEGMENT
mov ds, ax
mov es, ax

cld ; Set the direction flag to forward (for string instructions)

; Must not overwrite dl which is the boot drive number

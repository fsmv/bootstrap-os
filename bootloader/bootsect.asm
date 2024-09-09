; This assembles to a 512 byte BIOS bootloader sector that works on real
; hardware as long as it has an x86 CPU with BIOS support.
;
; Loads NUM_EXTRA_SECTORS of code after the bootsector on disk into memory
; directly after the bootsector code. Also sets the video mode to some nice
; text mode defaults and sets up the stack and other segment registers.
;
; After that it JMPs to _start

; To use this:
;
;  1. Include this bootloader/bootsect.asm file at the top of your code before
;     any other bytes (this file includes the header and footer)
;  2. Put a label at the top right below the include of this file Ex:
;     extra_sectors_start:
;  3. Paste at the bottom after all of your application code is written/included
;
;       NUM_EXTRA_SECTORS: equ NUM_SECTORS(extra_sectors_start)
;
; You can write all of your code with org 0 (the default) since we set cs so
; that the bootloader code and then your code will start at 0.
;
; This will only work up to 64k of code. Once you're beyond that size you will
; need to set the cs register correctly as you go because it doesn't all fit
; within the 16 bit address space if you keep cs fixed. You would have to
; separate your code into 64k sized modules so internal references work and move
; cs as needed to call the different modules, or implement some other kind of
; dynamic loading of chunks of code system.
;
; So NUM_EXTRA_SECTORS can be at most 0x7F. I can't add a %if %error %endif
; check here because nasm requires that to go after NUM_EXTRA_SECTORS is
; defined.

%define BIOS_PRINT_STRING 0x1301
%define BIOS_PRINT_CHAR 0x0E
%define QEMU_DEBUG_PORT 0xE9

%include "bootloader/bootsect-header.asm"

mov [BOOT_DISK], dl ; Save the boot disk number

; Set video mode, 16 color 80x25 chars
;
; The IBM BIOS manual describes a long procedure for determining which video
; modes are supported and all the possible options for supporting both mono and
; color. Sometimes mono isn't supported if the PC supports color modes. So, I'm
; just going to assume all hardware this is used on supports color modes.
mov ax, 0x0003
int 0x10

; Make the 8th bit of the colors bg-intensity instead of blink
mov ax, 0x1003
mov bl, 0
int 0x10

; Make the 4th bit of the colors fg-intensity instead of font selection
; Use block 0 for the font
; Apparently this is the default in VGA so maybe we don't need it
mov ax, 0x1103
mov bl, 0
int 0x10

; Color byte is now: bg-intensity,bg-r,bg-g,bg-b ; fg-intensity,fg-r,fg-g,fg-b

; Set cursor type to an underline
mov ax, 0x0100
mov cx, 0x0607
int 0x10

; Set the keyboard repeat speed
mov ax, 0x0305
;mov bx, 0x0107 ; 500 ms delay before repeat ; 16 characters per second
mov bx, 0x0100 ; 500 ms delay before repeat ; 30 characters per second
int 0x16
; fallthrough

load_extra_sectors:
; int 0x13 shouldn't be called with 0 for the amount to read.
; Can't use preprocessor for this unfortunately because it's a label
mov al, NUM_EXTRA_SECTORS ; number of sectors to read
test al, al
je start_

; Load the code from the extra sectors
; Try to do the full read every time to take advantage of possible BIOS support
; for multi-track reads if NUM_EXTRA_SECTORS > 63
mov ah, 0x02
mov bx, SECTOR_SIZE ; es:bx is address to write to. es = cs, so write directly after the boot sector
mov dl, [BOOT_DISK] ; Drive number
xor dh, dh ; Head number
mov cx, 0x0002 ; Read from Cylinder (track) 0; Sector 2 (1 is the boot sector)
int 0x13

jnc start_ ; if there was no error, jump to the loaded user code
; Otherwise handle errors

push ax ; push the error code

; Print the error message
mov ax, 0x1301 ; Write String, move cursor mode in al
mov bp, disk_error_msg ; String pointer in es:bp (es is at code start from bootsect-header.asm)
mov cx, disk_error_msg_len ; String length
xor dx, dx ; top left
mov bx, 0x004F ; bh = 0 (page number); bl = color (white on red)
int 0x10

pop cx ; pop the error code
call print_hex ; print the error code

mov ax, 0x1301 ; Write String, move cursor mode in al
mov bp, retry_msg ; String pointer in es:bp (es is at code start from bootsect-header.asm)
mov cx, retry_msg_len ; String length
mov dx, 0x0100 ; second line; left edge
mov bx, 0x004F ; bh = 0 (page number); bl = color (white on red)
int 0x10

mov cl, [READ_RETRIES]
call print_hex

cmp byte [READ_RETRIES], 0
je .no_more_tries

; Reset the disk and retry
dec byte [READ_RETRIES]
xor ax, ax
mov dl, [BOOT_DISK] ; Drive number
int 0x13
jmp load_extra_sectors

.no_more_tries:
jmp $ ; stop forever

; Prints the ascii hex character which represents the integer value of al
; Only accepts 0x0 <= al <= 0xF, anything else is garbage output
; e.g. al = 12 prints "C"
; clobbers ax, and bx
print_hex_char:
  mov ah, BIOS_PRINT_CHAR ; Scrolling teletype BIOS routine (used with int 0x10)
  xor bx, bx ; Clear bx. bh = page, bl = color

  cmp al, 9
  ja .over_9

  add al, '0'
%ifdef DEBUGCON
  out QEMU_DEBUG_PORT, al
%endif
  int 0x10
  ret

.over_9:
  sub al, 10
  add al, 'A'
%ifdef DEBUGCON
  out QEMU_DEBUG_PORT, al
%endif
  int 0x10
  ret

; cx = two bytes to write at current cursor
; clobbers ax, and bx
print_hex:
  ; Nibble 0 (most significant)
  mov al, ch
  shr al, 4
  call print_hex_char
  ; Nibble 1
  mov al, ch
  and al, 0x0F
  call print_hex_char
  ; Nibble 2
  mov al, cl
  shr al, 4
  call print_hex_char
  ; Nibble 3
  mov al, cl
  and al, 0x0F
  call print_hex_char

  ret

; cl = byte to write at current cursor
; clobbers ax, and bx
print_hex_byte:
  ; Nibble 1
  mov al, cl
  shr al, 4
  call print_hex_char
  ; Nibble 2
  mov al, cl
  and al, 0x0F
  call print_hex_char

  ret

; === Bootsector data area ===

disk_error_msg: db `Error reading from disk: `
disk_error_msg_len: equ $-disk_error_msg
retry_msg: db `Retry attempts remaining: `
retry_msg_len: equ $-retry_msg

BOOT_DISK: db 0x00 ; value is filled first thing
READ_RETRIES: db 5

%include "bootloader/bootsect-footer.asm"

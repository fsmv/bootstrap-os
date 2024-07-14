; This assembles to a 512 byte BIOS bootloader sector
;
; Loads NUM_EXTRA_SECTORS of code after the bootsector on disk into memory
; directly after the bootsector code. Also set the video mode and some nice text
; mode defaults.
;
; After that it JMPs to _start

; To load more code from disk you need to set NUM_EXTRA_SECTORS,
; put a label at the top of your code/data and paste:
;
; NUM_EXTRA_SECTORS: equ NUM_SECTORS(extra_sectors_start)

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

; Load the code from the extra sectors
mov ah, 0x02
mov al, NUM_EXTRA_SECTORS
mov bx, SECTOR_SIZE ; es:bx is address to write to. es = cs, so write directly after the boot sector
mov cx, 0x0002 ; Cylinder 0; Sector 2 (1 is the boot sector)
mov dl, [BOOT_DISK]
xor dh, dh ; Head 0
int 0x13

; Check for errors
cmp ax, NUM_EXTRA_SECTORS
je start_

push ax ; push the error code

; Print the error message
mov ax, 0x1301 ; Write String, move cursor mode in al
mov bp, error_msg ; String pointer in es:bp (es is at code start from bootsect-header.asm)
mov cx, error_msg_len ; String length
xor dx, dx ; top left
mov bx, 0x004F ; bh = 0 (page number); bl = color (white on red)
int 0x10

pop cx ; pop the error code
call print_hex ; print the error code

jmp $ ; stop forever

; Prints the ascii hex character which represents the integer value of al
; Only accepts 0x0 <= al <= 0xF, anything else is garbage output
; e.g. al = 12 prints "C"
; clobbers ax, and bx
print_hex_char:
  mov ah, 0x0E
  xor bx, bx
  ; fallthrough
; Also assumes ah = 0x0E and bx = 0
_print_hex_char:
  cmp al, 9
  ja .over_9

  add al, '0'
  int 0x10
  ret

.over_9:
  sub al, 10
  add al, 'A'
  int 0x10
  ret

; cx = two bytes to write at current cursor
; clobbers ax, and bx
print_hex:
  mov ah, 0x0E ; Scrolling teletype BIOS routine (used with int 0x10)
  xor bx, bx ; Clear bx. bh = page, bl = color

  ; Nibble 0 (most significant)
  mov al, ch
  shr al, 4
  call _print_hex_char
  ; Nibble 1
  mov al, ch
  and al, 0x0F
  call _print_hex_char
  ; Nibble 2
  mov al, cl
  shr al, 4
  call _print_hex_char
  ; Nibble 3
  mov al, cl
  and al, 0x0F
  call _print_hex_char

  ret

; cl = byte to write at current cursor
; clobbers ax, and bx
print_hex_byte:
  mov ah, 0x0E ; Scrolling teletype BIOS routine (used with int 0x10)
  xor bx, bx ; Clear bx. bh = page, bl = color

  ; Nibble 1
  mov al, cl
  shr al, 4
  call _print_hex_char
  ; Nibble 2
  mov al, cl
  and al, 0x0F
  call _print_hex_char

  ret

; === Bootsector data area ===

error_msg: db `Error reading additional sectors from disk: `
error_msg_len: equ $-error_msg

BOOT_DISK: db 0x00 ; value is filled first thing

%include "bootloader/bootsect-footer.asm"

; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andrew Kallmeyer <ask@ask.systems>
times 0x200-4-2-0x40-2-($-$$) db 0 ; Pad with zeros up to 512 bytes
dd 0xDEADC0DE                      ; Windows reserved unique drive number
dw 0x0000                          ; Just extra padding
times 0x40 db 0                    ; Empty partition table
dw 0xAA55                          ; BIOS bootable drive magic number

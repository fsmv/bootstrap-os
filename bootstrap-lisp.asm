; Provided under the MIT License: http://mit-license.org/
; Copyright (c) 2020 Andrew Kallmeyer <ask@ask.systems>

%include "bootloader/bootsect.asm"

; This is the start for calculating NUM_EXTRTA_SECTORS
; There must be nothing other than the bootsector above this label
extra_sectors_start:

%define RUN_CODE
%include "text-editor.asm"

run_code:
  mov byte [es:di+0], "L"
  mov byte [es:di+1], "I"
  mov byte [es:di+2], "S"
  mov byte [es:di+3], "P"
  mov byte [es:di+4], "!"
  mov byte [es:di+5], 0
  ret

NUM_EXTRA_SECTORS: equ NUM_SECTORS(extra_sectors_start)


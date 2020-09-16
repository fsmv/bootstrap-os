%ifdef DVORAK

; Used to type with the dvorak layout (see convert_keyboard_layout)
;
; Instructions to make a new map are at the %include location
keyboard_map:
db ` !_#$%&-()*}w[vz0123456789SsW]VZ@AXJE>UIDCHTNMBRL"POYGK<QF:/\\=^{\`axje.uidchtnmbrl'poygk,qf;?|+~`

; Switch on the call to convert_keyboard_layout
%define CONVERT_LAYOUT

%endif

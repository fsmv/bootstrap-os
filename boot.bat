@echo off
set file=%~1
set name=%~n1
set binfile=bin\%name%.bin

mkdir bin 2> NUL
nasm %file% -f bin -o %binfile% && qemu-system-x86_64 -drive file=%binfile%,format=raw,if=ide
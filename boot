#!/bin/bash

if [ "$#" -eq 0 ]; then
    printf "Usage: $0 [filename] [any other nasm args].\nEx: $0 bootstrap-asm.asm -DDVORAK\n"
    exit 1
fi

file=$1
name="${file%%.*}"
binfile="bin/$name.bin"

mkdir -p `dirname $binfile`
nasm $file -f bin -o $binfile "${@:2}" && qemu-system-x86_64 -drive file=$binfile,format=raw,if=ide

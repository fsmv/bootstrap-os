#!/bin/bash

file=$1
name="${file%%.*}"
binfile="bin/$name.bin"

mkdir -p ./bin
nasm $file -f bin -o $binfile && qemu-system-x86_64 -drive file=$binfile,format=raw,if=ide

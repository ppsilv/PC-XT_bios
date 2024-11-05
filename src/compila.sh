#!/bin/bash

nasm -O9 -f bin -o pcxt_bios.bin -l pcxt_bios.lst pcxt_bios.asm

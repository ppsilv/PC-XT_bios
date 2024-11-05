boot1.asm basic floppy boot program
  The nasm source code is boot1.asm
  The result of the assembly is boot1.lst
  The hex dump of executable boot1 is boot1.dmp

  Once you have written a bootable program for NASM, only two
  command are needed to assemble and make a boot floppy:

      nasm -f bin boot1.asm
      dd if=boot1 of=/dev/fd0

  By default, the program is run in 16-bit mode for the 8086.
  There are many BIOS functions ready for your use.

  The boot1 program demonstrates basic text output to a screen,
  input from the keyboard and rebooting the computer.

; boot1.asm   stand alone program for floppy boot sector
; Compiled using            nasm -f bin boot1.asm
; Written to floppy with    dd if=boot1 of=/dev/fd0

; Boot record is loaded at 0000:7C00,
	ORG 7C00h
; load message address into SI register:
	LEA SI,[msg]
; screen function:
	MOV AH,0Eh
print:  MOV AL,[SI]
	CMP AL,0
	JZ done		; zero byte at end of string
	INT 10h		; write character to screen.
     	INC SI
	JMP print

; wait for 'any key':
done:   MOV AH,0
    	INT 16h		; waits for key press
			; AL is ASCII code or zero
			; AH is keyboard code

; store magic value at 0040h:0072h to reboot:
;		0000h - cold boot.
;		1234h - warm boot.
	MOV  AX,0040h
	MOV  DS,AX
	MOV  word[0072h],0000h   ; cold boot.
	JMP  0FFFFh:0000h	 ; reboot!

msg 	DB  'Welcome, I have control of the computer.',13,10
	DB  'Press any key to reboot.',13,10
	DB  '(after removing the floppy)',13,10,0
; end boot1

The file memtest.bin can be downloaded to your Intel computer
running linux. Then, put in a blank floppy.
From a command line type

   dd if=memtest.bin of=/dev/fd0

The memtest program will be written on the floppy.
Use standard Linux shutdown and reboot.

Upon reboot you will see memtest.bin run.
It identifies your CPU, speed, cache sizes and speed,
memory size and then proceeds to run a memory diagnostic.
Unless you are having memory problems, use 'esc' to exit.
Quickly remove the floppy so Linux will reboot.

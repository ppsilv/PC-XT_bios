PAGE	58,132
TITLE	FIFO	3-18-91	[4-23-91]

;Disassembly of FIFO.COM via ASMGEN,
;slightly tweaked, expanded for COM2..COM4, etc.
;
;David Kirschbaum
;Toad Hall
;kirsch%maxemail@uunet

LF	EQU	0AH
CR	EQU	0DH

COM1	EQU	03F8H		;COM1 base address
COM2	EQU	02F8H		;COM2 "
COM3	EQU	03E8H		;COM3 base address (usually)
COM4	EQU	02E8H		;COM4 base address (usually)

UART	EQU	COM2		;set to yours

CSEG	SEGMENT
	ASSUME DS:CSEG, SS:CSEG ,CS:CSEG ,ES:CSEG
	ORG	100H
Fifo	proc	near

	MOV	DX,UART + 2		;COMn UART base address
	MOV	AL,0C1H
	OUT	DX,AL
	XOR	AX,AX			;clear msb
	IN	AL,DX
	PUSH	AX			;save the result
	mov	dx,offset is16550	;assume 16550 UART
	AND	AL,0C0H			;mask
	CMP	AL,0C0H			;16550?
	jz	L0141			;yep
	 mov	DX,offset is8250	;well, then, it must be 8250
L0141:	MOV	AH,9			;display msg
	INT	21H
	POP	AX
	MOV	AH,4CH			;terminate, ERRORLEVEL in AL
	INT	21H

is16550	DB	' 16550 UART    $'
is8250	DB	' 8250 UART     $'


Fifo	ENDP

CSEG	ENDS
	END	Fifo

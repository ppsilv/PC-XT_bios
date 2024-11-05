        CPU 8086
        BITS 16

;Available Memory
; Dram memory
; 00000h-----+--------RAM 1MBytes
; F7FFFh-----|
; Eeprom memory
; F8000h-----+--------ROM 32KBytes
; F9000h-----|
; FA000h-----|
; FB000h-----|
; FC000h-----|
; FD000h-----|
; FE000h-----|
; FF000H-----|
; FFFFFh-----|


%imacro setloc  1.nolist
%assign pad_bytes (%1-($-$$)-START)
%if pad_bytes < 0
%assign over_bytes -pad_bytes
%error Preceding code extends beyond setloc location by over_bytes bytes
%endif
%if pad_bytes > 0
%warning Inserting pad_bytes bytes
 times  pad_bytes db 0FFh
%endif
%endm
;History
; 2444 - Version 10.0.01 implemented print2
; 2444 - Version 10.0.01 fixed erro in UART_TX, no push de BX
; 2444 - Version 10.0.02 implemented prompt
; 2445 - Version 10.0.03 now run in a 32k bytes of eeprom.
;                        START = 0x8000
;                        init2 = 0xE000
;                        reset = 0xFFF0
; 0C000h

%define	START		08000h		; BIOS starts at offset 08000h
%define DATE		'22/10/24'
%define MODEL_BYTE	0FEh		; IBM PC/XT
%define VERSION		'1.0.02'	; BIOS version

%define context_off  0x0
%define context_seg  0x2
%define context_len  0x4
%define context_val  0x6000

bioscseg	equ	0F000h
dramcseg        equ     06000h
biosdseg	equ	0040h

post_reg	equ	80h
serial_timeout	equ	7Ch	; byte[4] - serial port timeout values
equip_serial	equ	00h	; word[4] - addresses of serial ports
unused_reg	equ	0C0h	; used for hardware detection and I/O delays
equipment_list	equ	10h	; word - equpment list

reg_addr_dump   equ     0x0000
reg_buff_read   equ     0x0002  ; buffer 255 bytes
reg_counter     equ     0x0100  ; char counter in the buffer
reg_next_dumm   equ     0x0101  ; next variable

        org	START

           jmp     init
           ;12345678901234567890
msg0    db "8088 - CPU TXM/8 III",0
msg1    db "Paulo Silva  (c)2024",0
msg2    db "Mon86 V 1.0.00 2443A",0
msg3    db "1MB dram rom at28c64",0
row:    db 0, 40, 20, 84, 80

        setloc	0E000h

welcome		db	"XT 8088 BIOS, Version "
		db	VERSION
		db	". ", 0Dh
		db	"Paulo Silva(pgordao) - Copyright (C) 2024", 0Dh
		db	"CPU 8088-2   board TXM/8 III  ", 0Dh
		db	"Mon86 V ",VERSION ," 2443A 1MB Dram Rom at28c64", 0Dh, 0
        
init:
        cli				; disable interrupts
        cld				; clear direction flag
        mov ax, 0x6000
        mov es, ax
        mov ax, 0x7000                  ; Segmento Stack
        mov ss, ax
        mov ax, 0xF000
        mov ds, ax
        xor sp, sp

;	MOV AX, 0XF000		;SETS STACK SEGMENT TO 0xF000
;	MOV SS, AX		;
;	MOV SP, 0X7FE0		;0xF000:0X7FE0 = 0XF7FE0

        ;PPI 99 PortA = input PortB = output PortC = input
        mov AL, 0x99
        out 0x63, AL

	CALL LOAD_INT_VECTOR_TABLE	;COPIES INTERRUPT VECTOR TABLE TO ADDRESS 0X0000:0X0000
	CALL LOAD_BIOS_DATA_AREA	;COPIES BIOS DATA TO ADDRESS 0X0000:0X0400
	CALL INITIALIZE_8259		;INTERRUPT CONTROLLER
	CALL INITIALIZE_8253		;TIMER

	;THIS IS NOT PART OF THE V40 BUT IS BUILT INTO THE MAIN BOARD
	MOV  AL, 0X00 	;DISABLE SPK AND CHANEL CHECK
	OUT  0X61, AL	;PORT 0X61 CONTROL PORT		

;	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
;	MOV AL, 'I'		;SET AL TO CHAR	
;	INT 0X10		;CALL INTERRUPT 0X10
;	MOV AL, '1'		;SET AL TO CHAR	
;	INT 0X10		;CALL INTERRUPT 0X10
;	MOV AL, 'D'		;SET AL TO CHAR
;	INT 0X10		;CALL INTERRUPT 0X10

        call configure_uart

        call scr_clear
        mov  bx, welcome
        call print2

        mov  AX, 0xE000
        call writeRegAddrDump
        call dump

        ;;STI ;Enable interrupts

        jmp MainLoop

writeRegAddrDump:
        push AX
        mov AX, dramcseg ; Segmento DRAM
        mov ES, AX
        pop AX
        mov word es:[reg_addr_dump], AX
        mov bx, word es:[reg_addr_dump]
        ret

ReadLine:
        mov cl,0x0
        mov  bx,  reg_buff_read
loop:
        call printPrompt
loopP:  ;RX blocante
        call UART_RX_blct       
 ;       jnc  loopP
        call printch

        mov  byte es:[bx], al 
        mov  byte es:[bx+1], 0x0 
        inc  bx

        cmp  AL, 0x41
        jz   tbled1
        cmp  AL, 0x42
        jz   tbled2

        CMP  AL, 0x0A
        JNZ  loopP
        call printLf
        call printPrompt
        mov  BX, reg_buff_read
        call printFromDram
        ret

MainLoop:
        CALL tbled1
        mov cx, 0x1fFF
	call	basicDelay    
        CALL tbled2
        mov cx, 0x1fFF
	call	basicDelay    

;;        call ReadLine
        jmp MainLoop       

tbled1: 
        in  al, 0x61
        or  al, 0x08 
        out 0x61, al
        ret
tbled2:
        in  al, 0x61
        and al, 0xF7
        out 0x61, al
          ret

LOAD_INT_VECTOR_TABLE:
	CLD
	MOV AX, 0X0000
	MOV ES, AX
	MOV DI, 0X0000
	MOV AX, CS
	MOV DS, AX
	MOV SI, INT_VECTOR_TABLE
	MOV CX, INT_VECTOR_TABLE.END - INT_VECTOR_TABLE
	REP MOVSB
	RET

LOAD_BIOS_DATA_AREA:
	CLD
	MOV AX, 0X0040
	MOV ES, AX
	MOV DI, 0X0000
	MOV AX, CS
	MOV DS, AX
	MOV SI, BIOS_DATA_AREA
	MOV CX, BIOS_DATA_AREA.END - BIOS_DATA_AREA
	REP MOVSB
	RET

INITIALIZE_8259:
	PUSH AX

	MOV AL, 0X13		;ICW1
	OUT 0X20, AL
	MOV AL, 0X08		;ICW2
	OUT 0X21, AL
	MOV AL, 0X09		;ICW4
	OUT 0X21, AL

        ;OCW1
        ;UNMASK IRQ0, IRQ1, IRQ6
	MOV AL, 0XFC		
	OUT 0X21, AL		
	POP AX
	RET

INITIALIZE_8253:

	PUSH AX
	PUSH CX

	MOV AL, 0X36 		;00110110b  
			        ;CHANNEL 0
			        ;WRITE LOW BYTE THEN HIGH BYTE
			        ;MODE 3 
			        ;16 BIT COUNTER 
			
	OUT 0X43, AL		;CONTROL REG

	MOV CX, 0XFFFF		;COUNT 

	MOV AL, CL		;WRITE LOW BYTE OF COUNT
	OUT 0X40, AL		;PORT 0X40
			        ;INTERNAL FLIP-FLOP INC
	MOV AL, CH		;WRITE HIGH BYTE OF COUNT 
	OUT 0X40, AL		;PORT 040

	;;;;;;;;;;;
	;TEST TONE
	;;;;;;;;;;;
	MOV AL, 0X03		;ENABLE SPK AND TIMMER 2 'GO'
	OUT 0X61, AL		;PORT 0X61 CONTROL PORT
	MOV AL, 0XB6
	OUT 0X43, AL
	MOV AL, 0X00
	OUT 0X42, AL
	MOV AL, 0X05
	OUT 0X42, AL

	mov cx, 0xafff
	call	basicDelay
	MOV AL, 0X00		;DISABLE SPK AND TIMMER 2 'GO'
	OUT 0X61, AL		;PORT 0X61 CONTROL PORT

	POP CX
	POP AX

	RET          
;=================================
; Dump memory
; Memory address: bx
;        counter: cx
dump:
        PUSH DS
        MOV  AX, 0xF000
        MOV DS, AX
        mov  Cl, 16

dump_01:        
        mov  al, 0x0d
        call UART_TX
        mov  AX, BX
        call print_hex
        mov  al, ':'
        call UART_TX
        MOV  AL, ' '
        CALL printch
        
        ;;Write 16 bytes em hexadecimal
        MOV  CH, 16
dump_02:
        MOV  AL, DS:[BX]
        CALL byte_to_hex_str
        PUSH AX
        CALL printch
        POP  AX
        MOV  AL, AH
        CALL printch
        MOV  AL, ' '
        CALL printch
        INC  BX
        DEC  CH
        JNZ  dump_02
        ;;Wrote 16 bytes

        MOV  AL, ' '
        CALL printch

        SUB  BX, 16

        ;;Write 16 bytes em ASCII
        MOV  CH, 16
dump_03:
        MOV  AL, DS:[BX]
        CMP  AL, 0x20
        JC  printPonto ; Flag carry set to 1 AL < 0x20
        CMP  AL, 0x80
        JnC  printPonto ; Flag carry set to 0 AL > 0x80
        CALL printch
        INC  BX
        DEC  CH
        JNZ  dump_03
        jmp  dump_Fim
printPonto:        
        MOV  AL, '.'
        CALL printch
        INC  BX
        DEC  CH
        JNZ  dump_03
        ;;Wrote 16 bytes

dump_Fim:
        DEC  CL
        JNZ  dump_01
        mov  al, 0x0d
        call UART_TX
        POP DS
        ret

printPrompt:
        mov al, '>'
        call printch
        ret

printLf:
        mov al, 0x0D
        call printch
        ret

writeRam:
        mov byte ES:[BX], AL
        ret
readRam:
        mov AL, byte ES:[BX]
        ret
;byte_to_hex_str
;This function return in AX the ascii code for hexadecimal number from 0 to F
;Parameters:
;               AL = imput
;               AX = output
;Changes CL
byte_to_hex_str:
        PUSH CX
        mov ah, al
        mov cl, 4
        shr al, cl
        and ax, 0x0f0f
        cmp al, 0x09
        jbe .1
        add al, 'A' - '0' - 10
.1:
        cmp ah, 0x09
        jbe .2
        add ah, 'A' - '0' - 10
.2:
        add ax, "00"
.ret:
        POP CX
        ret

;=========================================================================
; print_digit - print hexadecimal digit
; Input:
;	AL - bits 3...0 - digit to print (0...F)
; Output:
;	none
;-------------------------------------------------------------------------
print_digit:
	push	ax
	push	bx
	and	al,0Fh
	add	al,'0'			; convert to ASCII
	cmp	al,'9'			; less or equal 9?
	jna	.1
	add	al,'A'-'9'-1		; a hex digit
.1:
        call    printch
	pop	bx
	pop	ax
	ret

;=========================================================================
; print_hex - print 16-bit number in hexadecimal
; Input:
;	AX - number to print
; Output:
;	none
;-------------------------------------------------------------------------
print_hex:
	xchg	al,ah
	call	print_byte		; print the upper byte
	xchg	al,ah
	call	print_byte		; print the lower byte
	ret
;=========================================================================
; print_byte - print a byte in hexadecimal
; Input:
;	AL - byte to print
; Output:
;	none
;-------------------------------------------------------------------------
print_byte:
	rol	al,1
	rol	al,1
	rol	al,1
	rol	al,1
	call	print_digit
	rol	al,1
	rol	al,1
	rol	al,1
	rol	al,1
	call	print_digit
	ret

%include "DRV16C550_8088.asm"		
;%include "serial1.inc"	
;%include "serial2.inc"	
;%include "errno.inc"	
;%include "messages.inc"	
%include "screen.asm"	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BIOS DATA AREA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%INCLUDE "asm/bios_data.asm"


INT_VECTOR_TABLE:
	DW INT00, 0xF800
	DW INT01, 0xF800
	DW INT02, 0xF800
	DW INT03, 0xF800
	DW INT04, 0xF800
	DW INT05, 0xF800
	DW INT06, 0xF800
	DW INT07, 0xF800
	DW INT08, 0xF800
	DW INT09, 0xF800
	DW INT0A, 0xF800
	DW INT0B, 0xF800
	DW INT0C, 0xF800
	DW INT0D, 0xF800
	DW INT0E, 0xF800
	DW INT0F, 0xF800
	DW INT10, 0xF800
	DW INT11, 0xF800
	DW INT12, 0xF800
	DW INT13, 0xF800
	DW INT14, 0xF800
	DW INT15, 0xF800
	DW INT16, 0xF800
	DW INT17, 0xF800
	DW INT18, 0xF800
	DW INT19, 0xF800
	DW INT1A, 0xF800
	DW INT1B, 0xF800
	DW INT1C, 0xF800
	DW INT1D, 0xF800
	DW INT1E, 0xF800
	DW INT1F, 0xF800
.END:
INT00: ;Divide by Zero
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT

INT01: ;Single Step
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '1'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT

INT02: ;Nonmaskable 
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '2'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT

INT03: ;Breakpoint
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '3'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT

INT04: ;Overflow
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '4'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT

INT05: ;Print Screen
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '5'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT

INT06: ;Reserved
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '6'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT

INT07: ;Reserved
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '7'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT

INT08: ;Reserved
	PUSH AX			;STORE AX
	PUSH BX			;STORE BX
	PUSH DS			;STORE DS
	MOV AX, 0X0040			;BIOS DATA AREA
	MOV DS, AX			;SET DATA SEGMENT 
	MOV BX, 0X006C			;SET BX TO TICK COUNTER
	INC WORD [BX]			;INC LOWER WORD TICK BY ONE
	CMP WORD [BX], 0X0000			;CMP TO 0 TO SEE IF ROLLED OVER 	
	JNZ .END_INT08			;IF NOT END
	ADD BX, 0X0002			;ADD 2 TO BX FOR NEXT WORD
	INC WORD [BX]			;INC UPPER WORD
  .END_INT08:				;END LABEL
	POP DS			;RESTORE DS
	POP BX			;RESTORE BX


    	MOV AL, 0x20
    	OUT 0x20, AL	
	POP AX			;RESTORE AX
	IRET			;RETURN FROM INTERRUPT	

;INT09:	
%INCLUDE "asm/int09.asm"

INT0A: ;Reserved
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, 'A'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	IRET		;RETURN FROM INTERRUPT	

INT0B: ;Communications
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, 'B'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	IRET		;RETURN FROM INTERRUPT	
	
INT0C: ;Communications
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, 'C'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	IRET		;RETURN FROM INTERRUPT			

INT0D: ;Disk
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, 'D'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	IRET	
INT0E: ;Disk
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, 'E'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	IRET	;RETURN FROM INTERRUPT
INT0F: ;Printer
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '0'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, 'F'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	IRET		;RETURN FROM INTERRUPTINT0F:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;INT 0X10
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%INCLUDE "asm/int10.asm"

INT11: ;Equipment Check	
	PUSH DS			;STORE DS
	PUSH BX			;STORE BX
	MOV AX, 0X0040			;BIOS DATA AREA
	MOV DS, AX			;SET DATA SEGMENT
	MOV BX, 0X0010			;OFFSET 0X10 EQUIPMENT LIST
	MOV AX, WORD [BX]			;STORE EQUPMENT LIST AX
	POP BX			;RESTORE BX
	POP DS			;RESTORE DS
	IRET			;RETRUN
INT12: ;Memory	
	PUSH DS			;STORE DS
	PUSH BX			;STORE BX
	MOV AX, 0X0040			;BIOS DATA AREA
	MOV DS, AX			;SET DATA SEGMENT
	MOV BX, 0X0013			;OFFSET 0X13 FOR MEMORY SIZE
	MOV AX, WORD [BX]			;STORE MEMORY SIZE AX
	POP BX			;RESTORE BX
	POP DS			;RESTORE DS
	IRET			;RETRUN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;INT 0X13
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%INCLUDE "asm/int13.asm"

INT14: ;Communications
	IRET
INT15: ;Cassette
	MOV AL, 0X00			;AL = 0X00
	MOV AH, 0X86			;NO CASSETTE PRESENT
	STC			;NOT BIOS SUPPORTED
				;THIS IS HOW I RETURN THE CARRY FLAG
	PUSH AX			;STORE AX
	PUSHF			;STORE FLAGS
	POP AX			;GET AX = FLAGS
	PUSH BP			;STORE BP
	MOV BP, SP              ;Copy SP to BP for use as index
	ADD BP, 0X08			;offset 8
	AND WORD [BP], 0XFFFE			;CLEAR CF = ZER0
	AND AX, 0X0001			;ONLY CF 
	OR	WORD [BP], AX		;SET CF AX
	POP BP               			;RESTORE BASE POINTER
	POP AX			;RESTORE AX	
	IRET			;RETRUN	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;INT 0X16
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%INCLUDE "asm/int16.asm"
	
INT17: ;Printer	
	IRET

INT18: ;Resident Basic
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '1'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '8'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT

INT19: ;Bootstrap
	JMP 0XFFFF:0X0000			;JUMPS TO BOOT CODE

INT1A: ;Time of Day
	PUSH DS
  	PUSH BX
	MOV AX, 0X0040			;BIOS DATA AREA
	MOV DS, AX			;SET DS
	MOV BX, 0X006C			;NUMBER OF CLICKS
	MOV DX, WORD [BX]			;SET DX CLICKS
	ADD BX, 0X0002
	MOV CX, WORD [BX]			;0X006E
	AND CX, 0x000F
	MOV AX, 0X0000			;
	POP BX
	POP DS
	IRET

INT1B: ;Keyboard Break
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '1'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, 'B'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT

INT1C: ;Timer Tick
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '1'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, 'C'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT

INT1D: ;Video Initialization 
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '1'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, 'D'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT

INT1E: ;Diskette Parameters
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '1'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, 'E'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT

INT1F: ;Video Graphics Characters
	MOV AH, 0X0E		;SET AH 0X0E FOR TELOTYPE
	MOV AL, 'I'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, '1'		;SET AL TO CHAR	
	INT 0X10		;CALL INTERRUPT 0X10
	MOV AL, 'F'		;SET AL TO CHAR
	INT 0X10		;CALL INTERRUPT 0X10
	HLT		;HLT PROCESSOR NOT SUPPORTED INTERRUPT
	IRET		;RETURN FROM INTERRUPT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EXTRA FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%INCLUDE "asm/functions.asm"

        setloc	0FFF0h			; Power-On Entry Point
reset:
        jmp 0xF000:init

        setloc	0FFF5h			; ROM Date in ASCII
        db	DATE			; BIOS release date MM/DD/YY
        db	20h

        setloc	0FFFEh			; System Model byte
        db	MODEL_BYTE
        db	0ffh

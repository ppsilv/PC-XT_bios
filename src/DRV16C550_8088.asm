        CPU 8086
        BITS 16

; Port
COM1:	DW		0x3F8
; Here are the port numbers for various UART registers:
uart_tx_rx 		EQU  0x3f8 ; 0 DLAB = 0 for Regs. TX and RX
uart_DLL 		EQU  0x3f8 ; 0 DLAB = 1 Divisor lacth low
uart_IER 		EQU  0x3f9 ; 1 DLAB = 0 Interrupt Enable Register
uart_DLH 		EQU  0x3f9 ; 1 DLAB = 1 Divisor lacth high
uart_ISR 		EQU  0x3fa ; 2 IIR Interrupt Ident. Register READ ONLY
uart_FCR 		EQU  0x3fa ; 2 Fifo Control Resgister WRITE ONLY
uart_LCR 		EQU  0x3fb ; 3 Line Control Register
uart_MCR 		EQU  0x3fc ; 4 Modem Control Register
uart_LSR 		EQU  0x3fd ; 5 Line Status Register
uart_MSR 		EQU  0x3fe ; 6 Modem Status Register
uart_scratch 	EQU  0x3ff ; 7 SCR Scratch Register

UART_FREQUENCY		equ 4915000
;Fomula UART_FREQUENCY/(  9600 * 16)
;Baudrates
UART_BAUD_9600		EQU 32
UART_BAUD_19200		EQU 16
UART_BAUD_38400		EQU  8
UART_BAUD_56800		EQU  5
UART_BAUD_115200	EQU  3
UART_BAUD_230400	EQU  1

UART_TX_WAIT		EQU	0x7fff		; Count before a TX times out

msg0_01:   db "Serial driver for 16C550",0
;configure_uart
;Parameters:None
;			
;			
configure_uart:
			mov cx, 0x1fff
			call	basicDelay
			MOV		AL,0x0	 		;
			MOV		DX, uart_IER
			OUT  	DX,	AL	; Disable interrupts

			mov cx, 0x1f
			call	basicDelay

			MOV		AL, 0x80			;
			MOV		DX, uart_LCR
			OUT     DX,	AL 	; Turn DLAB on
			mov cx, 0x1f
			call	basicDelay

			MOV		AL, UART_BAUD_38400 ;0x08
			MOV		DX, uart_DLL
			OUT     DX,   AL	; Set divisor low
			mov cx, 0x1f
			call	basicDelay

			MOV		AL, 0x00		;
			MOV		DX, uart_DLH
			OUT     DX,	AL	; Set divisor high
			mov cx, 0x1f
			call	basicDelay

			MOV     AL, 0x03	; AH	
			MOV		DX, uart_LCR
			OUT     DX,	AL	; Write out flow control bits 8,1,N
			mov cx, 0x1f
			call	basicDelay

			MOV 	AL,0x81			;
			MOV		DX, uart_ISR
			OUT     DX,	AL	; Turn on FIFO, with trigger level of 8.
								                ; This turn on the 16bytes buffer!
			RET
;UART_RX:
;Parameters: 
;			AL = return the available character
;			If al returns with a valid char flag carry is set, otherwise
;			flag carry is clear
UART_RX:	
			MOV DX, uart_LSR
			IN	AL, DX	 		; Get the line status register
			AND AL, 0x01		; Check for characters in buffer
			CLC 				; Clear carry
			JZ	END				; Just ret (with carry clear) if no characters
			MOV DX, uart_tx_rx
			IN	AL, DX			; Read the character from the UART receive buffer
			STC 				; Set the carry flag
END:			
			RET

UART_RX_blct:	
			MOV DX, uart_LSR
			IN	AL, DX	 		; Get the line status register
			AND AL, 0x01		; Check for characters in buffer
			JZ	UART_RX_blct	; Just loopif no characters
			MOV DX, uart_tx_rx
			IN	AL, DX			; Read the character from the UART receive buffer
			RET


printch:
UART_TX:	
			PUSH DX
			PUSH CX 	
			PUSH BX
			PUSH AX
			MOV BX, UART_TX_WAIT	; Set CB to the transmit timeout
LOOP_UART_TX:
			MOV DX, uart_LSR
			IN	AL,	DX 				; Get the line status register
			AND AL, 0x60			; Check for TX empty
			JNZ	OUT_UART_TX			; If set, then TX is empty, goto transmit
			mov	cx, 0x17ff
			call basicDelay
			DEC	BX
			JNZ LOOP_UART_TX		; Otherwise loop
			POP	AX					; We've timed out at this point so
			POP BX
			CLC						; Clear the carry flag and preserve AX
			RET
OUT_UART_TX:
			POP	AX					; Good to send at this point, so	
			CMP AL, 0x0D
			JZ  println
			MOV	DX, uart_tx_rx
			OUT	DX, AL		; Write the character to the UART transmit buffer
			mov	cx, 0x2ff
			call basicDelay
			POP BX
			POP CX
			POP DX
			STC						; Set carry flag
			RET
println:
			MOV	DX, uart_tx_rx
			OUT	DX, AL		; Write the character to the UART transmit buffer
			mov	cx, 0xff
			call basicDelay
			MOV AL, 0x0A
			MOV	DX, uart_tx_rx
			OUT	DX, AL		; Write the character to the UART transmit buffer
			mov	CX, 0xff
			call basicDelay
			POP BX	
			POP CX
			POP DX
			STC						; Set carry flag
			RET

;print
;parameters:
;          bx = message address
;
;print:
;        	mov  al,byte ds:[bx]
;        	cmp  al,0h
;        	jz   fimPrint;
;
;			MOV	DX, uart_tx_rx
; ;       	OUT	DX, AL
;			mov	cx, 0xff
;			call basicDelay
;
;        	inc  bx
;        	jmp  print
;fimPrint:   ret

print2:
        	mov  al,byte ds:[bx]
        	cmp  al,0h
        	jz   fimPrint2
cont:
			call UART_TX
			JNC	cont

        	inc  bx
        	jmp  print2
fimPrint2:   ret		

printFromDram:
        	mov  al,byte es:[bx]
        	cmp  al,0h
        	jz   fimPrintFromDram

contFromDram:
			call UART_TX
			JNC	contFromDram
        	inc  bx
        	jmp  printFromDram

fimPrintFromDram:  
			ret		

;print3:
;        	mov  al,byte ds:[bx]
;        	cmp  al,0h
;        	jz   fimPrint3
;
;			MOV	DX, uart_tx_rx
;			OUT	DX, AL		; Write the character to the UART transmit buffer
;			mov	cx, 0xff
;			call basicDelay
;
;        	inc  bx
;        	jmp  print2
;fimPrint3:   ret		

	
basicDelay:
        dec cx
        jnz basicDelay
        ret

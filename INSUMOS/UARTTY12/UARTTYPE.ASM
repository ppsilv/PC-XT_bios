title "UART Detector"
UART_DET  segment para public 'code'
        assume  cs:UART_DET, ds:UART_DET, es:UART_DET
page 60, 132

; comments -- the WRITE function is left up to your own implementation
; v1.1 Toad Hall Tweak, May 91
;        Yeah, well, we can do that.
;        David Kirschbaum
;        Toad Hall
;        kirsch%maxemail@uunet
; v1.2 wvl tweak, May 92
;        Heck, we can even check all the comm ports, not just the one 
;        which is hard coded and display the port base address, too.
;        William Luitje
;        luitje@m-net.ann-arbor.mi.us

; constants

BIOSdseg equ    40h          ; data segment for BIOS
IIR      equ    2            ;Interrupt Indentification Register offset

         .model tiny           ; tiny model (.COM)
         .code
         org    100h           ; .COM files start at 100h

entry:
         jmp    start          ; jump to start of progarm

; variables

_UARTbase       dw      ?    ;UART base address
xbyte2          db      ?

jmp_table   dw      uart_8250_or_16450, uart_unknown, uart_16550, uart_16550a
banner          db      'UART Detector, Version 1.2',0ah,0dh
                db      'Comm Base Adr Type',0ah,0dh,'$'
type_msg        db      '  '
portno          db      'x   $'
base_msg        db      ' xxxx   $'
notinst         db      'Not Installed',0ah,0dh,'$'
m8250           db     '8250',0ah,0dh,'$'
m16450          db     '16450',0ah,0dh,'$'
munknown        db     'Unknown (possibly 82510)',0ah,0dh,'$'
m16550          db     '16550',0ah,0dh,'$'
m16550a         db     '16550A',0ah,0dh,'$'
HexTbl          db     '0123456789ABCDEF'

; ------------------------ MAIN ------------------------------

start    proc
;-Display banner
        mov     dx,offset banner
         call   Display
         mov    ax, BIOSdseg
         mov    es, ax         ; move bios data segment into ES
;-For (cx = 0; cx<4; ++cx)
         xor    cx,cx
;---Find & display UART type
start1:  call   findType
         inc    cx
         cmp    cx,4
         jne    start1
;-End For
;-Exit
         mov    ax, 4c00h     ;terminate
         int    21h

;------------Find UART type for a given comm port-------------
; CX:  Comm port # -1
findType:
         push   cx
;-Display port number
         mov    al,cl
         add    al,031h
        mov     portno,al
        mov     dx,offset type_msg
         call   Display
;-Get Requested Port Base Address
         shl    cx,1
         mov    bx,cx
        mov     dx,es:[bx]      ; get offset for serial port
        mov     _UARTbase,dx    ; _UARTbase := serial port base address
;-Display Base Address 
         call   Hex2Asc
         mov    dx,offset base_msg
         call   Display
         mov    dx,_UARTBASE
;-If base address == 0
        test    dx,0ffffh
        jne     find1           ;no, cool
;---Display "Not installed"
         mov    dx,offset notinst
         call   Display
         jmp    findret
;-Else
;---Figure out which type IS installed
find1:
         mov    dx,_UARTbase
         add    dx,IIR
         in     al, dx         ; al := port [dx]
         mov    xbyte2, al    ; xbyte2 := al
         mov    al, 0c1h      ; al := $C1
         out    dx, al         ; port [dx] := al
         in     al, dx         ; al := port [dx]
         and    al, 0c0h      ; al := (al AND $C0)
         mov    cl, 5          ; cl := 5
         shr    al, cl         ; al := (al shr 5) (5 for word jmp)
         xor    ah,ah          ; ah := 0   v1.1
         mov    bp, ax         ; bp := ax (al)
         mov    al, xbyte2    ; al := xbyte2
         out    dx, al         ; port [dx] := al
         jmp    CS:[jmp_table+bp] ;    v1.1

uart_8250_or_16450:
         mov    dx, _UARTbase  ; dx := _UARTbase
         add    dx, 7          ; dx := dx + 7
         in     al, dx         ; al := port [dx]
         mov    xbyte2, al    ; xbyte2 := al
         mov    al, 0fah      ; al := $FA
         out    dx, al         ; port [dx] := al
         in     al, dx         ; al := port [dx]
         cmp    al, 0fah      ; if (al <> $FA)
         jne    uart_16450    ; then uart is 16450
         mov    al, 0afh      ; al := $AF
         out    dx, al         ; port [dx] := al
         in     al, dx         ; al := port [dx]
         cmp    al, 0afh      ; if (al <> $AF)
         jne    uart_8250     ; then uart is 8250
         mov    al, xbyte2    ; else al := xbyte2
         out    dx, al         ; port [dx] := al
         jmp    short uart_16450 ; uart is 16450

uart_8250:
         mov    dx,offset m8250
         call   Display
         jmp    findret

uart_16450:
         mov    dx,offset m16450
         call   Display
         jmp    findret

uart_unknown:
         mov    dx,offset munknown
         call   Display
         jmp    findret

uart_16550:
         mov    dx,offset m16550
         call   Display
         jmp    findret

uart_16550a:
         mov    dx,offset m16550a
         call   Display
findret: pop    cx
         ret

Display:
         mov    ah,09h         ;display msg in DX  v1.1
         int    21H            ;via DOS   v1.1
         ret
Hex2Asc:
         xor    bl,bl
         mov    bl,dh                  ;display high nibble of dh
         mov    cl,4
         shr    bl,cl
         mov    al,byte ptr HexTbl[bx]
         mov    base_msg+1,al
         mov    bl,dh                  ;display low nibble of dh
         and    bl,0fH
         mov    al,HexTbl[bx]
         mov    base_msg+2,al
         mov    bl,dl                  ;display high nibble of dl
         mov    cl,4
         shr    bl,cl
         mov    al,HexTbl[bx]
         mov    base_msg+3,al
         mov    bl,dl                  ;display low nibble of dl
         and    bl,0fH
         mov    al,HexTbl[bx]
         mov    base_msg+4,al
         ret

start    endp                  ; end of proc

END      entry                 ; end of program
UART_DET  ends


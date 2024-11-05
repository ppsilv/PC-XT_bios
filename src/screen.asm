
cls     db 0x1B,"[2J",0
curpos  db 0x1B,"[!;!H",0

;=====================
; ESC [ 2 J
;
scr_clear:
        mov	bx, cls
        call print2	
        ret
;=====================
; ESC [ Pl ; Pc H
; input:
;	dh = y position
; 	dl = x position
; MARK: scr_goto
scr_goto:
        push DS
        mov AX, 0x0
        mov DS, AX
        mov bx, AX
        mov byte ds:[bx],0x1B
        inc bx
        mov byte ds:[bx],'['
        inc bx
        mov byte ds:[bx],10
        inc bx
        mov byte ds:[bx],';'
        inc bx
        mov byte ds:[bx],10
        inc bx
        mov byte ds:[bx],'H'  
        inc bx
        mov byte ds:[bx],0x0


        mov AX, 0x0
        mov bx, AX
        call print2
        POP DS
		ret



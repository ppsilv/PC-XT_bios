

i00:
        mov al, '0'
        int 10h
    int 0h
    ret
i01:
    int 1h
    ret
i02:
    int 2h
    ret
i03:
    int 3h
    ret
i04:
    int 4h
    ret
i05:
    int 5h
    ret
i06:
    int 6h
    ret
i07:
    int 7h
    ret
i08:
    int 8h
    ret
i09:
    int 9h
    ret
i0A:
   int 0Ah
   ret
i0B:
   int 0Bh
   ret
i0C:
   int 0Ch
   ret
i0D:
   int 0Dh
   ret
i0E:
   int 0Eh
   ret
i0F:
   int 0Fh
   ret
i10:
   int 10h
   ret
i11:
   int 11h
   ret
i12:
   int 12h
   ret
i13:
   int 13h
   ret
i14:
   int 14h
   ret
i15:
   int 15h
   ret
i16:
   int 16h
   ret
i17:
   int 17h
   ret
i18:
   int 18h
   ret
i19:
   int 19h
   ret
i1A:
   int 1Ah
   ret
i1B:
   int 1Bh
   ret
i1C:
   int 1Ch
   ret
i1D:
    int 1Dh
    ret
i1E:
    int 1Eh
    ret
i1F:
    int 1Fh
    ret

go2intr:
        CMP AL, 0
        jz  i00
        CMP AL, 1 
        jz  i01
        CMP AL, 2  
        jz  i02
        CMP AL, 3
        jz  i03
        CMP AL, 4
        jz  i04
        CMP AL, 5
        jz  i05
        CMP AL, 6
        jz  i06
        CMP AL, 7
        jz  i07
        CMP AL, 8
        jz  i08
        CMP AL, 9
        jz  i09
        CMP AL, 0xA
        jz  i0A
        CMP AL, 0xB 
        jz  i0B 
        CMP AL, 0xC
        jz  i0C
        CMP AL, 0xD
        jz  i0D
        CMP AL, 0xE
        jz  i0E
        CMP AL, 0xF
        jz  i0F
        CMP AL, 0x10
        jz  i10
        CMP AL, 0x11
        jz  i11
        CMP AL, 0x12
        jz  i12
        CMP AL, 0x13
        jz  i13
        CMP AL, 0x14
        jz  i14
        CMP AL, 0x15 
        jz  i15
        CMP AL, 0x16
        jz  i16
        CMP AL, 0x17
        jz  i17
        CMP AL, 0x18
        jz  i18
        CMP AL, 0x19
        jz  i19
        CMP AL, 0x1A
        jz  i1A
        CMP AL, 0x1B
        jz  i1B
        CMP AL, 0x1C
        jz  i1C
        CMP AL, 0x1D
        jz  i1D
        CMP AL, 0x1E
        jz  i1E
        CMP AL, 0x1F
        jz  i1F
        ret
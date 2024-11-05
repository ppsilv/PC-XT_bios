hello.asm basic stand alone program
  The nasm source code is hello.asm
  The result of the assembly is hello.lst
  Running the program produces output hello.out

  This program demonstrates basic text output to a screen.
  No "C" library functions are used.
  Calls are made to the operating system directly. (int 80 hex)

;  hello.asm  a first program for nasm for Linux, Intel, gcc
;
; assemble:	nasm -f elf -l hello.lst  hello.asm
; link:		gcc -o hello  hello.o
; run:	        hello
; output is:	Hello World

	SECTION .data		; data section
msg:	db "Hello World",10	; the string to print, 10=cr
len:	equ $-msg		; "$" means "here"
				; len is a value, not an address

	SECTION .text		; code section
        global main		; make label available to linker
main:				; standard  gcc  entry point

	mov	edx,len		; arg3, length of string to print
	mov	ecx,msg		; arg2, pointer to string
	mov	ebx,1		; arg1, where to write, screen
	mov	eax,4		; write sysout command to int 80 hex
	int	0x80		; interrupt 80 hex, call kernel

	mov	ebx,0		; exit code, 0=normal
	mov	eax,1		; exit command to kernel
	int	0x80		; interrupt 80 hex, call kernel

printf1.asm basic calling printf
  The nasm source code is printf1.asm
  The result of the assembly is printf1.lst
  The equivalent "C" program is printf1.c
  Running the program produces output printf1.out

  This program demonstrates basic use of "C" library function  printf.
  The equivalent "C" code is shown as comments in the assembly language.

; printf1.asm   print an integer from storage and from a register
; Assemble:	nasm -f elf -l printf.lst  printf1.asm
; Link:		gcc -o printf1  printf1.o
; Run:		printf1
; Output:	a=5, eax=7

; Equivalent C code
; /* printf1.c  print an int and an expression */
; #include
; int main()
; {
;   int a=5;
;   printf("a=%d, eax=%d\n", a, a+2);
;   return 0;
; }

; Declare some external functions
;
        extern	printf		; the C function, to be called

        SECTION .data		; Data section, initialized variables

	a:	dd	5		; int a=5;
fmt:    db "a=%d, eax=%d", 10, 0 ; The printf format, "\n",'0'


        SECTION .text                   ; Code section.

        global main		; the standard gcc entry point
main:				; the program label for the entry point
        push    ebp		; set up stack frame
        mov     ebp,esp

	mov	eax, [a]	; put a from store into register
	add	eax, 2		; a+2
	push	eax		; value of a+2
        push    dword [a]	; value of variable a
        push    dword fmt	; address of ctrl string
        call    printf		; Call C function
        add     esp, 12		; pop stack 3 push times 4 bytes

        mov     esp, ebp	; takedown stack frame
        pop     ebp		; same as "leave" op

	mov	eax,0		;  normal, no error, return value
	ret			; return



printf2.asm more types with printf
  The nasm source code is printf2.asm
  The result of the assembly is printf2.lst
  The equivalent "C" program is printf2.c
  Running the program produces output printf2.out

  This program demonstrates basic use of "C" library function  printf.
  The equivalent "C" code is shown as comments in the assembly language.

; printf2.asm  use "C" printf on char, string, int, double
;
; Assemble:	nasm -f elf -l printf2.lst  printf2.asm
; Link:		gcc -o printf2  printf2.o
; Run:		printf2
; Output:
;Hello world: a string of length 7 1234567 6789ABCD 5.327000e-30 -1.234568E+302
;
; A similar "C" program
; #include
; int main()
; {
;   char   char1='a';         /* sample character */
;   char   str1[]="string";   /* sample string */
;   int    int1=1234567;      /* sample integer */
;   int    hex1=0x6789ABCD;   /* sample hexadecimal */
;   float  flt1=5.327e-30;    /* sample float */
;   double flt2=-123.4e300;   /* sample double */
;
;   printf("Hello world: %c %s %d %X %e %E \n", /* format string for printf */
;          char1, str1, int1, hex1, flt1, flt2);
;   return 0;
; }


        extern printf                   ; the C function to be called

        SECTION .data                   ; Data section

msg:    db "Hello world: %c %s of length %d %d %X %e %E",10,0
					; format string for printf
char1:	db	'a'			; a character
str1:	db	"string",0	        ; a C string, "string" needs 0
len:	equ	$-str1			; len has value, not an address
inta1:	dd	1234567		        ; integer 1234567
hex1:	dd	0x6789ABCD	        ; hex constant
flt1:	dd	5.327e-30		; 32-bit floating point
flt2:	dq	-123.456789e300	        ; 64-bit floating point

	SECTION .bss

flttmp:	resq 1			        ; 64-bit temporary for printing flt1

        SECTION .text                   ; Code section.

        global	main		        ; "C" main program
main:				        ; label, start of main program

	fld	dword [flt1]	        ; need to convert 32-bit to 64-bit
	fstp	qword [flttmp]          ; floating load makes 80-bit,
	                                ; store as 64-bit
	                                ; push last argument first
	push	dword [flt2+4]	        ; 64 bit floating point (bottom)
	push	dword [flt2]	        ; 64 bit floating point (top)
	push	dword [flttmp+4]        ; 64 bit floating point (bottom)
	push	dword [flttmp]	        ; 64 bit floating point (top)
	push	dword [hex1]	        ; hex constant
	push	dword [inta1]	        ; integer data pass by value
	push	dword len	        ; constant pass by value
	push	dword str1		; "string" pass by reference
        push    dword [char1]		; 'a'
        push    dword msg		; address of format string
        call    printf			; Call C function
        add     esp, 40			; pop stack 10*4 bytes

        mov     eax, 0			; exit code, 0=normal
        ret				; main returns to operating system


intarith.asm simple integer arithmetic
  The nasm source code is intarith.asm
  The result of the assembly is intarith.lst
  The equivalent "C" program is intarith.c
  Running the program produces output intarith.out

  This program demonstrates basic integer arithmetic  add, subtract,
  multiply and divide.
  The equivalent "C" code is shown as comments in the assembly language.

; intarith.asm    show some simple C code and corresponding nasm code
;                 the nasm code is one sample, not unique
;
; compile:	nasm -f elf -l intarith.lst  intarith.asm
; link:		gcc -o intarith  intarith.o
; run:		intarith
;
; the output from running intarith.asm and intarith.c is:
; c=5  , a=3, b=4, c=5
; c=a+b, a=3, b=4, c=7
; c=a-b, a=3, b=4, c=-1
; c=a*b, a=3, b=4, c=12
; c=c/a, a=3, b=4, c=4
;
;The file  intarith.c  is:
;  /* intarith.c */
;  #include
;  int main()
;  {
;    int a=3, b=4, c;
;
;    c=5;
;    printf("%s, a=%d, b=%d, c=%d\n","c=5  ", a, b, c);
;    c=a+b;
;    printf("%s, a=%d, b=%d, c=%d\n","c=a+b", a, b, c);
;    c=a-b;
;    printf("%s, a=%d, b=%d, c=%d\n","c=a-b", a, b, c);
;    c=a*b;
;    printf("%s, a=%d, b=%d, c=%d\n","c=a*b", a, b, c);
;    c=c/a;
;    printf("%s, a=%d, b=%d, c=%d\n","c=c/a", a, b, c);
;    return 0;
; }

        extern printf		; the C function to be called

%macro	pabc 1			; a "simple" print macro
	section .data
.str	db	%1,0		; %1 is first actual in macro call
	section .text
				; push onto stack backwards
	push	dword [c]	; int c
	push	dword [b]	; int b
	push	dword [a]	; int a
	push	dword .str 	; users string
        push    dword fmt       ; address of format string
        call    printf          ; Call C function
        add     esp,20          ; pop stack 5*4 bytes
%endmacro

	section .data  		; preset constants, writeable
a:	dd	3		; 32-bit variable a initialized to 3
b:	dd	4		; 32-bit variable b initializes to 4
fmt:    db "%s, a=%d, b=%d, c=%d",10,0	; format string for printf

	section .bss 		; unitialized space
c:	resd	1		; reserve a 32-bit word

	section .text		; instructions, code segment
	global	 main		; for gcc standard linking
main:				; label

lit5:				; c=5;
	mov	eax,5	 	; 5 is a literal constant
	mov	[c],eax		; store into c
	pabc	"c=5  "		; invoke the print macro

addb:				; c=a+b;
	mov	eax,[a]	 	; load a
	add	eax,[b]		; add b
	mov	[c],eax		; store into c
	pabc	"c=a+b"		; invoke the print macro

subb:				; c=a-b;
	mov	eax,[a]	 	; load a
	sub	eax,[b]		; subtract b
	mov	[c],eax		; store into c
	pabc	"c=a-b"		; invoke the print macro

mulb:				; c=a*b;
	mov	eax,[a]	 	; load a (must be eax for multiply)
	imul	dword [b]	; signed integer multiply by b
	mov	[c],eax		; store bottom half of product into c
	pabc	"c=a*b"		; invoke the print macro

diva:				; c=c/a;
	mov	eax,[c]	 	; load c
	mov	edx,0		; load upper half of dividend with zero
	idiv	dword [a]	; divide double register edx eax by a
	mov	[c],eax		; store quotient into c
	pabc	"c=c/a"		; invoke the print macro

        mov     eax,0           ; exit code, 0=normal
	ret			; main return to operating system



fltarith.asm simple floating point arithmetic
  The nasm source code is fltarith.asm
  The result of the assembly is fltarith.lst
  The equivalent "C" program is fltarith.c
  Running the program produces output fltarith.out

  This program demonstrates basic floating point add, subtract,
  multiply and divide.
  The equivalent "C" code is shown as comments in the assembly language.

; fltarith.asm   show some simple C code and corresponding nasm code
;                the nasm code is one sample, not unique
;
; compile  nasm -f elf -l fltarith.lst  fltarith.asm
; link     gcc -o fltarith  fltarith.o
; run      fltarith
;
; the output from running fltarith and fltarithc is:
; c=5.0, a=3.000000e+00, b=4.000000e+00, c=5.000000e+00
; c=a+b, a=3.000000e+00, b=4.000000e+00, c=7.000000e+00
; c=a-b, a=3.000000e+00, b=4.000000e+00, c=-1.000000e+00
; c=a*b, a=3.000000e+00, b=4.000000e+00, c=1.200000e+01
; c=c/a, a=3.000000e+00, b=4.000000e+00, c=4.000000e+00
;
;The file  fltarith.c  is:
;  #include
;  int main()
;  {
;    double a=3.0, b=4.0, c;
;
;    c=5.0;
;    printf("%s, a=%e, b=%e, c=%e\n","c=5.0", a, b, c);
;    c=a+b;
;    printf("%s, a=%e, b=%e, c=%e\n","c=a+b", a, b, c);
;    c=a-b;
;    printf("%s, a=%e, b=%e, c=%e\n","c=a-b", a, b, c);
;    c=a*b;
;    printf("%s, a=%e, b=%e, c=%e\n","c=a*b", a, b, c);
;    c=c/a;
;    printf("%s, a=%e, b=%e, c=%e\n","c=c/a", a, b, c);
;    return 0;
; }

        extern printf		; the C function to be called

%macro	pabc 1			; a "simple" print macro
	section	.data
.str	db	%1,0		; %1 is macro call first actual parameter
	section .text
				; push onto stack backwards
	push	dword [c+4]	; double c (bottom)
	push	dword [c]	; double c
	push	dword [b+4]	; double b (bottom)
	push	dword [b]	; double b
	push	dword [a+4]	; double a (bottom)
	push	dword [a]	; double a
	push	dword .str 	; users string
        push    dword fmt       ; address of format string
        call    printf          ; Call C function
        add     esp,32          ; pop stack 8*4 bytes
%endmacro

	section	.data  		; preset constants, writeable
a:	dq	3.333333333	; 64-bit variable a initialized to 3.0
b:	dq	4.444444444	; 64-bit variable b initializes to 4.0
five:	dq	5.0		; constant 5.0
fmt:    db "%s, a=%e, b=%e, c=%e",10,0	; format string for printf

	section .bss 		; unitialized space
c:	resq	1		; reserve a 64-bit word

	section .text		; instructions, code segment
	global	main		; for gcc standard linking
main:				; label

lit5:				; c=5.0;
	fld	qword [five]	; 5.0 constant
	fstp	qword [c]	; store into c
	pabc	"c=5.0"		; invoke the print macro

addb:				; c=a+b;
	fld	qword [a] 	; load a (pushed on flt pt stack, st0)
	fadd	qword [b]	; floating add b (to st0)
	fstp	qword [c]	; store into c (pop flt pt stack)
	pabc	"c=a+b"		; invoke the print macro

subb:				; c=a-b;
	fld	qword [a] 	; load a (pushed on flt pt stack, st0)
	fsub	qword [b]	; floating subtract b (to st0)
	fstp	qword [c]	; store into c (pop flt pt stack)
	pabc	"c=a-b"		; invoke the print macro

mulb:				; c=a*b;
	fld	qword [a]	; load a (pushed on flt pt stack, st0)
	fmul	qword [b]	; floating multiply by b (to st0)
	fstp	qword [c]	; store product into c (pop flt pt stack)
	pabc	"c=a*b"		; invoke the print macro

diva:				; c=c/a;
	fld	qword [c] 	; load c (pushed on flt pt stack, st0)
	fdiv	qword [a]	; floating divide by a (to st0)
	fstp	qword [c]	; store quotient into c (pop flt pt stack)
	pabc	"c=c/a"		; invoke the print macro

        mov     eax,0           ; exit code, 0=normal
	ret			; main returns to operating system


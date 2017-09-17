;This program takes a command line from the UART and runs its corresponding program.

; There is subroutines of PAULMON
; I'm use it for now
cout	EQU	30H
cin	EQU	32H
phex	EQU	34H
pstr	EQU	38H
newline	EQU	48H

maxstr	EQU	30		;Max lenth of command string
; There are a test records in xram
	ORG 0000H
DB	"-help", 0, 90H, 0
	ORG 0020H
DB	"-editor", 0, 18H, 40H
	ORG 0040H
DB	"-reset", 90H, 40H
	ORG 0060H
DB	"-dir", 0, 90H, 50H
	ORG 0080H
DB 	"print", 0, 90H, 60H
	ORG 00A0H
DB	"reboot", 0, 0H, 0H
	ORG 00C0H
DB	"12345678901234567890", 0, 90H, 80H
	ORG 00E0H
DB	"dirs", 0, 90H, 90H
	ORG 0100H
DB	"flush", 0, 90H, 0A0H
	ORG 0120H
DB	"test", 0, 90H, 0B0H
	ORG 0140H
DB	0
	
	ORG 8000H
	; The begining of program
start:
	mov a, #'>'
	lcall cout
	mov r0, #maxstr		;Input symbol counter
	mov r1, #40H		;Input string bufer start in IRAM
nxt:
	lcall cin
	lcall cout
	mov @r1, a
	inc r1
	cjne a, #0DH, far
	; If ENTER was pressed
	mov a, #maxstr
	subb a, r0
	mov r7, a
	; r7 = lenth(str)
	
	
	mov dptr, #0		; Begin of cmd table
srch:	
	mov r0, #40H		; First symbol of cmd
	mov r6, 07		; Copy lenth of cmd
srch1:
	movx a, @dptr		; Load char
	jz notfound		; If char is 0 - end of table
				; else
	mov r1, a		; r1 now has symbol from table
	mov a, @r0		; a now has symbol from bufer
	cjne a, 01, nextcmd	; if symbols (a & r1) are not equal
				; else, if equal then
	inc r0
	inc dptr
	djnz r6, srch1
	; There is all symbols of command is match! But programm may have more symbols, check this
	movx a, @dptr
	jnz nextcmd
	inc dptr
	movx a, @dptr		; Haddr
	mov r0, a
	inc dptr
	movx a, @dptr		; Laddr
	mov dpl, a
	mov a, r0
	mov dph, a
	clr a
	mov r0, a
	jmp @a+dptr     ; Run programm

nextcmd:
	anl dpl, #0E0H
	mov a, #32		; This code just add 32D to DPTR {
	add a, dpl
	mov dpl, a
	jnc srch
	inc dph	
	ajmp srch		;--}
	
notfound:
	lcall newline
	
	mov dptr, #err_uc
	lcall pstr
	lcall newline
	ajmp start
far:
	cjne a, #08H, far1
	lcall newline
	ajmp start
far1:
	djnz r0, nxt
	lcall newline
	mov dptr, #err_tl
	lcall pstr
	lcall newline
	ajmp start
	
err_tl:
DB	"Too long command string.", 0
err_uc:
DB	"Command or file not found.", 0
hlp:
DB	"", 0

; Just for test!
	ORG 9000H
	mov 0, #0AAH
	ajmp $
END

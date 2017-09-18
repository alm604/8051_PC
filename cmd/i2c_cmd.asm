;****************************************************************************
;*                        I2C Library for SBC P80C552EBA                    *
;*                   candidate to release version of x51 system             *
;*--------------------------------------------------------------------------*
;* (C) Alm604 14.09.17                                                      *
;****************************************************************************
;*
;*   
;* Register 18H is the I2C communication status register; useage is:
;*
;*     STATUS=DBY(18H)
;*      IF STATUS.AND.2=2 PRINT "Time out error!"
;*      IF STATUS.AND.4=4 PRINT "Busy error!"
;*      IF STATUS.AND.8=8 PRINT "No acknowlege error!"
;*
;*---------------------------------------------------------------------------
;*
;****************************************************************************

;----- Definitions ----------------------------------------------------------

SDA		bit	P1.7                    ;I2C serial data line.
SCL		bit	P1.6                    ;I2C serial clock line.

status		equ	018H			;Communication status.
cout		EQU	30H
cin		EQU	32H
phex		EQU	34H
pstr		EQU	38H
newline		EQU	48H
pint8u		EQU	4DH
phex16		EQU	36H
ghex16		EQU	3CH

maxstr		EQU	30		;Max lenth of command string
location	EQU	8000H
; the following bits will be set in the status byte:

tout		EQU	00000010B		;I2C time out status.
busy		EQU	00000100B		;I2C bus busy status.
nack		EQU	00001000B		;Slave sends no acknowledge.
	
		ORG location
DB     0A5H,0E5H,0E0H,0A5H     ;signiture bytes
DB     254,'X',080H,000H       ;id (254=cmd), starting address!!!
DB     0,0,0,0                 ;prompt code vector
DB     0,0,0,0                 ;TimeStamp Creating file YY,MM,DD,hh
DB     0,0,0,0                 ;Timestamp mm, ss, YY, MM
DB     0,0,0,0                 ;DD, hh, mm, ss
DB     0,0,0,0                 ;user defined
DB     255,255,255,255         ;length and checksum (255=unused)
DB     "Explorer",0
ORG    location+64         
	
	;clear ram
	mov dptr, #1FFFH
	
	clr a
cl_1:	
	movx @dptr, a
	djnz dpl, cl_1
	djnz dph, cl_1
cl_2:	
	movx @dptr, a 
	djnz dpl, cl_2
	
	; TODO load this by copy rom to ram!!!
	; save
	; reboot
	mov dptr, #0
	mov a, #'r'
	movx @dptr, a
	inc dptr
	mov a, #'e'
	movx @dptr, a
	inc dptr
	mov a, #'b'
	movx @dptr, a
	inc dptr
	mov a, #'o'
	movx @dptr, a
	inc dptr
	mov a, #'o'
	movx @dptr, a
	inc dptr
	mov a, #'t'
	movx @dptr, a
	inc dptr
	mov a, #0
	movx @dptr, a
	inc dptr
	mov a, #0
	movx @dptr, a
	inc dptr
	mov a, #0
	movx @dptr, a
	
	; help
	mov dptr, #20H
	mov a, #'h'
	movx @dptr, a
	inc dptr
	mov a, #'e'
	movx @dptr, a
	inc dptr
	mov a, #'l'
	movx @dptr, a
	inc dptr
	mov a, #'p'
	movx @dptr, a
	inc dptr
	mov a, #0
	movx @dptr, a
	inc dptr
	mov a, #HIGH help
	movx @dptr, a
	inc dptr
	mov a, #LOW help
	movx @dptr, a
	
	; ?
	mov dptr, #40H
	mov a, #'?'
	movx @dptr, a
	inc dptr
	mov a, #0
	movx @dptr, a
	inc dptr
	mov a, #HIGH help
	movx @dptr, a
	inc dptr
	mov a, #LOW help
	movx @dptr, a
	
	; diskinfo
	mov dptr, #60H
	mov a, #'d'
	movx @dptr, a
	inc dptr
	mov a, #'i'
	movx @dptr, a
	inc dptr
	mov a, #'s'
	movx @dptr, a
	inc dptr
	mov a, #'k'
	movx @dptr, a
	inc dptr
	mov a, #'i'
	movx @dptr, a
	inc dptr
	mov a, #'n'
	movx @dptr, a
	inc dptr
	mov a, #'f'
	movx @dptr, a
	inc dptr
	mov a, #'o'
	movx @dptr, a
	inc dptr
	mov a, #0
	movx @dptr, a
	inc dptr
	mov a, #HIGH diskinfo
	movx @dptr, a
	inc dptr
	mov a, #LOW diskinfo
	movx @dptr, a
	
	; dump
	mov dptr, #80H
	mov a, #'d'
	movx @dptr, a
	inc dptr
	mov a, #'u'
	movx @dptr, a
	inc dptr
	mov a, #'m'
	movx @dptr, a
	inc dptr
	mov a, #'p'
	movx @dptr, a
	inc dptr
	mov a, #0
	movx @dptr, a
	inc dptr
	mov a, #HIGH dump
	movx @dptr, a
	inc dptr
	mov a, #LOW dump
	movx @dptr, a
	
	; load
	mov dptr, #80H
	mov a, #'l'
	movx @dptr, a
	inc dptr
	mov a, #'o'
	movx @dptr, a
	inc dptr
	mov a, #'a'
	movx @dptr, a
	inc dptr
	mov a, #'d'
	movx @dptr, a
	inc dptr
	mov a, #0
	movx @dptr, a
	inc dptr
	mov a, #HIGH load
	movx @dptr, a
	inc dptr
	mov a, #LOW load
	movx @dptr, a
	; find a program on a disk
	acall findheader
	
	acall show_header
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
	
show_header:
	acall sh_0
	mov dptr, #hdr
	lcall pstr
	acall sh_0
	ret
sh_0:
	mov a, #'*'
	mov r0, #80
sh_1:
	lcall cout
	djnz r0, sh_1	
	ret
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
help:
	mov dptr, #hlp
	lcall pstr
	ajmp start
; Subroutine for scaning disk for free space
diskinfo:
	mov dptr, #dinf
	lcall pstr
	;TODO scan disk!!!
	mov a, #0FFH		; First sector (after incrementing)
	mov r2, #0		; Free sector counter
di_0:
	inc a
	push ACC
	acall checkfree
	jnc di_1
	inc r2
di_1:
	pop ACC
	cjne a, #127, di_0
	mov a, r2
	lcall newline
	lcall pint8u
	mov dptr, #dinf1
	lcall pstr
	ajmp start		

findheader:
	mov a, #0FFH		; First sector
fh_0:
	inc a
	push ACC
	acall find
	pop ACC
	cjne a, #128, fh_0	
	ret			; end of disk
; Subroutine for searching a file header on a sector (a - number of sector)
; OUT: if carry flag is set then file is present
find:
	clr C
	mov r1, a
	acall i2cstart
	mov a, #0A0H						;AT24C256 Slave address TODO add register for other EEPROM's!!!
	acall i2cput
	mov a, r1
	acall i2cput
	mov a, #0H
	acall i2cput
	acall i2cstart
	mov a, #0A1H
	acall i2cput
	acall i2cget
	cjne a, #0A5H, fh_1
	acall i2cget
	cjne a, #0E5H, fh_1
	acall i2cget
	cjne a, #0E0H, fh_1
	acall i2cget
	cjne a, #0A5H, fh_1
	; There is we found a file in EEPROM
	setb C
fh_1:	
	acall i2cstop
	ret
; Subroutine for check sector for 0xFFH. Number of sector in ACC
checkfree:
	clr C
	mov r0, #0	; Byte counter
	mov r1, a
	acall i2cstart
	mov a, #0A0H						;AT24C256 Slave address TODO add register for other EEPROM's!!!
	acall i2cput
	mov a, r1
	acall i2cput
	mov a, #0H
	acall i2cput
	acall i2cstart
	mov a, #0A1H
	acall i2cput
fr_0:	
	acall i2cget
	lcall cout
	cjne a, #0FFH, fr_1
	djnz r0, fr_0
	; There is all bytes in block is 0xFFH
	setb C
fr_1:	
	acall i2cstop
	ret
;===== i2cstart - sends an I2C start condition to beginn communication ======

i2cstart:	call	SCLhigh			;Set SCL to high.
		mov	R7,#4			;Load time out counter.
setSDA:		setb	SDA			;Set SDA to high.
		jb	SDA,ishigh		;If not high bus is busy.
		djnz	R7,setSDA		;If not try until R7 is zero.
		orl	status,#busy		;Set busy status.
		ret				;return to BASIC.

ishigh:		clr	SDA			;Set start condition.
		anl     status,#0		;Clear I2C status.
 		ret				;return to BASIC.

;===== i2cstop - sends an I2C stop condition to end communication ===========

i2cstop:	anl 	status,#0		;Clear I2C status.
		clr	SDA			;Get SDA ready for stop.
		acall 	SCLhigh			;Set clock for stop.
           	acall	delay			;Delay 4 machine cycles.
		setb	SDA			;Set stop condition.
		ret				;Return to BASIC.

;===== i2cput - sends a byte from a BASIC value out to the I2C bus ==========

;----- Get value and test for 8 bit only ------------------------------------

i2cput:	
;----- Send byte to I2C bus -------------------------------------------------
		;orl 	a, #080H
		push 	acc
		mov	R6,#8			;Load bit counter
send:		clr	SCL			;Make clock low
           	acall	delay			;Delay 4 machine cycles.
		rlc	A			;Rotate data bit to C.
		mov	SDA,C			;Put data bit on pin.
		acall	SCLhigh		        ;Send clock.
           	acall	delay			;Delay 4 machine cycles.
		djnz	R6,send			;Repeat until all bits sent.

;----- Read acknowledge from slave ------------------------------------------
	
		clr	SCL			;Make clock low.
           	acall	delay			;Delay 4 machine cycles.
		setb	SDA			;Release line for acknowledge.
		acall	SCLhigh		        ;Send clock for acknowlege.
	     	acall	delay			;Delay 4 machine cycles.
		jnb	SDA,ackok		;Check for valid acknowledge.
		orl	status,#nack		;Set no acknowledge status.
ackok:		clr	SCL			;Finish acknowledge bit.
		pop 	acc
		ret				;Return to BASIC.

;===== i2cget - Reads one byte from I2C bus to the argument stack ===========

i2cget:		
		mov	R6,#8			;Load bit counter
read:		
		clr	SCL			;Make clock low.
        	acall	delay			;Delay 4 machine cycles.
		acall	SCLhigh		        ;Send clock.
	   	acall	delay			;Delay 4 machine cycles.
		mov	C,SDA			;Get data bit from pin.
		rlc	A			;Rotate bit into result byte.
		djnz	R6,read			;Repeat until all received.

;----- Send acknowledge to slave --------------------------------------------
	
		clr	SCL			;Set clock low.
        	acall	delay			;Delay 4 machine cycles.
		clr SDA;mov	SDA,C			;send acknowledge bit.
		acall	SCLhigh		        ;Send acknowledge clock.
		acall	delay
		clr	SCL
		setb	SDA
		;acall	SCLhigh
;----- delay - generates a delay of 4 machine cycles ------------------------

delay:		ret				;4 cycles for CALL and RET.

;----- SCLhigh - sends SCL pin high and waits for any clock stretching ------

SCLhigh:	mov	R7,#4			;Load time out counter.
setSCL:	   	setb	SCL			;Set SCL to high.
	       	jb	SCL,quit		;If SCL actually high return.
		djnz	R7,setSCL		;If not try until R7 is zero.
		orl	status,#tout		;Set status time out.
quit:		ret

;----------------------------------------------------------------------------
load:
	mov dptr, #0
	acall 	i2cstart
	mov 	a, #0A0H						;AT24C256 Slave address TODO add register for other EEPROM's!!!
	acall 	i2cput
	mov 	a, #0
	acall 	i2cput
	mov 	a, #0
	acall 	i2cput
	acall 	i2cstart
	mov 	a, #0A1H
	acall 	i2cput	
load1:
	acall 	i2cget
	movx	@dptr, a
	inc 	dptr
	mov a, #080H
	cjne	a, dph, load1	
	acall 	i2cstop
	jmp 0
	
dump:	
	mov	dptr, #dmp
	lcall	pstr
	lcall	ghex16
	mov	r2, #16		;number of lines to print
	lcall	newline
	mov	r0, dpl
	mov	r1, dph
dump1:	lcall	phex16		;tell 'em the memory location
	mov	a,#':'
	lcall	cout
	mov 	a, #' '
	lcall 	cout
	mov	r3, #16		;r3 counts # of bytes to print
	;acall	r6r7todptr
	
	acall 	i2cstart
	mov 	a, #0A0H						;AT24C256 Slave address TODO add register for other EEPROM's!!!
	acall 	i2cput
	mov 	a, r1
	acall 	i2cput
	mov 	a, r0
	acall 	i2cput
	acall 	i2cstart
	mov 	a, #0A1H
	acall 	i2cput
dump2:	
	acall 	i2cget
	inc 	r0
	jnc	dump3
	inc 	r1
dump3:	lcall	phex		;print each byte in hex
	mov 	a, #' '
	lcall 	cout
	djnz	r3, dump2
	acall 	i2cstop
	
	lcall	newline
	djnz	r2, dump1	;loop back up to print next line
	lcall	newline
	ajmp	start

err_tl:
DB	"Too long command string.", 0
err_uc:
DB	"Command or file not found.", 0DH, 0AH, "Type 'help' or '?' for help.", 0
hlp:
DB	0DH, 0AH
DB	"'help' or '?' - view this help.", 0DH, 0AH
DB	"'save' - save file to disk.",  0DH, 0AH
DB	"'diskinfo' - information about disk.",  0DH, 0AH
DB	"'dump' - dump of one disk sector.",  0DH, 0AH
DB	"'load' - copy whole disk to xram.",  0DH, 0AH
DB	"'reboot' - reboot the PC.",  0DH, 0AH, 0DH, 0AH, 0
dinf:
DB	0DH, 0AH, "EEPROM disk on I2C bus.", 0DH, 0AH, "HW address - 0xA0h",  0DH, 0AH, "32768 bytes total.",  0DH, 0AH, "128 sectors by 256 bytes.",  0DH, 0AH, 0
dinf1:
DB	" sectors free of 128.", 0DH, 0AH, 0DH, 0AH, 0
hdr:
DB	"*                                     DOS                                      *", 0
dmp:
DB	0DH, 0AH, "Enter HEX address:", 0DH, 0AH, 0
		end


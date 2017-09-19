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

maxstr		EQU	14		;Max lenth of command string
location	EQU	0A000H
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
DB     "DOS",0
ORG    location+64         
	
;ram:
;0000 filename1 00 ADH ADL not_care
;0010 filename2 00 ADH ADL not_care
;0020 filename3 00 ADH ADL not_care
;0030 filename4 00 ADH ADL not_care
;0040 filename5 00 ADH ADL not_care
;0050 filename6 00 ADH ADL not_care
;0060 filename7 01 STR not_care		- prgm on disk (STR - sector number)
;0070 filename8 01 STR not_care
;0080 filename9 01 STR not_care
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
	
	mov dptr, #cmd_reboot
	acall ld
	mov dptr, #0
	acall sv
	mov dptr, #cmd_help
	acall ld
	mov dptr, #10H
	acall sv
	mov dptr, #cmd_?
	acall ld
	mov dptr, #20H
	acall sv
	mov dptr, #cmd_diskinfo
	acall ld
	mov dptr, #30H
	acall sv
	mov dptr, #cmd_dump
	acall ld
	mov dptr, #40H
	acall sv
	mov dptr, #cmd_load
	acall ld
	mov dptr, #50H
	acall sv
	mov dptr, #cmd_dir
	acall ld
	mov dptr, #60H
	acall sv
nextcmd		EQU	70H	;Change that if add cmds!!!
	; find a program on a disk
	acall dir_load
	acall show_header
	ajmp start
	
ld:	
	mov r1, #10H
	mov r0, #16
ld1:	
	movx a, @dptr
	mov @r1, a
	inc dptr
	inc r1
	djnz r0, ld1
	ret

sv:
	mov r1, #10H
	mov r0, #16
sv1:
	mov a, @r1
	movx @dptr, a
	inc r1
	inc dptr
	djnz r0, sv1
	ret	
;------------------------------------------------------	
dir:
	mov dptr, #dir_msg
	lcall pstr
	mov r1, #0
dir0:
	acall find
	
	mov a, r3
	jnz dir2
dir1:
	inc r1
	cjne r1, #128, dir0
	ajmp start
dir2:
	acall showfile
	ajmp dir1
;----------------------------	
dir_load:			; Load all program names to RAM
	mov dph, #0		; 0070 - first program record for now after embedded cmd's
	mov r2, #nextcmd
	mov r1, #0		; MSB I2C Address
dl0:
	acall find		; scan sector. Set r3 is one if header found
	
	mov a, r3		; copy r3
	jnz dl2			; if not zero then jump dl2
dl1:				; there we're if no header found
	inc r1			; increment MSB I2C address
	cjne r1, #128, dl0	; If it's not last address then jump dl0
	ret			; else return
dl2:				; There we're if header found
	acall dlfile		; Call move filename to iram
	push 01
	acall sv;ajmp $		; Save filename to xram
	pop 01
	mov a, #10H		;{
	add a, r2		; Increment r2 and dpl
	mov r2, a		;}
	jnz dl3
	inc dph
dl3:
	mov dpl, a
	
	ajmp dl1	
	
dlfile:
	mov r0, #10H
	acall i2cstart
	mov a, #0A0H						;AT24C256 Slave address TODO add register for other EEPROM's!!!
	acall i2cput
	mov a, r1
	acall i2cput
	mov a, #20H
	acall i2cput
	acall i2cstart
	mov a, #0A1H
	acall i2cput
dlf_0:
	acall i2cget
	jz dlf_1
	mov @r0, a
	inc r0
	
	ajmp dlf_0
dlf_1:	
	acall i2cstop
	mov @r0, #01H
	inc r0
	mov @r0, 1
	
	ret
;----------------------------	
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
	mov r3, #0	; flag
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
	mov r3, #1	; flag
fh_1:	
	acall i2cstop
	ret
	
	
showfile:
	acall i2cstart
	mov a, #0A0H						;AT24C256 Slave address TODO add register for other EEPROM's!!!
	acall i2cput
	mov a, r1
	acall i2cput
	mov a, #20H
	acall i2cput
	acall i2cstart
	mov a, #0A1H
	acall i2cput
sf_0:
	acall i2cget
	jz sf_1
	lcall cout
	ajmp sf_0
sf_1:	
	acall i2cstop
	lcall newline
	ret
;------------------------------------------------------	
load_and_run:
	ajmp start
;------------------------------------------------------
add_dptr:
	anl dpl, #0F0H
	mov a, #10H		; This code just add 32D to DPTR {
	add a, dpl
	mov dpl, a
	jnc srch
	inc dph	
	ajmp srch		;--}
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
	cjne a, 01, add_dptr	; if symbols (a & r1) are not equal
				; else, if equal then
	inc r0
	inc dptr
	djnz r6, srch1
	; There is all symbols of command is match! But programm may have more symbols, check this
	movx a, @dptr
	jnz srch2
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
srch2:
	dec a
	jnz add_dptr
	ajmp load_run

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


help:
	mov dptr, #hlp
	lcall pstr
	ajmp start

;----------------------------------------------------------------
; There is loading prg from disk and run it
load_run:
	inc dptr
	movx a, @dptr		; Sector number
	mov r1, a
	acall i2cstart
	mov a, #0A0H						;AT24C256 Slave address TODO add register for other EEPROM's!!!
	acall i2cput
	mov a, r1
	acall i2cput
	mov a, #06H
	acall i2cput
	acall i2cstart
	mov a, #0A1H
	acall i2cput
	acall i2cget
	; HADDR
	mov dph, a
	mov r3, a
	acall i2cget
	; LADDR
	mov dpl, a
	mov r2, a
	mov r0, #21
srch3:
	acall i2cget
	djnz r0, srch3
	mov r5, a	; HIGH size
	acall i2cget
	mov r4, a	; LOW size
	acall i2cstop
	; We have all parameters we need
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
srch4:
	acall i2cget
	movx @dptr, a
	inc dptr
	djnz r4, srch4
	djnz r5, srch4
	acall i2cstop
	
	mov dph, r3
	mov a, r2
	add a, #64
	mov dpl, r2
	clr a
	jmp @a+dptr
	ajmp start

;-----------------------------------------------------------------
; Subroutine for scaning disk for free space
diskinfo:
	mov dptr, #dinf
	lcall pstr
	mov a, #0FFH		; First sector (after incrementing)
	mov r2, #0		; Free sector counter
di_0:
	inc a
	push ACC
	acall checkfree
	mov a, r3
	jz di_1
	inc r2
di_1:
	pop ACC
	cjne a, #127, di_0
	mov a, r2
	lcall pint8u
	mov dptr, #dinf1
	lcall pstr
	ajmp start		


; Subroutine for check sector for 0xFFH. Number of sector in ACC
checkfree:
	mov r3, #0	; flag
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
	cjne a, #0FFH, fr_1
	djnz r0, fr_0
	; There is all bytes in block is 0xFFH
	mov r3, #1
fr_1:	
	acall i2cstop
	ret
;===== i2cstart - sends an I2C start condition to beginn communication ======

i2cstart:	jb	SDA,i2c_st
		clr	SCL
		setb	SDA
		setb	SCL
i2c_st:
		call	SCLhigh			;Set SCL to high.
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
	lcall	phex		;print each byte in hex
	inc 	r0
	mov 	a, r0
	jnz	dump3
	inc 	r1
dump3:			
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
dir_msg:
DB	0DH, 0AH, "Programs found on disk:", 0DH, 0AH, 0
dinf:
DB	0DH, 0AH, "EEPROM disk on I2C bus.", 0DH, 0AH, "HW address - 0xA0h",  0DH, 0AH, "32768 bytes total.",  0DH, 0AH, "128 sectors by 256 bytes.",  0DH, 0AH, 0
dinf1:
DB	" sectors free of 128.", 0DH, 0AH, 0DH, 0AH, 0
hdr:
DB	"*                                     DOS                                      *", 0
dmp:
DB	0DH, 0AH, "Enter HEX address:", 0DH, 0AH, 0
cmd_reboot:
DB	"reboot",0,0,0
cmd_help:
DB	"help",0,HIGH help,LOW help
cmd_?:
DB	"?",0,HIGH help,LOW help
cmd_diskinfo:
DB	"diskinfo",0,HIGH diskinfo,LOW diskinfo
cmd_dump:
DB	"dump",0,HIGH dump,LOW dump
cmd_load:
DB	"load",0,HIGH load,LOW load
cmd_dir:
DB	"dir",0,HIGH dir,LOW dir



		end


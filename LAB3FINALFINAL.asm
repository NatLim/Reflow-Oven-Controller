$MODLP52
org 0000H
   ljmp MainProgram

CLK  EQU 22118400
BAUD equ 115200
T1LOAD equ (0x100-(CLK/(16*BAUD)))
CE_ADC	EQU P2.0
MY_MOSI EQU P2.1
MY_MISO EQU P2.2
MY_SCLK EQU P2.3 
cseg

dseg at 0x30
Result: ds 2
x: ds 4
y: ds 4
bcd: ds 5
temp: ds 2

bseg
mf: dbit 1

$NOLIST
$include(math32.inc) ; A library of LCD related functions and utility macros
$LIST

CSEG

;LCD_RS equ P1.2
;LCD_RW equ P1.3
;LCD_E  equ P1.4
;LCD_D4 equ P3.2
;LCD_D5 equ P3.3
;LCD_D6 equ P3.4
;LCD_D7 equ P3.5 
;$NOLIST
;$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
;$LIST
; Configure the serial port and baud rate using timer 1
INIT_SPI:
	setb MY_MISO ; Make MISO an input pin
	clr MY_SCLK ; Mode 0,0 default
	ret
DO_SPI_G:
	push acc
	mov R1, #0 ; Received byte stored in R1
	mov R2, #8 ; Loop counter (8-bits)
DO_SPI_G_LOOP:
	mov a, R0 ; Byte to write is in R0
	rlc a ; Carry flag has bit to write
	mov R0, a
	mov MY_MOSI, c
	setb MY_SCLK ; Transmit
	mov c, MY_MISO ; Read received bit
	mov a, R1 ; Save received bit in R1
	rlc a
	mov R1, a
	clr MY_SCLK
	djnz R2, DO_SPI_G_LOOP
	pop acc
	ret

Wait_Milli_Seconds mac
	push AR2
	mov R2, %0
	lcall ?Wait_Milli_Seconds
	pop AR2
endmac

?Wait_Milli_Seconds:
	push AR0
	push AR1
L3: mov R1, #45
L2: mov R0, #166
L1: djnz R0, L1 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, L2 ; 22.51519us*45=1.013ms
    djnz R2, L3 ; number of millisecons to wait passed in R2
    pop AR1
    pop AR0
    ret

	
WaitHalfSec:
    mov R2, #89
Loop3: mov R1, #250
Loop2: mov R0, #166
Loop1: djnz R0, Loop1 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, Loop2 ; 22.51519us*250=5.629ms
    djnz R2, Loop3 ; 5.629ms*89=0.5s (approximately)
    ret
	
Get_BCD mac
    push ar0
    mov r0, %0
    lcall ?Get_BCD
    pop ar0
endmac

?Get_BCD:
    push acc
    ; Write most significant digit
    mov a, r0
    swap a
    anl a, #0fh
    orl a, #30h
    lcall putchar
    ; write least significant digit
    mov a, r0
    anl a, #0fh
    orl a, #30h
    lcall putchar
    pop acc
    ret

InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, or risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can safely proceed with the configuration
	clr	TR1
	anl	TMOD, #0x0f
	orl	TMOD, #0x20
	orl	PCON,#0x80
	mov	TH1,#T1LOAD
	mov	TL1,#T1LOAD
	setb TR1
	mov	SCON,#0x52
    ret

; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret
 
Hello_World:
    ;DB  '\n', 0
    DB  '\r\n', 0

MainProgram:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    mov PMOD, #0 ; Configure all ports in bidirectional mode
	lcall InitSerialPort
	lcall INIT_SPI

Forever:
	clr CE_ADC
	mov R0, #00000001B ; Start bit:1
	lcall DO_SPI_G
	mov R0, #10000000B ; Single ended, read channel 0
	lcall DO_SPI_G
	mov a, R1 ; R1 contains bits 8 and 9
	anl a, #00000011B ; We need only the two least significant bits
	mov Result+1, a ; Save result high.
	mov R0, #55H ; It doesn't matter what we transmit...
	lcall DO_SPI_G
	mov Result, R1 ; R1 contains bits 0 to 7. Save result low.
	setb CE_ADC

	;mov DPTR, #Hello_World
	;lcall SendString

	
	;lcall WaitHalfSec
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	
	mov x+3, #0
	mov x+2, #0
	mov x+1, Result+1
	mov x+0, Result
	
	load_y (5000000)
	lcall mul32
	load_y (1023*300*41)
	lcall div32
	load_y(28)
	lcall add32
	
	mov temp+0, x+0
	mov temp+1, x+1
	
	lcall hex2bcd
	
	Display_BCD(temp+1)
	Display_BCD(temp)
	Get_BCD(bcd+1)
	;mov a, #'.'
	;lcall putchar
	Get_BCD(bcd)
	
;	mov DPTR, #Hello_World
	lcall SendString
	
	ljmp Forever ; This is equivalent to 'forever: sjmp forever'
	
END

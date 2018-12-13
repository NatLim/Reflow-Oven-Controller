$MODF38X

CSEG at 0H
	ljmp main

Wait_half_second:	
	;For a 12MHz oscillator one machine cycle 
	;takes 1/12.0000MHz=83.333333ns
    mov R2, #25
L3:	mov R1, #250
L2:	mov R0, #240
L1:	djnz R0, L1 ; 4 machine cycles-> 4*83.3333ns*240=80us
    djnz R1, L2 ; 80us*250=0.02s
    djnz R2, L3 ; 0.02s*25=0.5s
    ret
    
main:
	anl	PCA0MD,#0xBF  ; DISABLE WDT: clear Watchdog Enable bit
	orl	OSCICN, #0x03 ; Configure internal oscillator for its maximum frequency
	mov SP, #7FH
	orl	P1SKIP,#0x04  ; skip LED pin in crossbar assignments
	mov	XBR1,#0x40    ; enable Crossbar
	orl	P1MDOUT,#0x08 ; make LED pin output push-pull
	orl	P1MDIN,#0x08  ; make LED pin input mode digital
M0:
	cpl P1_3 ; Led off/on
	lcall Wait_half_second
	sjmp M0
end

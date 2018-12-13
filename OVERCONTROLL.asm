$NOLIST
$MODLP52
$LIST


CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096   ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))
BAUD 		  EQU 115200
T1LOAD 	 	  EQU (0x100-(CLK/(16*BAUD)))
CE_ADC		  EQU P2.0
MY_MOSI 	  EQU P2.1
MY_MISO 	  EQU P2.2
MY_SCLK 	  EQU P2.3 

;PUSHBUTTON KEYS
PARAM_SELECT_BUTTON equ Px.x	;cycles between soak temp, soak time, reflow temp and reflow time
START/STOP_BUTTON	equ Px.x	;starts/stops reflow process
FEEDBACK_SPEAKER	equ Px.x
UP_BUTTON			equ Px.x	;increments paramter value
DOWN_BUTTON			equ Px.x	;decrements paramter value

; Reset vector
org 0000H
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0003H
	reti

; Timer/Counter 0 overflow interrupt vector
org 000BH
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0013H
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 001BH
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0023H 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 002BH
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30

Count1ms:     		ds 2 ; Used to determine when half second has passed
BCD_counter:  		ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
	
Result: ds 2
x: ds 4
y: ds 4
bcd: ds 5

stemp_var: ds 1
stime_var: ds 1
rtemp_var: ds 1
rtime_var: ds 1
screen_var: ds 1

bseg

half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
mf: dbit 1

$NOLIST
$include(math32.inc) ; A library of LCD related functions and utility macros
$LIST

CSEG

LCD_RS equ P1.2
LCD_RW equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5 
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the position of the counter
Initial_Message:  db 'SELECT PARAMETER' , 0
SOAK_TEMP_MSG  :  db 'SOAK TEMP:  xxxx' , 0
SOAK_TIME_MSG  :  db 'SOAK TIME:  xxxx' , 0
REFL_TEMP_MSG  :  db 'REFLOW TEMP:xxxx' , 0
REFL_TIME_MSG  :  db 'REFLOW TIME:xxxx' , 0
PRE_PROCESS	   :  db 'START?'			, 0
;PROCESS_1	   :  db 'Temp: xxx'		, 0
;PROCESS_2	   :  db 'Time: xxx'		, 0
PROCESS		   :  db 'Temp: xxx \n xxx' , 0


;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
    setb EA   ; Enable Global interrupts
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	; Define a latency correction for the timer reload
	CORRECTION EQU (4+4+2+2+4+4) ; lcall+ljmp+clr+mov+mov+setb
	; In mode 1 we need to reload the timer.	
	clr TR0
	mov TH0, #high(TIMER0_RELOAD+CORRECTION)
	mov TL0, #low(TIMER0_RELOAD+CORRECTION)	
	setb TR0
	cpl SOUND_OUT ; Connect speaker to P3.7!
	

	
Timer0_ISR_done:
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer.  Autoreload mode.
	; One millisecond interrupt
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Set the 16-bit variable Count1ms to zero
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
    setb EA   ; Enable Global interrupts
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
;	cpl TR0; This line makes a beep-silence-beep-silence sound
	; Reset the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, BCD_counter
	jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01
	sjmp Timer2_ISR_da
Timer2_ISR_decrement:
	add a, #0x99
Timer2_ISR_da:
	da a
	mov BCD_counter, a
	cjne a,#0x60,timer2_ISR_done
	mov BCD_counter, #0
	mov a,BCD_counter_min
	add a, #0x01
	da a
	mov BCD_counter_min,a
	cjne a,#0x60,timer2_ISR_done
	mov BCD_counter_min, #0
	mov a,BCD_counter_hour
	add a, #0x01
	da a
	mov BCD_counter_hour,a
	cjne a,#0x12,timer2_ISR_done
	mov BCD_counter_hour, #0
	mov a, AMPM
	add a, #0x01
	da a
	mov AMPM, a
	cjne a,#0x02,timer2_ISR_done
	mov AMPM, #0x00
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti

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

;InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, or risk displaying gibberish!
;    mov R1, #222
 ;   mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
 ;   djnz R1, $-4 ; 22.51519us*222=4.998ms
 ;   ; Now we can safely proceed with the configuration
;	clr	TR1
;	anl	TMOD, #0x0f
;	orl	TMOD, #0x20
;	orl	PCON,#0x80
;	mov	TH1,#T1LOAD
;	mov	TL1,#T1LOAD
;	setb TR1
;	mov	SCON,#0x52
 ;   ret

; Send a character using the serial port
;putchar:
;    jnb TI, putchar
;    clr TI.
;    mov SBUF, a
;    ret

; Send a constant-zero-terminated string using the serial port
;SendString:
;    clr A
;    movc A, @A+DPTR
;    jz SendStringDone
;    lcall putchar
;    inc DPTR
;    sjmp SendString
;SendStringDone:
;    ret
 
;Hello_World:
    ;DB  '\n', 0
;    DB  '\r\n', 0

B_CHECK MAC
	;B_CHECK(button,jump not pressed)
	jb %0, %1
	Wait_Milli_Seconds(#50)
	jb %0, %1
	jnb %0, $
	ENDMAC
			
	
;BEGIN:
;	Set_Cursor(1, 1)
 ;   Send_Constant_String(#Initial_Message)
	;SETB Px.x    					         ; Make port Px.x as input port (SELECT_PARAM)
  ; 	JB PARAM_SELECT_BUTTON, STEMP_SET		 ; check if button is pressed
  	;Wait_Milli_Seconds(#50)
;	JB PARAM_SELECT_BUTTON, STEMP_SET              	
 ;  	JNB PARAM_SELECT_BUTTON,$       		 ; Wait until the button is released
  ; 	SJMP BEGIN             				     ; do it continuously
	
BEGIN:
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
	B_CHECK(PARAM_SELECT_BUTTON,param)
B1: B_CHECK(UP_BUTTON,screen1ADD)
B2: B_CHECK(DOWN_BUTTON,screen1MIN)
B3: B_CHECK(START/STOP_BUTTON,REFLOW)
	sjmp BEGIN
	

param:
	mov a, screen_var
	add a, #0x01
	da a
	cjne a, #0x05, param_reset
	clr a
	mov screen_var, a
	ljmp B1
param_reset:
	mov screen_var, a
	ljmp B1	

  
screen1ADD:
	mov a, screen_var
	cjne a, #0x01, screen2ADD
	mov a, stemp_var
	add a, #0x01
	da a
	cjne a, #201, screen1ADDX
	clr a
	mov stemp_var, a
	ljmp B2
screen1ADDX:
	mov stemp_var, a
	ljmp B2

screen2ADD:
	mov a, screen_var
	cjne a, #0x02, screen3ADD
	mov a, stime_var
	add a, #0x01
	da a
	cjne a, #100, screen2ADDX
	clr a
	mov stime_var, a
	ljmp B2
screen2ADDX:
	mov stime_var, a
	ljmp B2	
screen3ADD:
	mov a, screen_var
	cjne a, #0x03, screen4ADD
	mov a, rtemp_var
	add a, #0x01
	da a
	cjne a, #201, screen3ADDX
	clr a
	mov rtemp_var, a
	ljmp B2
screen3ADDX:
	mov rtemp_var, a
	ljmp B2
	
screen4ADD:
	mov a, screen_var
	cjne a, #0x04, screen1ADD
	mov a, rtime_var
	add a, #0x01
	da a
	cjne a, #100, screen4ADDX
	clr a
	mov rtime_var, a
	ljmp B2
screen4ADDX:
	mov rtime_var, a
	ljmp B2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
screen1MIN:
	mov a, screen_var
	cjne a, #0x01, screen2MIN
	mov a, stemp_var
	subb a, #0x01
	da a
	cjne a, #0, screen1MINX
	mov a, #200
	mov stemp_var, a
	ljmp B3
screen1MINX:
	mov stemp_var, a
	ljmp B#

screen2MIN:
	mov a, screen_var
	cjne a, #0x02, screen3MIN
	mov a, stime_var
	subb a, #0x01
	da a
	cjne a, #0, screen2MINX
	mov a, #100
	mov stime_var, a
	ljmp B3
screen2MINX:
	mov stime_var, a
	ljmp B3	
screen3MIN:
	mov a, screen_var
	cjne a, #0x03, screen4MIN
	mov a, rtemp_var
	subb a, #0x01
	da a
	cjne a, #0, screen3MINX
	mov a, #200
	mov rtemp_var, a
	ljmp B3
screen3MINX:
	mov rtemp_var, a
	ljmp B3
	
screen4MIN:
	mov a, screen_var
	cjne a, #0x04, screen1MIN
	mov a, rtime_var
	subb a, #0x01
	da a
	cjne a, #0, screen4MINX
	mov a, #100
	mov rtime_var, a
	ljmp B3
screen4MINX:
	mov rtime_var, a
	ljmp B3

main:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    mov PMOD, #0 ; Configure all ports in bidirectional mode
	lcall InitSerialPort
	lcall INIT_SPI
	lcall Timer0_Init
    lcall Timer2_Init
    lcall LCD_4BIT
	lcall BEGIN
	setb half_seconds_flag
	mov BCD_counter, #0x58
	
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
;	Wait_Milli_Seconds(#50)
;	
;	mov x+3, #0
;	mov x+2, #0
;	mov x+1, Result+1
;	mov x+0, Result
;	
;	load_y (499)
;	lcall mul32
;	load_y (1023)
;	lcall div32
;	lcall hex2bcd
;	
;	Get_BCD(bcd+1)
;	;mov a, #'.'
;	;lcall putchar
;	Get_BCD(bcd)
;	
;	mov DPTR, #Hello_World
;m	lcall SendString
	
	sjmp Forever ; This is equivalent to 'forever: sjmp forever'
	
END

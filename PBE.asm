; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
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

BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P3.7
UP_BUTTON			equ P2.6	;increments paramter value
DOWN				equ P2.5
START_STOP_BUTTON	equ P0.4
PARAM_SELECT_BUTTON equ P2.4

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
;	ljmp ALARM_ISR
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
Result:		    ds 2
x: 				ds 4
y: 				ds 4
bcd: 			ds 5
state: 			ds 1

hour:		  ds 1
a_minute:	  ds 1
a_hour:		  ds 1
second:		  ds 1 
minute:		  ds 1
stemp_var: 	  ds 2
stime_var:    ds 2
rtemp_var:    ds 2
rtime_var:    ds 2
screen_var:   ds 2
temp:	  	  ds 2
SHIGH:		  ds 1
SLOW:		  ds 1


; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
on_off: dbit 1
mf: dbit 1

cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.2
LCD_RW equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$include(math32.inc)
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'SELECT PARAMETER' , 0
SOAK_TEMP_MSG  :  db 'SOAK TEMP: xxx C' , 0
SOAK_TIME_MSG  :  db 'SOAK TIME: xxx S' , 0
REFL_TEMP_MSG  :  db 'REFLOW TEMP:xxxx' , 0
REFL_TIME_MSG  :  db 'REFLOW TIME:xxxx' , 0
PRE_PROCESS	   :  db 'START?'			, 0
PROCESS	  	   :  db 'Time: x:xx	  ' , 0
PROCESS_2	   :  db 'Temp: xxx C  xx ' , 0
MESSAGE2:		  db 'INITIALIZING    ', 0
Alarm_Msg:		  db '  ALARM ALARM  ', 0
Alarm_Msg2:		  db '  PRESS BOOT!  ', 0

;ExtInt_Init:
;	setb EX0
;	setb IT0	;enable external interrupt
;	clr PX0
;	;clr IE0
;	ret
	
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
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	; In mode 1 we need to reload the timer.
	clr TR0
	mov TH0, SHIGH
	mov TL0, SLOW
	setb TR0
	cpl SOUND_OUT ; Connect speaker to P3.7!
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	;cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, BCD_counter
	mov a, second
	add a, #0x01
	da a
	cjne a, #0x60, Timer2_ISR_da
	clr a
	mov second, a
	mov a, minute
	add a, #0x01
	da a
	cjne a, #0x60, Timer2_ISR_minute
	clr a
	mov minute, a
	mov a, hour
	add a, #0x01
	da a
	cjne a, #0x12, aaaa
;	cpl am_flag
aaaa:
	cjne a, #0x13, Timer2_ISR_hour
	mov a, #0x01
	mov hour, a
	clr a
	mov minute, a
	mov second, a
	sjmp Timer2_ISR_done

Timer2_ISR_da:
	clr c
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a
	mov second, a
	sjmp Timer2_ISR_done
	
Timer2_ISR_minute:
	clr c
	da a
	mov minute, a
	sjmp Timer2_ISR_done

Timer2_ISR_hour:
	clr c
	da a
	mov hour, a
	sjmp Timer2_ISR_done
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti


;---------------------------------;
; ISR for setting alarm           ;
;---------------------------------;
;ALARM_ISR:
;	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
;	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
;	
	; The two registers used in the ISR must be saved in the stack
;	push acc
;	push psw
;	mov a, temp
;	add a, #0x01
;	da a
;	mov temp, a
;	pop acc
;	reti
	
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    mov PMOD, #0 ; Configure all ports in bidirectional mode
    lcall Timer0_Init
    lcall Timer2_Init
;	lcall ExtInt_Init
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    Set_cursor(2, 1)
	Send_Constant_String(#MESSAGE2)
	clr on_off
	mov BCD_counter, #0x00
	mov second, #0x00
	mov minute, #0x00
	mov hour, #0x00
	mov state, #0x00
	mov screen_var, #0x00
	mov stemp_var, #150
	mov stime_var, #60
	mov rtemp_var, #220
	mov rtime_var, #45
	clr TR0
	
	; After initialization the program stays in this 'forever' loop
loop:
;	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
;	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
;	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
;	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
;	clr TR2                 ; Stop timer 2
;	clr a
;	mov Count1ms+0, a
;	mov Count1ms+1, a
	; Now clear the BCD counter
;	mov BCD_counter, a
;	mov second, a
;	setb TR2                ; Start timer 2

	B_CHECK(PARAM_SELECT_BUTTON, screen1ADD)
	mov a, screen_var
	add a, #0x01
	
	cjne a, #0x05, param_reset
	clr a
	mov screen_var, a
	ljmp loop_b
param_reset:
	mov screen_var, a
	ljmp loop_b	

screen1ADD:
	B_CHECK(UP_BUTTON, screen1MIN)
	mov a, screen_var
	cjne a, #0x01, screen2ADD
	mov a, stemp_var
	add a, #0x01
	da a
	cjne a, #201, screen1ADDX
	clr a
	mov stemp_var, a
	ljmp loop_b
screen1ADDX:
	mov stemp_var, a
	ljmp loop_b

screen2ADD:
	clr c
	mov a, screen_var
	cjne a, #0x02, screen3ADD
	mov a, stime_var
	add a, #0x01
	da a
	cjne a, #100, screen2ADDX
	clr a
	mov stime_var, a
	ljmp loop_b
screen2ADDX:
	mov stime_var, a
	ljmp loop_b	
screen3ADD:
	clr c
	mov a, screen_var
	cjne a, #0x03, screen4ADD
	mov a, rtemp_var
	add a, #0x01
	da a
	cjne a, #201, screen3ADDX
	clr a
	mov rtemp_var, a
	ljmp loop_b
screen3ADDX:
	mov rtemp_var, a
	ljmp loop_b
	
screen4ADD:
	clr c
	mov a, screen_var
	cjne a, #0x04, screen1ADD
	mov a, rtime_var
	add a, #0x01
	da a
	cjne a, #100, screen4ADDX
	clr a
	mov rtime_var, a
	ljmp loop_b
screen4ADDX:
	mov rtime_var, a
	ljmp loop_b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

screen1MIN:
	B_CHECK(DOWN, REFLOW)
	mov a, screen_var
	cjne a, #0x01, screen2MIN
	clr c
	mov a, stemp_var
	add a, #0x99
	da a
	cjne a, #1, screen1MINX
	mov a, #200
	mov stemp_var, a
	ljmp loop_b
screen1MINX:
	clr c
	mov stemp_var, a
	ljmp loop_b

screen2MIN:
	clr c
	mov a, screen_var
	cjne a, #0x02, screen3MIN
	mov a, stime_var
	add a, #0x99
	da a
	cjne a, #0, screen2MINX
	mov a, #100
	mov stime_var, a
	ljmp loop_b
screen2MINX:
	mov stime_var, a
	ljmp loop_b	
screen3MIN:
	clr c
	mov a, screen_var
	cjne a, #0x03, screen4MIN
	mov a, rtemp_var
	add a, #0x99
	da a
	cjne a, #0, screen3MINX
	mov a, #200
	mov rtemp_var, a
	ljmp loop_b
screen3MINX:
	mov rtemp_var, a
	ljmp loop_b
	
screen4MIN:
	clr c
	mov a, screen_var
	cjne a, #0x04, screen1MIN
	mov a, rtime_var
	add a, #0x99
	da a
	cjne a, #0, screen4MINX
	mov a, #100
	mov rtime_var, a
	ljmp loop_b
screen4MINX:
	mov rtime_var, a
	ljmp loop_b

REFLOW:
	B_CHECK(START_STOP_BUTTON, loop_a)
	setb on_off
	jmp loop_b
	
	
loop_a:
	jnb half_seconds_flag, jump
	sjmp loop_b
jump:
	ljmp loop
	

loop_b:
	clr c
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	;Set_Cursor(1, 14)     ; the place in the LCD where we want the BCD counter value
	;Display_BCD(BCD_counter) ; This macro is also in 'LCD_4bit.inc'w3
	Set_Cursor(2,1)
	Send_Constant_String(#PROCESS_2)
	Set_Cursor(2, 7)
	Display_BCD(temp)
	Set_Cursor(2, 14)
	Display_BCD(screen_var)
	
d_PROC: 
    mov a, screen_var
	cjne a, #0x00, d_STE
	Set_Cursor(1, 1)
	Send_Constant_String(#PROCESS)
	Set_Cursor(1, 7)
	Display_BCD(minute)
	Set_Cursor(1, 10)
	Display_BCD(second)	
	ljmp SMACH
d_STE:
	mov a, screen_var
	cjne a, #0x01, d_STI
	Set_Cursor(1, 1)
	Send_Constant_String(#SOAK_TEMP_MSG)
	Set_Cursor(1, 12)
	Display_BCD(stemp_var+1)
	Display_BCD(stemp_var)
	ljmp SMACH
d_STI:
mov a, screen_var
	cjne a, #0x02, d_RTE
	Set_Cursor(1, 1)
	Send_Constant_String(#SOAK_TIME_MSG)
	Set_Cursor(1, 12)
	Display_BCD(stime_var)
	
	ljmp SMACH
d_RTE:
mov a, screen_var
	cjne a, #0x03, d_RTI
	Set_Cursor(1, 1)
	Send_Constant_String(#REFL_TEMP_MSG)
	Set_Cursor(1, 13)
	Display_BCD(rtemp_var)
	ljmp SMACH
	
d_RTI:	
mov a, screen_var
	cjne a, #0x04, SMACH
	Set_Cursor(1, 1)
	Send_Constant_String(#REFL_TIME_MSG)
	Set_Cursor(1, 13)
	Display_BCD(rtime_var)
	ljmp SMACH
SMACH:
done:
    ljmp loop
END
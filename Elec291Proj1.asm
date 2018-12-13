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
PARAM_SELECT_BUTTON equ P2.4	;cycles between soak temp, soak time, reflow temp and reflow time
START_STOP_BUTTON	equ P2.6	;starts/stops reflow process
;FEEDBACK_SPEAKER	equ P2.6
UP_BUTTON			equ P2.5	;increments paramter value
DOWN				equ P2.7
BOOT_BUTTON  		 equ P4.5
SOUND_OUT    		 equ P3.7
LED_1				equ P0.2
oven_on				equ P0.3
LED_2				equ P0.4
LED_3				equ P0.5
LED_4				equ P0.6
LED_5				equ P0.7
LOCK_KEY			equ P0.0


;oven_on				equ P3.6

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
	
Result:		    ds 2
x: 				ds 4
y: 				ds 4
bcd: 			ds 5
state: 			ds 1

hour:		  ds 1
c_minute:	  ds 1
c_second:	  ds 1
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
sec: 		  ds 1
pwm1:		  ds 1
pwm2:		  ds 1
pwmx:		  ds 1 
pwmx2:		  ds 1 
six:		  ds 1
tempfaren:    ds 2
temp_old:	  ds 2
rate:		  ds 2

bseg

half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
mf: dbit 1
on_off: dbit 1
flag1: dbit 1
flag2: dbit 1
flag3: dbit 1
flagL1: dbit 1
flagL2: dbit 1
flagL3: dbit 1
flagL4: dbit 1 
flagL5: dbit 1 
cf:		dbit 1
pm:		dbit 1
lockf:	dbit 1
doorf:  dbit 1

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
Initial_Message:  db 'ReflowOvenCtrllr' , 0
SOAK_TEMP_MSG  :  db 'SoakTemp: xxxx C' , 0
SOAK_TIME_MSG  :  db 'SoakTime: xx   S' , 0
REFL_TEMP_MSG  :  db 'ReflowTemp:xxxxC' , 0
REFL_TIME_MSG  :  db 'ReflowTime: xx S' , 0
COMPLETE_MSG   :  db 'Process complete'	, 0
PROCESS	  	   :  db 'Time: xx:xx     ' , 0
PROCESS_2	   :  db 'Temp: xxxx      ' , 0
MESSAGE2:		  db 'Version 1.1.42b ', 0
Alarm_Msg:		  db 'Wire not in Oven', 0
Alarm_Msg2:		  db 'Process Aborted!', 0
Alarm_Msg3:		  db 'OVEN OVERHEAT!!!', 0
OPEN_DOOR:		  db 'Open Oven Door..', 0
NAME1:			  db 'Nat Limapichat  ', 0
NAME2:			  db 'Arslan Bhatti   ', 0
NAME3:			  db 'Mike Yuan       ', 0
NAME4:			  db 'Intishar Islam  ', 0
NAME5:			  db 'Hamza Ahmed     ', 0
NAME6:			  db 'Cem Kaspi       ', 0
NAME7:			  db 'Feb 16, 2016    ', 0
GROUP_NAME:		  db 'ELEC 291 Team AE', 0
LOCK_MSG:		  db 'LOCK' , 0
BUFFER:
	DB   '\r', '\n', 0


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
	;cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
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
	;lcall read_tempf
	mov temp_old+0, temp+0
	mov temp_old+1, temp+1
	lcall read_temp
	lcall deriv
	mov a, sec
	add a, #0x01
	da a
	mov sec, a
	lcall oven_func
	lcall led_func
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
;	mov a, hour
;	add a, #0x01
;	da a
;	cjne a, #0x12, aaaa
;	cpl am_flag
;aaaa:
;	cjne a, #0x13, Timer2_ISR_hour
;	mov a, #0x01
;	mov hour, a
;	clr a
;	mov minute, a
;	mov second, a
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

;Timer2_ISR_hour:
;	clr c
;	da a
;	mov hour, a
;	sjmp Timer2_ISR_done
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti

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
	
read_temp:
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

;	Wait_Milli_Seconds(#250)
;	Wait_Milli_Seconds(#250)
	
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
	
;	lcall hex2bcd
	
	mov temp+0, bcd+0
	mov temp+1, bcd+1
	
	lcall hex2bcd
	
	;Display_BCD(temp+1)
	;Display_BCD(temp)
	Send_BCD(bcd+1)
	Send_BCD(bcd)
	mov DPTR, #BUFFER
    lcall SendString
	
;	lcall hex2bcd
	ret

conv_temp:

	mov a, temp+0
	addc a, #low(0x273)
	da a
	mov tempfaren, a
	mov a, tempfaren
	mov a, temp+1 
	addc a, #high(0x273)
	da a
	mov tempfaren+1, a
	
	ret

door_check:
	mov a, rate+0
	subb a, #0x09
	clr doorf
	jnc DDDD
	setb doorf
DDDD:
	clr c
	
	jb doorf, DDDD1
	Set_Cursor(1, 13)
	Display_char(#'D')
DDDD1:  
	jnb doorf, DDDD2
	Set_Cursor(1, 13)
	Display_char(#' ')
DDDD2:	
	
	ret
	
deriv:
	clr c
	mov a, temp+0
	subb a, temp_old+0
	da a
	mov rate+0, a
	mov a, temp+1
	subb a, temp_old+1
	mov rate+1, a
	setb pm
	jnc derivx2
	clr c
	mov a, temp_old+0
	subb a, temp+0
	da a
	mov rate+0, a
	clr pm
derivx2:
	clr c
	ret
	
oven_func:
	clr a
	
	jb flag3, contt3
	jb flag2, LL2
	jb flag1, LL1
	
	mov pwmx, pwm1
	mov pwmx2, pwm2
	clr oven_on
	setb flag1
LL1:
	mov a, pwmx 
	jnz contt
	setb flag2
	setb oven_on
LL2:
	mov a, pwmx2
	jnz contt2
	clr flag1
	clr flag2
Contt:
	subb a, #0x01
	mov pwmx, a
	sjmp contt3
contt2:
	subb a, #0x01 
	mov pwmx2, a
contt3:
	ret
	
WaitHalfSec:
    mov R2, #170
Loop3: mov R1, #250
Loop2: mov R0, #166
Loop1: djnz R0, Loop1 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, Loop2 ; 22.51519us*250=5.629ms
    djnz R2, Loop3 ; 5.629ms*89=0.5s (approximately)
    ret	
	
Lock_Check:
	jb lockf, LLLL
	Set_Cursor(1, 14)
	Display_char(#' ')
LLLL:  
	jnb lockf, LLLL1
	Set_Cursor(1, 14)
	Display_char(#'L')
LLLL1:	
	ret

WireCheck:
	mov a, #0x60
	clr c
	subb a, sec
	jnc S1
	clr c
	mov a, temp+0
	subb a, #low(0x50)
	mov a, temp+1
	subb a, #high(0x50)
	jnc S1
	mov c_second, second
	mov c_minute, minute
	mov state, #0x0
	clr on_off
	Set_Cursor(1,1)
	Send_Constant_String(#Alarm_Msg)
	Set_Cursor(2,1)
	Send_Constant_String(#Alarm_Msg2)
	lcall abort_beep
	lcall abort_screen	
S1:	
	ret
	
OVERHEATcheck:
	mov a, #low(0x280)
	subb a, temp+0
	mov a, #high(0x280)
	subb a, temp+1
	jnc N1
	mov c_second, second
	mov c_minute, minute
	mov state, #0x0
	clr on_off
	Set_Cursor(1,1)
	Send_Constant_String(#Alarm_Msg3)
	Set_Cursor(2,1)
	Send_Constant_String(#Alarm_Msg2)
	lcall abort_beep
	lcall overheat_screen	
N1:
	ret
	
	
start_BEEP:
	mov SHIGH, #208
	mov SLOW, #249
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0
	mov SHIGH, #211
	mov SLOW, #156
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0

	mov SHIGH, #208
	mov SLOW, #249
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0
	mov SHIGH, #211
	mov SLOW, #156
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0	
	
	mov SHIGH, #216
	mov SLOW, #116
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	clr TR0
	ret
	
abort_beep:
	mov SHIGH, #216
	mov SLOW, #116
	cpl TR0
	Wait_Milli_Seconds(#100)
	cpl TR0
	Wait_Milli_Seconds(#50)
	cpl TR0
	mov SHIGH, #206
	mov SLOW, #45
	cpl TR0
	Wait_Milli_Seconds(#100)
	cpl TR0
	Wait_Milli_Seconds(#50)
	cpl TR0
	B_CHECK(BOOT_BUTTON,abort_beep)
	clr TR0
	ret
	
overheat_screen:
	Set_Cursor(1,1)
	Send_Constant_String(#Alarm_Msg3)
	Set_Cursor(2,1)
	Send_Constant_String(#PROCESS)
	Set_Cursor(2, 7)
	Display_BCD(c_minute)
	Set_Cursor(2, 10)
	Display_BCD(c_second)	
NN1:B_CHECK(BOOT_BUTTON,NN1)
	setb LED_1
	setb LED_3
	setb LED_2
	setb LED_4
	setb LED_5
	ret
	
complete_beep:
	mov SHIGH, #208
	mov SLOW, #249
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0	
	
	mov SHIGH, #193
	mov SLOW, #57
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0	
	mov SHIGH, #161
	mov SLOW, #240
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0	
	mov SHIGH, #181
	mov SLOW, #90
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0	
	ret
	
door_beep:
	Set_Cursor(1,1)
	Send_Constant_String(#OPEN_DOOR)
	mov SHIGH, #216
	mov SLOW, #116
	cpl TR0
SS3:B_CHECK(BOOT_BUTTON,SS3)
	clr TR0
	ret
	
six_beep:
	mov SHIGH, #211
	mov SLOW, #156
	cpl TR0
	Wait_Milli_Seconds(#80)
	cpl TR0
	Wait_Milli_Seconds(#50)
	djnz six, six_beep
	clr TR0
	ret

tran_beep:
	mov SHIGH, #196
	mov SLOW, #192
	cpl TR0
	Wait_Milli_Seconds(#100)
	cpl TR0
	Wait_Milli_Seconds(#50)
	clr TR0
	ret
	
abort_screen:
	Set_Cursor(1,1)
	Send_Constant_String(#Alarm_Msg2)
	Set_Cursor(2,1)
	Send_Constant_String(#PROCESS)
	Set_Cursor(2, 7)
	Display_BCD(c_minute)
	Set_Cursor(2, 10)
	Display_BCD(c_second)	
SS1:B_CHECK(BOOT_BUTTON,SS1)
	setb LED_1
	setb LED_3
	setb LED_2
	setb LED_4
	setb LED_5
	ret
	
complete_screen:	

	Set_Cursor(1,1)
	Send_Constant_String(#COMPLETE_MSG)
	Set_Cursor(2,1)
	Send_Constant_String(#PROCESS)
	Set_Cursor(2, 7)
	Display_BCD(c_minute)
	Set_Cursor(2, 10)
	Display_BCD(c_second)	
	lcall complete_beep
SSS1:B_CHECK(PARAM_SELECT_BUTTON,SS2)
	lcall CREDIT
SS2:B_CHECK(BOOT_BUTTON,SSS1)
	setb LED_1
	setb LED_3
	setb LED_2
	setb LED_4
	setb LED_5
	ret

led_func:
	jnb flagL1, f1
	cpl LED_1
f1: jnb flagL2, f2
	cpl LED_2
f2: jnb flagL3, f3
	cpl LED_3
f3: jnb flagL4, f4
	cpl LED_4
f4: jnb flagL5, f5
	cpl LED_5
f5: 
	ret
	
CREDIT:
	Set_Cursor(2,1)
	Send_Constant_String(#GROUP_NAME)
	Set_Cursor(1,1)
	Send_Constant_String(#NAME1)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Set_Cursor(1,1)
	Send_Constant_String(#NAME2)
	Wait_Milli_Seconds(#250)
	Set_Cursor(1,1)
	Send_Constant_String(#NAME3)
	Wait_Milli_Seconds(#250)
	Set_Cursor(1,1)
	Send_Constant_String(#NAME4)
	Wait_Milli_Seconds(#250)
	Set_Cursor(1,1)
	Send_Constant_String(#NAME5)
	Wait_Milli_Seconds(#250)
	Set_Cursor(1,1)
	Send_Constant_String(#NAME6)
	Wait_Milli_Seconds(#250)
	Set_Cursor(1,1)
	Send_Constant_String(#NAME7)
	Wait_Milli_Seconds(#250)
	ret
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
	lcall InitSerialPort
	lcall INIT_SPI
;	lcall ExtInt_Init
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':

    setb half_seconds_flag
	mov BCD_counter, #0x00
	mov second, #0x00
	mov minute, #0x00
	mov state, #0x00
	mov screen_var, #0x00
	mov stemp_var+0, #low(0x150)
	mov stemp_var+1, #high(0x150)
	mov stime_var+0, #0x60
	mov rtemp_var+0, #low(0x220)
	mov rtemp_var+1, #high(0x220)
	mov rtime_var, #0x45
	mov state, #0x0
	mov sec, #0x0
	mov six, #0x06
	mov temp, #0x0
	mov tempfaren, #0x0
	clr flagL1
	clr flagL2
	clr flagL3
	clr flagL4
	clr flagL5
	clr cf
	clr lockf
	setb pm
	setb oven_on
	clr on_off
	clr TR0
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    Set_cursor(2, 1)
	Send_Constant_String(#MESSAGE2)
	Wait_Milli_Seconds(#250)
	clr LED_1
	clr LED_5
	Wait_Milli_Seconds(#250)
	clr LED_2
	clr LED_4
	Wait_Milli_Seconds(#250)
	clr LED_3
	Wait_Milli_Seconds(#250)
	
	setb LED_1
	setb LED_3
	setb LED_2
	setb LED_4
	setb LED_5
	setb flag1
	setb flag2
	setb flag3
	
	; After initialization the program stays in this 'forever' loop
loop:
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

REFLOW_JUMP:
	ljmp REFLOW
	
screen1ADD:
	jb lockf, REFLOW_JUMP
	B_CHECK(UP_BUTTON, screen1MIN)
	mov a, screen_var
	cjne a, #0x01, screen2ADD
	mov a, stemp_var+0
	add a, #0x01
	da a
	mov stemp_var+0, a
	mov a, stemp_var+1
	addc a, #0
	mov stemp_var+1, a
;	da a
;	cjne a, #201, screen1ADDX
;	clr a
;	mov stemp_var, a
;	ljmp loop_b
;screen1ADDX:
;	mov stemp_var, #low(s)
;	mov stemp_var+1,#high(a)
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
	mov a, rtemp_var+0
	add a, #0x01
	da a
	mov rtemp_var+0, a
	mov a, rtemp_var+1
	addc a, #0
	mov rtemp_var+1, a
;	cjne a, #201, screen3ADDX
;	clr a
;	mov rtemp_var, a
;	ljmp loop_b
;screen3ADDX:
;	mov rtemp_var, a
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
	B_CHECK(START_STOP_BUTTON, FarenCel)
	cpl on_off
	jmp loop_b
	
FarenCel:
	B_CHECK(BOOT_BUTTON, lockcheck)
	cpl cf
	jmp loop_b
	
lockcheck:
	B_CHECK(LOCK_KEY, loop_a)
	cpl lockf
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
	Set_Cursor(2,15)
	;Display_BCD(rate+1)
	Display_BCD(rate)
	jb pm, PPPP
	Set_Cursor(2, 14)
	Display_char(#'-')
PPPP:  
	jnb pm, PPPP1
	Set_Cursor(2, 14)
	Display_char(#'+')
PPPP1:	
;	Set_Cursor(2, 14)
;	Display_BCD(screen_var)
	jb cf, faren
	Set_Cursor(2, 7)
	Display_BCD(temp+1)
	Display_BCD(temp)
	Set_Cursor(2, 12)
	Display_char(#'C')
faren:  
	jnb cf, cel
	lcall conv_temp
	Set_Cursor(2, 7)
	Display_BCD(tempfaren+1)
	Display_BCD(tempfaren)
	Set_Cursor(2, 12)
	Display_char(#'K')
cel:
	
d_PROC: 
    mov a, screen_var
	cjne a, #0x00, d_STE
	Set_Cursor(1, 1)
	Send_Constant_String(#PROCESS)
	Set_Cursor(1, 7)
	Display_BCD(minute)
	Set_Cursor(1, 10)
	Display_BCD(second)	
	Set_Cursor(1,15)
	Display_BCD(state)
	lcall Lock_Check
	lcall door_check
	ljmp SMACH
d_STE:
	mov a, screen_var
	cjne a, #0x01, d_STI
	Set_Cursor(1, 1)
	Send_Constant_String(#SOAK_TEMP_MSG)
	Set_Cursor(1, 11)
	Display_BCD(stemp_var+1)
	Display_BCD(stemp_var)
	ljmp SMACH
d_STI:
mov a, screen_var
	cjne a, #0x02, d_RTE
	Set_Cursor(1, 1)
	Send_Constant_String(#SOAK_TIME_MSG)
	Set_Cursor(1, 11)
	Display_BCD(stime_var)
	
	ljmp SMACH
d_RTE:
mov a, screen_var
	cjne a, #0x03, d_RTI
	Set_Cursor(1, 1)
	Send_Constant_String(#REFL_TEMP_MSG)
	Set_Cursor(1, 12)
	Display_BCD(rtemp_var+1)
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
	mov a, state
state0:
	cjne a, #0, state1
	mov pwm1, #0
	setb oven_on
	setb flag3
;	jb START_STOP_BUTTON, state0_done
;	jnb START_STOP_BUTTON, $ ; Wait for key release
	jnb on_off, state0_done
	mov a, #0x00
	mov second, a
	mov minute, a
	mov state, #1
	mov sec, #0
	lcall start_beep
	clr flag1
	clr flag2
	clr oven_on
	
state0_done:
	ljmp cont2
state1:
	mov a, state
	cjne a, #1, state2
	
	setb flagL1
;	mov pwm1, #0x10
;	mov pwm2, #0x0
	clr oven_on
	setb flag3
	lcall WireCheck
	clr c
	lcall OVERHEATcheck
	clr c
	mov a, stemp_var+0
	subb a, temp+0
	mov a, stemp_var+1
	subb a, temp+1
	
	jnc state1_done
	mov state, #2
	mov sec, #0		;;in timer inc sec every second
	lcall tran_beep
	lcall tran_beep
	clr flagL1
	clr LED_1
	
state1_done:
	ljmp cont

	;;;;;;;
state2:
	mov a, state
	cjne a, #2, state3
	clr flag3
	clr LED_1
	setb flagL2
	lcall OVERHEATcheck
	clr c
	mov pwm1, #0x2
	mov pwm2, #0x8
	mov a, stime_var
	clr c
	subb a, sec
	jnc state2_done
	setb oven_on
	mov six, #0x06
	mov state, #3
	lcall tran_beep
	lcall tran_beep
	lcall tran_beep
	clr flagL2	
	clr LED_2
state2_done:
	ljmp cont
	
state3:
	mov a, state
	cjne a, #3, state4
;	mov pwm1, #0x10
;	mov pwm2, #0x0
	lcall OVERHEATcheck
	clr c
	clr oven_on
	setb flagL3
	setb flag3
	mov sec, #0
	clr c
	mov a, rtemp_var+0
	subb a, temp+0
	mov a, rtemp_var+1
	subb a, temp+1
	jnc state3_done
	mov state, #4
	setb oven_on
	clr flag1
	clr flag2
	clr oven_on
	mov sec, #0
	lcall tran_beep
	lcall tran_beep
	lcall tran_beep
	lcall tran_beep
	clr flagL3
	clr LED_3
state3_done:
	ljmp cont
	
state4:	
	cjne a, #4, state5
	lcall OVERHEATcheck
	clr c
	mov pwm1, #0x2
	mov pwm2, #0x8
	setb flagL4
	clr flag3

	mov a, rtime_var
	clr c
	subb a, sec
	jnc state4_done
	mov state, #5
	mov sec, #0x0
	setb oven_on
	clr flagL4
	clr LED_4
	lcall door_beep
	lcall tran_beep
	lcall tran_beep
	lcall tran_beep
	lcall tran_beep
state4_done:
	ljmp cont
	
state5:
	cjne a, #5, cont
	lcall OVERHEATcheck
	clr c
	setb oven_on
	setb flag3
	setb flagL5
;	mov pwm1, #0
	clr c
	mov a, temp+0
	subb a, #low(0x60)
	mov a, temp+1
	subb a, #high(0x60)
	jnc state5_done
	mov state, #0
	clr on_off
	clr flagL5
	clr LED_5
	lcall six_beep	
state5_done:
	ljmp cont	

cont:	
	jb on_off, cont2
	mov state, #0
	setb oven_on
	setb flag3
	mov c_minute, minute
	mov c_second, second
	lcall complete_screen
	setb LED_1
	setb LED_3
	setb LED_2
	setb LED_4
	setb LED_5
	setb flag1
	setb flag2
	setb flag3
	clr flagL1
	clr flagL2
	clr flagL3
	clr flagL4
	clr flagL5
	
cont2:	
	ljmp loop
END
; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP52
$LIST

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P3.7
UPDOWN        equ P0.0
BUTTON_1	  equ P2.0
BUTTON_2	  equ P2.1
BUTTON_3      equ p2.2
BUTTON_4 	  equ p2.3

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
second:		  ds 1 
minute:		  ds 1
hour:		  ds 1
a_minute:	  ds 1
a_hour:		  ds 1
SHIGH:		  ds 1
SLOW:		  ds 1


; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
am_flag:		dbit 1
a_am_flag:		dbit 1
alarm_flag:		dbit 1
select_flag:	dbit 1

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
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'xx : xx : xx ', 0
MESSAGE2:		  db 'xx : xx      ', 0
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
	cpl am_flag
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

BEEP:
	clr alarm_flag
	Set_Cursor(1, 1)
	Send_Constant_String(#Alarm_Msg)
	Set_Cursor(2, 1)
	Send_Constant_String(#Alarm_Msg2)
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
	cpl TR0

	jb BOOT_BUTTON, cont  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#250)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, cont  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	ljmp finish
cont:
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
	cpl TR0	
	Wait_Milli_Seconds(#25)
	jb BOOT_BUTTON, cont2  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#250)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, cont2  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	ljmp finish
cont2:	
	mov SHIGH, #216
	mov SLOW, #116
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

	mov SHIGH, #208
	mov SLOW, #249
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#120)
	cpl TR0		

	mov SHIGH, #196
	mov SLOW, #192
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#120)
	cpl TR0		

	mov SHIGH, #185
	mov SLOW, #138
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0	

	mov SHIGH, #208
	mov SLOW, #249
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#120)
	cpl TR0		

	mov SHIGH, #196
	mov SLOW, #192
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#120)
	cpl TR0	
	mov SHIGH, #203
	mov SLOW, #54
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0	
	mov SHIGH, #196
	mov SLOW, #192
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#120)
	cpl TR0		
	Wait_Milli_Seconds(#25)	
	mov SHIGH, #196
	mov SLOW, #192
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#120)
	cpl TR0	
	mov SHIGH, #185
	mov SLOW, #138
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0
	mov SHIGH, #208
	mov SLOW, #249
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	cpl TR0
	Wait_Milli_Seconds(#250)	
	jb BOOT_BUTTON, ASDF  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#250)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, ASDF  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	
finish:
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    Set_cursor(2, 1)
	Send_Constant_String(#MESSAGE2)
	clr TR0
	ljmp loop
ASDF:
	ljmp BEEP

BEEP2:
	clr alarm_flag
	Set_Cursor(1, 1)
	Send_Constant_String(#Alarm_Msg)
	Set_Cursor(2, 1)
	Send_Constant_String(#Alarm_Msg2)
	mov SHIGH, #200
	mov SLOW, #19
	cpl TR0
	Wait_Milli_Seconds(#125)
	cpl TR0
	mov SHIGH, #206
	mov SLOW, #45
	cpl TR0
	Wait_Milli_Seconds(#125)
	cpl TR0

	mov SHIGH, #211
	mov SLOW, #156
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0
	mov SHIGH, #218
	mov SLOW, #172
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	cpl TR0	
	
	mov SHIGH, #200
	mov SLOW, #19
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0

	jb BOOT_BUTTON, cont3  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#250)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, cont3  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	ljmp finish
cont3:
	mov SHIGH, #196
	mov SLOW, #192
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	cpl TR0
	mov SHIGH, #222
	mov SLOW, #191
	cpl TR0
	Wait_Milli_Seconds(#125)
	Wait_Milli_Seconds(#250)
	cpl TR0

	mov SHIGH, #218
	mov SLOW, #172
	cpl TR0
	Wait_Milli_Seconds(#65)
	cpl TR0
	mov SHIGH, #222
	mov SLOW, #191
	cpl TR0
	Wait_Milli_Seconds(#65)
	cpl TR0	 
	
	mov SHIGH, #218
	mov SLOW, #172
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	cpl TR0	
	Wait_Milli_Seconds(#25)
	jb BOOT_BUTTON, cont4  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#250)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, cont4  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	ljmp finish
cont4:	
	mov SHIGH, #211
	mov SLOW, #156
	cpl TR0
	Wait_Milli_Seconds(#125)
	cpl TR0	
	
	mov SHIGH, #216
	mov SLOW, #116
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0		
	
	mov SHIGH, #211
	mov SLOW, #156
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0	

	mov SHIGH, #206
	mov SLOW, #45
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0		

	mov SHIGH, #211
	mov SLOW, #156
	cpl TR0
	Wait_Milli_Seconds(#125)
	cpl TR0		

	mov SHIGH, #228
	mov SLOW, #9
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	cpl TR0	
	Wait_Milli_Seconds(#65)
	mov SHIGH, #208
	mov SLOW, #9
	cpl TR0
	Wait_Milli_Seconds(#250)
	cpl TR0		

	mov SHIGH, #226
	mov SLOW, #96
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	cpl TR0	
	mov SHIGH, #218
	mov SLOW, #172
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	cpl TR0	
	mov SHIGH, #222
	mov SLOW, #191
	cpl TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	cpl TR0		
	Wait_Milli_Seconds(#100)	
		
	jb BOOT_BUTTON, ASDF2  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#250)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, ASDF2  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.	
	ljmp finish
	
ASDF2:
	ljmp BEEP2
	
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
	setb am_flag
	setb a_am_flag
    setb half_seconds_flag
	mov BCD_counter, #0x00
	mov second, #0x00
	mov minute, #0x00
	mov hour, #0x01
	mov a_minute, #0x00
	mov a_hour, #0x00
	clr alarm_flag
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
	jnb alarm_flag, buttonx
	mov a, hour
	mov b, a_hour
	cjne a, b, buttonx
	mov a, minute
	mov b, a_minute
	cjne a, b, buttonx
	jb a_am_flag, bbbb
	jnb am_flag, jjjj
	jmp buttonx
bbbb:	
	jnb am_flag, buttonx
jjjj:
	jb select_flag, kkkk
	ljmp BEEP
kkkk:
	ljmp BEEP2

buttonx:
	jb BOOT_BUTTON, button1  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#250)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, button1  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	cpl select_flag
	ljmp loop_b
	
button1:
	clr c
	jb BUTTON_2, button2
	Wait_Milli_Seconds(#50)
	jb BUTTON_2, button2
	jnb BUTTON_2, $
	mov a, minute
	add a, #0x01
	da a
	cjne a, #0x60, button1x
	clr a
	mov minute, a
	ljmp loop_b             ; Display the new value
button1x:	
	mov minute, a
	ljmp loop_b             ; Display the new value
button2:
	jb BUTTON_1, button3
	Wait_Milli_Seconds(#50)
	jb BUTTON_1, button3
	jnb BUTTON_1, $
	mov a, hour
	add a, #0x01
	da a
	cjne a, #0x13, button2x
	mov a, #0x01
	mov hour, a
	cpl am_flag
	ljmp loop_b             ; Display the new value
button2x:
	mov hour, a
	ljmp loop_b 
button3:
	jb BUTTON_4, button4
	Wait_Milli_Seconds(#50)
	jb BUTTON_4, button4
	jnb BUTTON_4, $
	setb alarm_flag 
	mov a, a_minute
	add a, #0x01
	da a
	cjne a, #0x60, button3x
	clr a
	mov a_minute, a
	ljmp loop_b
button3x:
	mov a_minute, a
	ljmp loop_b
button4:
	jb BUTTON_3, loop_a
	Wait_Milli_Seconds(#50)
	jb BUTTON_3, loop_a
	jnb BUTTON_3, $
	setb alarm_flag
	mov a, a_hour
	add a, #0x01
	da a
	cjne a, #0x13, button4x
	clr a
	mov a_hour, a
	cpl a_am_flag
	ljmp loop_b     
button4x:
	mov a_hour, a
	ljmp loop_b 
loop_a:
	jnb half_seconds_flag, jump
	sjmp loop_b
jump:
	ljmp loop
loop_b:
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	;Set_Cursor(1, 14)     ; the place in the LCD where we want the BCD counter value
	;Display_BCD(BCD_counter) ; This macro is also in 'LCD_4bit.inc'w3
	Set_Cursor(1, 1)
	Display_BCD(hour)
	Set_Cursor(1, 6)
	Display_BCD(minute)
	Set_Cursor(1, 11)
	Display_BCD(second)
	
	Set_Cursor(2, 1)
	Display_BCD(a_hour)
	Set_Cursor(2, 6)
	Display_BCD(a_minute)
	

	jnb am_flag, pm
	set_Cursor(1, 14)
	Display_char(#'A')
	set_Cursor(1, 15)
	Display_char(#'M')
	jmp alarm
pm:	
	set_Cursor(1, 14)
	Display_char(#'P')
	set_Cursor(1, 15)
	Display_char(#'M')
	
alarm:
	jnb a_am_flag, a_pm
	set_Cursor(2, 14)
	Display_char(#'A')
	set_Cursor(2, 15)
	Display_char(#'M')
	jmp flag
a_pm:	
	set_Cursor(2, 14)
	Display_char(#'P')
	set_Cursor(2, 15)
	Display_char(#'M')
	
flag:
	jnb alarm_flag, flag2
	set_Cursor(2, 11)
	Display_char(#'!')
	jmp FFFF
flag2:
	set_Cursor(2, 11)
	Display_char(#'*')

FFFF:
	jnb select_flag, FFFF2
	set_Cursor(2, 9)
	Display_char(#'1')
	jmp done
FFFF2:	
	set_Cursor(2, 9)
	Display_char(#'2')
done:
    ljmp loop
END
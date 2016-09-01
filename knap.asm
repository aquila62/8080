; KNAP.ASM - UNBOUNDED KNAPSACK PROBLEM  VERSION 1.0.0
; COPYRIGHT (C) 2016 AQUILA62 AT GITHUB.COM

; THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
; MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS
; PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF
; THE LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.

; THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
; BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE
; GNU GENERAL PUBLIC LICENSE FOR MORE DETAILS.

; YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
; ALONG WITH THIS PROGRAM; IF NOT, WRITE TO:

   ; FREE SOFTWARE FOUNDATION, INC.
   ; 59 TEMPLE PLACE - SUITE 330
   ; BOSTON, MA 02111-1307, USA.

;------------------------------------------------------------
; UNBOUNDED KNAPSACK PROBLEM
; A KNAPSACK IS FILLED WITH THREE INGREDIENTS:
; 1. PANACEA (INCREDIBLE HEALING PROPERTIES)
; 2. AMPULES OF ICHOR (VAMPIRE'S BLOOD)
; 3. BARS OF GOLD (SHINEY GOLD)
; ITEM       VALUE    WEIGHT   VOLUME
; PANACEA    3000      0.3     0.025
; ICHOR      1800      0.2     0.015
; GOLD       2500      2.0     0.002
; MAXIMIZE VALUE WHERE WEIGHT <= 25 AND VOLUME <= 0.25
; WEIGHTS AND VOLUMES ARE CONVERTED TO INTEGERS
; DURING COMPUTATIONS.
; VALUE IS COMPUTED AT 1/10 OF ITS NORMAL VALUE
; DURING COMPUTATIONS.
; IN THIS PROGRAM THERE ARE FOUR MAXIMUM RESULTS.
; THEY ARE ALL VALID.
;------------------------------------------------------------

KCIN   EQU 0006H       ; CP/M JUMP VECTOR FOR KEY INPUT
KCOUT  EQU 0009H       ; CP/M JUMP VECTOR FOR CONSOLE OUTPUT
   ORG 100H            ; CP/M LOADS THE PROGRAM AT 100H
   JMP STRT            ; BYPASS DATA AREA
PAN:    DW 0,0         ; NUMBER OF PANACEAS
ICH:    DW 0,0         ; NUMBER OF ICHORS
GLD:    DW 0,0         ; NUMBER OF GOLD BARS
TPVAL:  DW 0,0         ; TOTAL PANACEA VALUE
TIVAL:  DW 0,0         ; TOTAL ICHOR   VALUE
TGVAL:  DW 0,0         ; TOTAL GOLD    VALUE
TPWT:   DW 0,0         ; TOTAL PANACEA WEIGHT
TIWT:   DW 0,0         ; TOTAL ICHOR   WEIGHT
TGWT:   DW 0,0         ; TOTAL GOLD    WEIGHT
TPVOL:  DW 0,0         ; TOTAL PANACEA VOLUME
TIVOL:  DW 0,0         ; TOTAL ICHOR   VOLUME
TGVOL:  DW 0,0         ; TOTAL GOLD    VOLUME
TOTVAL: DW 0,0         ; TOTAL VALUE
TOTWT:  DW 0,0         ; TOTAL WEIGHT
TOTVOL: DW 0,0         ; TOTAL VOLUME
MAXVAL: DW 0,0         ; MAXIMUM VALUE OBTAINED
NUM:    DW 0,0         ; NUMBER TO PRINT IN DECIMAL
STKADR: DW 0,0         ; CURRENT POINTER IN STACK
STKSZ:  DB 0,0,0,0     ; NUMBER OF DIGITS IN THE STACK
;-------------------- 8 BIT DIVISION
DVDND:     DW 0,0
TEN:       DB 10,0
DIVISOR:   DB 0,0
QUOTIENT:  DW 0,0
REMAINDER: DB 0,0,0,0
RMDR:      DB 0,0,0,0
;---------------------------------------------------
; TRANSLATE TABLE FOR PRINTING A 4-BIT NYBBLE IN HEX
;---------------------------------------------------
HXTBL:  DB '0123456789ABCDEF'
; VALUE OF INGREDIENTS
PANVAL:  DW 300         ; 1/10 OF ITS NORMAL VALUE
ICHVAL:  DW 180         ; 1/10 OF ITS NORMAL VALUE
GLDVAL:  DW 250         ; 1/10 OF ITS NORMAL VALUE
; WEIGHT OF INGREDIENTS
PANWT:   DW 3           ; TENTHS
ICHWT:   DW 2           ; TENTHS
GLDWT:   DW 20          ; TENTHS
MAXWT:   DW 250         ; TENTHS
; VOLUME OF INGREDIENTS
PANVOL:  DW 25          ; 1/1000
ICHVOL:  DW 15          ; 1/1000
GLDVOL:  DW 2           ; 1/1000
MAXVOL:  DW 250         ; 1/1000
; HEADERS
VHDR:    DB 'MAXIMUM VALUE ',0
PHDR:    DB 'PANACEA ',0
IHDR:    DB 'ICHOR ',0
GHDR:    DB 'GOLD ',0
WTHDR:   DB 'WT ',0
VOLHDR:  DB 'VOL ',0
; DECIMAL NUMBERS ARE PRINTED FROM A STACK 
STK: DS 16             ; DECIMAL NUMBER STACK
;---------------------------------------------------
STRT:                  ; PROGRAM STARTS HERE
   CALL MAXMIZE        ; MAXIMIZE THE VALUE OF THE KNAPSACK
;---------------------------------------------------
; END OF JOB
; JUMP TO ADDRESS ZERO TO RESET CP/M
;---------------------------------------------------
EOJ:
   JMP 0H
   NOP
   NOP
   NOP
   NOP
;---------------------------------------------------
; MAXIMIZE THE CONTENTS OF THE KNAPSACK
;---------------------------------------------------
MAXMIZE:
	PUSH PSW
	PUSH B
	XRA A
	;---------------------------------------------------
	; INITIALIZE ALL TOTALS
	;---------------------------------------------------
	STA TPWT
	STA TPVOL
	STA TPVAL
	STA TIWT
	STA TIVOL
	STA TIVAL
	STA TGWT
	STA TGVOL
	STA TGVAL
	STA PAN
	STA ICH
	STA GLD
	STA MAXVAL
	;---------------------------------------------------
	; CYCLE THROUGH PANACEA VALUES
	;---------------------------------------------------
MAX2:
	CALL DOICH            ; CYCLE THROUGH ICHOR VALUES
	;---------------------------------------------------
	; INCREASE PANACEA WEIGHT
	;---------------------------------------------------
	LHLD PANWT
	MOV B,H
	MOV C,L
	LHLD TPWT
	DAD B
	SHLD TPWT
	;---------------------------------------------------
	; TEST PANACEA WEIGHT OVER 250
	;---------------------------------------------------
	LDA TPWT+1
	ORA A
	JNZ MAX3
	LDA TPWT
	MVI B,251
	CMP B
	JNC MAX3
	;---------------------------------------------------
	; INCREASE PANACEA VOLUME
	;---------------------------------------------------
	LHLD PANVOL
	MOV B,H
	MOV C,L
	LHLD TPVOL
	DAD B
	SHLD TPVOL
	;---------------------------------------------------
	; TEST PANACEA VOLUME OVER 250
	;---------------------------------------------------
	LDA TPVOL+1
	ORA A
	JNZ MAX3
	LDA TPVOL
	MVI B,251
	CMP B
	JNC MAX3
	;---------------------------------------------------
	; INCREASE PANACEA VALUE
	;---------------------------------------------------
	LHLD PANVAL
	MOV B,H
	MOV C,L
	LHLD TPVAL
	DAD B
	SHLD TPVAL
	;---------------------------------------------------
	; INCREASE PANACEA COUNT
	;---------------------------------------------------
	LHLD PAN
	INX H
	SHLD PAN
	JMP MAX2           ; REPEAT PANACEA LOOP
MAX3:
	POP B
	POP PSW
	RET
;---------------------------------------------------
; ICHOR EXAMPLES
;---------------------------------------------------
DOICH:
	PUSH PSW
	PUSH B
ICH2:
	CALL DOGLD            ; CYCLE THROUGH GOLD VALUES
	;---------------------------------------------------
	; INCREASE ICHOR WEIGHT
	;---------------------------------------------------
	LHLD ICHWT
	MOV B,H
	MOV C,L
	LHLD TIWT
	DAD B
	SHLD TIWT
	;---------------------------------------------------
	; TEST ICHOR WEIGHT OVER 250
	;---------------------------------------------------
	LDA TIWT+1
	ORA A
	JNZ ICH3
	LDA TIWT
	MVI B,251
	CMP B
	JNC ICH3
	;---------------------------------------------------
	; INCREASE ICHOR VOLUME
	;---------------------------------------------------
	LHLD ICHVOL
	MOV B,H
	MOV C,L
	LHLD TIVOL
	DAD B
	SHLD TIVOL
	;---------------------------------------------------
	; TEST ICHOR VOLUME OVER 250
	;---------------------------------------------------
	LDA TIVOL+1
	ORA A
	JNZ ICH3
	LDA TIVOL
	MVI B,251
	CMP B
	JNC ICH3
	;---------------------------------------------------
	; INCREASE ICHOR VALUE
	;---------------------------------------------------
	LHLD ICHVAL
	MOV B,H
	MOV C,L
	LHLD TIVAL
	DAD B
	SHLD TIVAL
	;---------------------------------------------------
	; INCREASE ICHOR COUNT
	;---------------------------------------------------
	LHLD ICH
	INX H
	SHLD ICH
	JMP ICH2            ; REPEAT ICHOR LOOP
ICH3:                       ; ZERO ICHOR TOTALS
	XRA A
	MOV H,A
	MOV L,A
	SHLD TIWT
	SHLD TIVOL
	SHLD TIVAL
	SHLD ICH
	;---------------
	POP B
	POP PSW
	RET
;---------------------------------------------------
; GOLD EXAMPLES
;---------------------------------------------------
DOGLD:
	PUSH PSW
	PUSH B
GLD2:
	CALL EVAL            ; EVALUATE THE CURRENT STATE
	;---------------------------------------------------
	; INCREASE GOLD WEIGHT
	;---------------------------------------------------
	LHLD GLDWT
	MOV B,H
	MOV C,L
	LHLD TGWT
	DAD B
	SHLD TGWT
	;---------------------------------------------------
	; TEST GOLD WEIGHT OVER 250
	;---------------------------------------------------
	LDA TGWT+1
	ORA A
	JNZ GLD3
	LDA TGWT
	MVI B,251
	CMP B
	JNC GLD3
	;---------------------------------------------------
	; INCREASE GOLD VOLUME
	;---------------------------------------------------
	LHLD GLDVOL
	MOV B,H
	MOV C,L
	LHLD TGVOL
	DAD B
	SHLD TGVOL
	;---------------------------------------------------
	; TEST GOLD VOLUME OVER 250
	;---------------------------------------------------
	LDA TGVOL+1
	ORA A
	JNZ GLD3
	LDA TGVOL
	MVI B,251
	CMP B
	JNC GLD3
	;---------------------------------------------------
	; INCREASE GOLD VALUE
	;---------------------------------------------------
	LHLD GLDVAL
	MOV B,H
	MOV C,L
	LHLD TGVAL
	DAD B
	SHLD TGVAL
	;---------------------------------------------------
	; INCREASE GOLD COUNT
	;---------------------------------------------------
	LHLD GLD
	INX H
	SHLD GLD
	JMP GLD2             ; REPEAT GOLD LOOP
GLD3:                        ; ZERO GOLD TOTALS
	XRA A
	MOV H,A
	MOV L,A
	SHLD TGWT
	SHLD TGVOL
	SHLD TGVAL
	SHLD GLD
	POP B
	POP PSW
	RET
;---------------------------------------------------
; EVALUATE TOTAL VALUE FOR THE CURRENT STATE
;---------------------------------------------------
EVAL:
	PUSH PSW
	PUSH B
        ;---------------------------------------------------
	; IS TOTAL WEIGHT OVER THE MAXIMUM?
        ;---------------------------------------------------
	LHLD TIWT
	MOV B,H
	MOV C,L
	LHLD TPWT
	DAD B
	MOV B,H
	MOV C,L
	LHLD TGWT
	DAD B
	SHLD TOTWT
	;---------
	LDA TOTWT+1
	ORA A
	JNZ EVAL4           ; TOO HEAVY
	LDA TOTWT
	MVI B,251
	CMP B
	JNC EVAL4           ; TOO HEAVY
        ;---------------------------------------------------
	; IS TOTAL VOLUME OVER THE MAXIMUM?
        ;---------------------------------------------------
	LHLD TIVOL
	MOV B,H
	MOV C,L
	LHLD TPVOL
	DAD B
	MOV B,H
	MOV C,L
	LHLD TGVOL
	DAD B
	SHLD TOTVOL
	;-----------
	LDA TOTVOL+1
	ORA A
	JNZ EVAL4           ; TOO BULKY
	LDA TOTVOL
	MVI B,251
	CMP B
	JNC EVAL4           ; TOO BULKY
        ;---------------------------------------------------
	; COMPARE TOTAL VALUE TO MAXIMUM
        ;---------------------------------------------------
	LHLD TIVAL
	MOV B,H
	MOV C,L
	LHLD TPVAL
	DAD B
	MOV B,H
	MOV C,L
	LHLD TGVAL
	DAD B
	SHLD TOTVAL
	;------------
	LDA MAXVAL+1
	MOV B,A
	LDA TOTVAL+1
	CMP B
	JC EVAL4
	JZ EVAL2
	JNC EVAL3
EVAL2:
	LDA MAXVAL
	MOV B,A
	LDA TOTVAL
	CMP B
	JC EVAL4
EVAL3:
	LHLD TOTVAL
	SHLD MAXVAL           ; NEW MAXIMUM VALUE
	CALL PUTMAX           ; PRINT NEW MAXIMUM VALUE
EVAL4:
	POP B
	POP PSW
	RET
;---------------------------------------------------
; PRINT MAXIMUM VALUE AND COUNTS OF 3 INGREDIENTS
;---------------------------------------------------
PUTMAX:
	PUSH PSW
	PUSH H
        ;---------------------------------------------------
	; PRINT MAXIMUM VALUE IN DECIMAL
        ;---------------------------------------------------
	LXI H,VHDR
	CALL PUTSTR        ; PRINT HEADER
	LHLD TOTVAL
	CALL PDEC          ; PRINT DECIMAL NUMBER
	CALL PUTZERO       ; ADD ZERO ON END OF NUMBER
	CALL PUTSPC        ; PRINT FOLLOWING SPACE
        ;---------------------------------------------------
	; PRINT PANACEA COUNT IN DECIMAL
        ;---------------------------------------------------
	LXI H,PHDR
	CALL PUTSTR        ; PRINT HEADER
	LHLD PAN
	CALL PDEC          ; PRINT DECIMAL NUMBER
	CALL PUTSPC        ; PRINT FOLLOWING SPACE
        ;---------------------------------------------------
	; PRINT ICHOR COUNT IN DECIMAL
        ;---------------------------------------------------
	LXI H,IHDR
	CALL PUTSTR        ; PRINT HEADER
	LHLD ICH
	CALL PDEC          ; PRINT DECIMAL NUMBER
	CALL PUTSPC        ; PRINT FOLLOWING SPACE
        ;---------------------------------------------------
	; PRINT GOLD BAR COUNT IN DECIMAL
        ;---------------------------------------------------
	LXI H,GHDR
	CALL PUTSTR        ; PRINT HEADER
	LHLD GLD
	CALL PDEC          ; PRINT DECIMAL NUMBER
        ;---------------------------------------------------
	; PRINT WEIGHT AND VOLUME FOR DEBUGGING
        ;---------------------------------------------------
	; CALL PUTSPC        ; PRINT FOLLOWING SPACE
	; LXI H,WTHDR
	; CALL PUTSTR        ; PRINT HEADER
	; LHLD TOTWT
	; CALL PDEC          ; PRINT DECIMAL NUMBER
	; CALL PUTSPC        ; PRINT FOLLOWING SPACE
	; LXI H,VOLHDR
	; CALL PUTSTR        ; PRINT HEADER
	; LHLD TOTVOL
	; CALL PDEC          ; PRINT DECIMAL NUMBER
	; CALL PUTSPC        ; PRINT FOLLOWING SPACE
        ;---------------------------------------------------
	CALL PUTEOL        ; PRINT END OF LINE
	POP H
	POP PSW
	RET
;---------------------------------------------------------
; PAUSE FOR KEYBOARD INPUT, QUIT IF 'Q'
;---------------------------------------------------------
PAUSE:
   PUSH PSW
   CALL CIN         ; WAIT FOR KEYBOARD INPUT
   CPI 01AH         ; CONTROL Z?
   JZ EOJ           ; YES, QUIT
   CPI 'Q'          ; 'Q' KEY?
   JZ EOJ           ; YES, QUIT
   POP PSW          ; ELSE, RETURN FROM PAUSE
   RET
;---------------------------------------------------------
; PRINT 16-BIT BINARY NUMBER IN DECIMAL
; THE NUMBER TO PRINT IS IN THE HL REGISTER
; PRINT A SPACE AFTER THE NUMBER
; THE TECHNIQUE USED IS TO DIVIDE THE NUMBER REPEATEDLY BY 10
; EACH REMAINDER IS PUSHED ONTO A STACK OF DIGITS
; THE NUMBER IS PRINTED BY POPPING THE STACK FOR EACH DIGIT.
;---------------------------------------------------------
PDEC:
   PUSH PSW
   PUSH B
   PUSH D
   PUSH H
   ; STORE HL REGISTER IN VARIABLE NUM
   SHLD NUM
   ; POINT TO EMPTY STACK
   LXI H,STK
   SHLD STKADR
   ; ONLY 5 DIGITS ARE PUSHED ONTO THE STACK
   MVI A,5
   STA STKSZ
   CALL CLRSTK         ; CLEAR THE STACK TO ALL ZEROS
PDEC2:                 ; DIVISION LOOP
   ; SET UP THE PARAMETERS IN THE DIVISION SUBROUTINE
   LDA NUM
   STA DVDND
   LDA NUM+1
   STA DVDND+1
   XRA A
   STA DVDND+2
   LDA TEN
   STA DIVISOR
   CALL DIV8
   ; PUSH THE REMAINDER ONTO THE STACK
   LHLD STKADR
   LDA REMAINDER
   MOV M,A
   ; SAVE THE QUOTIENT FOR THE NEXT ITERATION
   LDA QUOTIENT
   STA NUM
   LDA QUOTIENT+1
   STA NUM+1
   ; BUMP THE STACK POINTER
   LDA STKADR
   ADI 1
   STA STKADR
   LDA STKADR+1
   ACI 0
   STA STKADR+1
   ; 5 DIGITS ON STACK?
   LDA STKSZ
   DCR A
   STA STKSZ
   ORA A
   JNZ PDEC2         ; NO, DIVIDE BY 10 AGAIN
; YES, BYPASS LEADING ZEROS
PDEC3:
   LDA STK+4
   ORA A
   JNZ PDEC4
   LDA STK+3
   ORA A
   JNZ PDEC5
   LDA STK+2
   ORA A
   JNZ PDEC6
   LDA STK+1
   ORA A
   JNZ PDEC7
   JMP PDEC9
; AFTER BYPASSING LEADING ZEROS
; PRINT HIGH ORDER HUNDRED THOUSANDS DIGIT
PDEC4:
   LDA STK+4
   ADI 030H
   CALL COUT
; PRINT HIGH ORDER THOUSANDS DIGIT
PDEC5:
   LDA STK+3
   ADI 030H
   CALL COUT
; PRINT HIGH ORDER HUNDREDS DIGIT
PDEC6:
   LDA STK+2
   ADI 030H
   CALL COUT
; PRINT HIGH ORDER TENS DIGIT
PDEC7:
   LDA STK+1
   ADI 030H
   CALL COUT
; PRINT LOW ORDER UNITS DIGIT
PDEC9:
   LDA STK
   ADI 030H
   CALL COUT
   POP H
   POP D
   POP B
   POP PSW
   RET
;---------------------------------------------------------
; PRINT CONTENTS OF VARIABLE NUM IN HEX, BIG ENDIAN FORMAT
;---------------------------------------------------------
PUTNUM:
   PUSH PSW
   LDA NUM+1
   CALL PUTHEX
   LDA NUM
   CALL PUTHEXA
   POP PSW
   RET
;---------------------------------------------------------
; CLEAR THE DECIMAL DIGIT STACK TO ALL ZEROS
; MAXIMUM OF 5 DIGITS
;---------------------------------------------------------
CLRSTK:
   PUSH PSW
   XRA A
   STA STK
   STA STK+1
   STA STK+2
   STA STK+3
   STA STK+4
   POP PSW
   RET
;---------------------------------------------------------
; PRINT THE DECIMAL DIGIT STACK
; IN BIG ENDIAN FORMAT
; PRINT LEADING ZEROS
;---------------------------------------------------------
PTSTK:
   PUSH PSW
   PUSH H
   LXI H,STK+4
   MOV A,M
   ADI 030H
   CALL COUT
   DCX H
   MOV A,M
   ADI 030H
   CALL COUT
   DCX H
   MOV A,M
   ADI 030H
   CALL COUT
   DCX H
   MOV A,M
   ADI 030H
   CALL COUT
   DCX H
   MOV A,M
   ADI 030H
   CALL COUT
   CALL PUTSPC
   CALL PAUSE
   POP H
   POP PSW
   RET
;---------------------------------------------------------
; STANDARD Z80 DIVISION
; OF 8-BIT NUMBER INTO A 24-BIT NUMBER
; GIVING A 16-BIT QUOTIENT AND AN 8-BIT REMAINDER
; HERE ARE THE VARIABLE NAMES USED:
; QUOTIENT  = DVDND / DIVISOR
; REMAINDER = DVDND % DIVISOR
;---------------------------------------------------------
DIV8:                    ; THIS ROUTINE PERFORMS THE OPERATION HL=HL/D
   PUSH PSW
   PUSH B
   PUSH D
   PUSH H
   LHLD DVDND
   LDA DIVISOR
   MOV D,A
   XRA A                  ; CLEARING THE UPPER 8 BITS OF AHL
   STA REMAINDER
   STA RMDR
   STA QUOTIENT
   STA QUOTIENT+1
   MVI B,16               ; THE LENGTH OF THE DIVIDEND (16 BITS)
DIV8LOOP:
   LDA RMDR
   DAD H                  ; ADVANCING A BIT
   RAL
   ;---------------------------------------------------------
   ; CHECKING IF THE DIVISOR DIVIDES THE DIGITS CHOSEN (IN A)
   ;---------------------------------------------------------
   CMP D
   JC DIV8NEXTBIT         ; IF NOT, ADVANCING WITHOUT SUBTRACTION
   SUB D                  ; SUBTRACTING THE DIVISOR
   INR L                  ; AND SETTING THE NEXT DIGIT OF THE QUOTIENT
DIV8NEXTBIT:
   STA RMDR
   DCR B
   MOV A,B
   CPI 0
   JNZ DIV8LOOP
DV9:
   SHLD QUOTIENT
   LDA RMDR
   STA REMAINDER
   POP H
   POP D
   POP B
   POP PSW
   RET
;----------------------- DIV8 ------------------ 
;
;----------------------------------------------------------
; PRINT A REGISTER IN HEXADECIMAL, THEN PRINT ONE SPACE
;----------------------------------------------------------
PUTHXA:
   CALL PUTHEX
   CALL PUTSPC
   RET
;----------------------------------------------------------
; PRINT BC REGISTER IN HEXADECIMAL
;----------------------------------------------------------
PUTBC: PUSH PSW
   MOV A,B
   CALL PUTHEX
   MOV A,C
   CALL PUTHEX
   CALL PUTSPC
   POP PSW
   RET
;----------------------------------------------------------
; PRINT HL REGISTER IN HEXADECIMAL
;----------------------------------------------------------
PUTHL:
   PUSH PSW
   PUSH H
   MOV A,H
   CALL PUTHEX
   MOV A,L
   CALL PUTHEX
   CALL PUTSPC
   POP H
   POP PSW
   RET
;----------------------------------------------------------
; PRINT A REGISTER IN HEXADECIMAL
;----------------------------------------------------------
PUTHEX:
   PUSH PSW
   PUSH B
   MOV B,A     ; SAVE A REG IN B REG
   ; SHIFT A REGISTER 4 BITS TO RIGHT
   ; 8080 DOES NOT HAVE A SHIFT INSTRUCTION
   STC         ; SET CARRY TO 1
   CMC         ; COMPLEMENT CARRY TO ZERO
   RAR         ; ROLL RIGHT 1 BIT WITH ZERO CARRY
   ;---------------
   STC
   CMC
   RAR
   ;---------------
   STC
   CMC
   RAR
   ;---------------
   STC
   CMC
   RAR
   CALL PUTNBL      ; PRINT THE HIGH ORDER 4 BITS IN HEX
   MOV A,B          ; RESTORE A REG
   ANI 0FH          ; ISOLATE LOWER ORDER 4 BITS
   CALL PUTNBL      ; PRINT THE LOW ORDER 4 BITS IN HEX
   POP B
   POP PSW
   RET
;----------------------------------------------------------
; PRINT 4-BIT NYBBLE IN HEXADECIMAL
; THE 8080 DOES NOT HAVE THE XLAT INSTRUCTION
;----------------------------------------------------------
PUTNBL:
   PUSH PSW
   PUSH B
   PUSH H
   MVI B,0            ; BC IS INDEX TO HEX TABLE
   MOV C,A
   LXI H,HXTBL        ; HL POINTS TO HEX TABLE
   DAD B              ; HL = HXTBL[BC]
   MOV A,M            ; LOAD ASCII CHARACTER FROM HEX TABLE
   CALL COUT          ; PRINT ASCII HEX CHARACTER
   POP H
   POP B
   POP PSW
   RET
;----------------------------------------------------------
; PRINT \R\N END OF LINE SEQUENCE
;----------------------------------------------------------
PUTEOL:
   PUSH PSW
   MVI A,13          ; PRINT CARRIAGE RETURN
   CALL COUT
   MVI A,10          ; PRINT LINE FEED
   CALL COUT
   POP PSW
   RET
;----------------------------------------------------------
; PRINT ONE SPACE
;----------------------------------------------------------
PUTSPC:
   PUSH PSW
   MVI A,020H
   CALL COUT
   POP PSW
   RET
;----------------------------------------------------------
; PRINT ONE ZERO
;----------------------------------------------------------
PUTZERO:
   PUSH PSW
   MVI A,030H
   CALL COUT
   POP PSW
   RET
;----------------------------------------------------------
; PRINT X FOLLOWED BY ONE SPACE, FOR DEBUGGING
;----------------------------------------------------------
PUTX:
   PUSH PSW
   MVI A,'X'
   CALL COUT
   MVI A,20H
   CALL COUT
   POP PSW
   RET
;----------------------------------------------------------
; PRINT Y FOLLOWED BY ONE SPACE, FOR DEBUGGING
;----------------------------------------------------------
PUTY:
   PUSH PSW
   MVI A,'Y'
   CALL COUT
   MVI A,20H
   CALL COUT
   POP PSW
   RET
;----------------------------------------------------------
; PRINT Z FOLLOWED BY ONE SPACE, FOR DEBUGGING
;----------------------------------------------------------
PUTZ:
   PUSH PSW
   MVI A,'Z'
   CALL COUT
   MVI A,20H
   CALL COUT
   POP PSW
   RET
;----------------------------------------------------------
; PRINT STRING.  HL POINTS TO STRING.
;----------------------------------------------------------
PUTSTR:
   PUSH PSW
   PUSH H
PUTS2:
   MOV A,M
   ORA A
   JZ PUTS3
   CALL COUT
   INX H
   JMP PUTS2
PUTS3:
   POP H
   POP PSW
   RET
;----------------------------------------------------------
; READ KEYBOARD WITH WAIT, WITHOUT ECHO
;----------------------------------------------------------
CIN:
   PUSH B
   PUSH D
   PUSH H
   LXI D,KCIN
   CALL IOS
   POP H
   POP D
   POP B
   ; RETURNS CHARACTER IN REG A
   RET
;----------------------------------------------------------
; PRINT A REGISTER IN ASCII TO CONSOLE FOLLOWED BY
; PRINTING ONE SPACE
;----------------------------------------------------------
COUTSPC:
   CALL COUT
   CALL PUTSPC
   RET
;----------------------------------------------------------
; PRINT A REGISTER IN ASCII TO CONSOLE
;----------------------------------------------------------
COUT:
   PUSH PSW
   PUSH B
   PUSH D
   PUSH H
   MOV C,A
   LXI D,KCOUT
   CALL IOS
   POP H
   POP D
   POP B
   POP PSW
   RET
;----------------------------------------------------------
; CP/M INPUT/OUTPUT SERVICE ROUTINE
; RETURN ADDRESS IS ON STACK
; RETURNS TO CALLER OF CIN OR COUT
;----------------------------------------------------------
IOS:
   LHLD 01H
   DAD D
   PCHL
   NOP
   NOP
   NOP
   NOP
   END             ; END OF PROGRAM

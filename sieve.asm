; SIEVE.ASM - SIEVE OF ERATOSTHENES   VERSION 1.0.0
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

;--------------------------------------------------------------
; THIS PROGRAM CREATES A LIST OF PRIME NUMBERS FROM 2 UP TO
; 2039.
;
; THE ALGORITHM USED IS CALLED THE "SIEVE OF ERATOSTHENES".
;
; WHEN DEBUGGING, QUIT THE PROGRAM,
; BY PRESSING 'Q' DURING A PAUSE.
;
; THE LIMIT OF 2039 IS CHOSEN, SO THAT THE PRIME NUMBER LIST
; CAN FIT EASILY ON A SINGLE SCREEN.
;
; THE PROGRAM STARTS WITH A LIST OF ODD NUMBERS FROM 3 TO
; 2047.  ALL NUMBERS THAT ARE MULTIPLES OF A PRIME NUMBER
; ARE CHANGED TO ZERO ON THE LIST.  FINALLY THE NUMBERS LEFT
; OVER ON THE LIST ARE PRIME NUMBERS.
;
; THE PRIME NUMBERS LEFT ON THE LIST ARE PRINTED IN BIG ENDIAN
; DECIMAL FORMAT.  LEADING ZEROS ARE NOT PRINTED.  WORD WRAP IS
; NOT USED.  SOME NUMBERS OVERFLOW ONTO A SECOND LINE.
;
; ONE HEURISTIC, THAT IS NOT USED IN THIS PROGRAM, IS TO START
; ZEROING WITH THE SQUARE OF A CANDIDATE PRIME NUMBER.
;--------------------------------------------------------------

KCIN   EQU 0006H       ; CP/M JUMP VECTOR FOR KEY INPUT
KCOUT  EQU 0009H       ; CP/M JUMP VECTOR FOR CONSOLE OUTPUT
   ORG 100H            ; CP/M LOADS THE PROGRAM AT 100H
   JMP STRT            ; BYPASS DATA AREA
PRM:    DW 0,0         ; CURRENT PRIME NUMBER CANDIDATE
GAP:    DW 0,0         ; GAP IN BYTES BETWEEN MULTIPLES
ADDR:   DW 0,0         ; ADDRESS OF PRIME + 2P IN ARRAY SV
CURADR: DW 0,0         ; CURRENT ADDRESS IN ARRAY SV
DMPADR: DW 0,0         ; DUMP ADDRESS IN ARRAY SV
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
; DECIMAL NUMBERS ARE PRINTED FROM A STACK 
STK: DS 16             ; DECIMAL NUMBER STACK
SV: DS 8192            ; SIEVE ARRAY OF ODD NUMBERS
SVEND: DS 32           ; MARKER FOR THE END OF THE ARRAY
;---------------------------------------------------
STRT:                  ; PROGRAM STARTS HERE
   CALL BLD            ; FILL THE SIEVE ARRAY WITH ODD NUMBERS
   CALL XOUT           ; ZERO OUT MULTIPLES OF PRIME NUMBERS
   CALL SHW            ; PRINT REMAINING PRIME NUMBERS
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
;---------------------------------------------
; BUILD THE SIEVE ARRAY
; FILL THE SIEVE ARRAY WITH ODD NUMBERS FROM
; 3 TO 2047.
; FILL THE REST OF THE SIEVE ARRAY WITH HEX FF.
; HEX FF MARKS THE END OF THE ODD NUMBER LIST.
;---------------------------------------------
BLD:
   PUSH PSW
   PUSH B
   PUSH H
   ; INITIALIZE 16-BIT INTEGER PRM TO 3
   MVI A,3H
   STA PRM
   XRA A
   STA PRM+1
   ; INITIALIZE GAP TO 6
   ; THIS REPRESENTS THE GAP FOR PRIME NUMBER 3
   ; THE GAP IS TWICE AS MANY BYTES AS THE PRIME NUMBER
   ; SO THAT THE GAP FOR 5 IS 10, FOR EXAMPLE
   MVI A,6H
   STA GAP
   XRA A
   STA GAP+1
   ; SET THE POINTER TO THE ADDRESS OF A CANDIDATE
   ; PRIME NUMBER IN THE SIEVE ARRAY.
   LXI H,SV
   SHLD ADDR
;---------------------------------------------
; MAIN ODD NUMBER LOOP IN BLD
;---------------------------------------------
BLD2:
   ; IN BLD, PRM IS A CANDIDATE PRIME NUMBER
   ; IT IS ALL THE ODD NUMBERS FROM 3 TO 2039
   ; PLACE THE NEXT ODD NUMBER IN ITS ORDINAL
   ; PLACE IN THE ODD NUMBER ARRAY
   LDA PRM
   LHLD ADDR
   MOV M,A
   INX H
   LDA PRM+1
   MOV M,A
   INX H
   SHLD ADDR
   ; ADD 2 TO THE CURRENT ODD NUMBER
   ; TO GET THE NEXT ODD NUMBER
   LDA PRM
   ADI 2H
   STA PRM
   LDA PRM+1
   ACI 0H
   STA PRM+1
   ; CHECK PRM TO SEE IF END OF LIST HAS BEEN REACHED.
   ; 800 HEX IS 2048.
   ; ALL NUMBERS ARE STORED IN LITTLE ENDIAN FORMAT
   LDA PRM+1
   CPI 08H       ; 800 HEX?  (>= 2048?)
   JNZ BLD2      ; NO, ADD NEXT ODD NUMBER TO LIST
;---------------------------------------------
; END OF ODD NUMBER LIST AT 2047 (800 HEX)
; NOW ADD HEX FF TO MARK THE END OF OF THE LIST 
; TERMINATE ADDING HEX FF AT 8192. 
;---------------------------------------------
BLD3:
   MVI A,0FFH
   MOV M,A
   INX H
   MOV A,H
   CPI 020H          ; END OF LIST AT 8192 (2000 HEX)?
   JM BLD3           ; NO, REPEAT. MOVE 0FFH TO END OF ARRAY
   POP H
   POP B
   POP PSW
   RET
;-------------------------------------------------------
; ROUTINE TO CROSS OUT ODD NUMBERS THAT ARE MULTIPLES
; OF A PRIME NUMBER
;-------------------------------------------------------
XOUT:
   PUSH PSW
   PUSH B
   PUSH H
   ; INITIALIZE PRM TO 3
   MVI A,3H
   STA PRM
   ; INITIALIZE THE GAP TO 6 (THE GAP FOR 3)
   MVI A,6H
   STA GAP
   XRA A
   STA PRM+1
   STA GAP+1
   ; POINT TO THE BEGINNING OF THE SIEVE ARRAY
   LXI H,SV
   SHLD ADDR
;-------------------------------------------------------
; OUTER LOOP
; EACH ITERATION REPRESENTS A CANDIDATE PRIME NUMBER
; AND ITS MULTIPLES
;-------------------------------------------------------
XOUT1:
   ; POINT TO THE FIRST MULTIPLE OF PRM
   ; CURADR POINTS TO THE FIRST MULTIPLE
   LHLD GAP
   MOV B,H
   MOV C,L
   LHLD ADDR
   DAD B
   SHLD CURADR
;-------------------------------------------------------
; INNER LOOP TO ZERO OUT MULTIPLES OF PRIMES
;-------------------------------------------------------
XOUT2:
   ; IS CURRENT MULTIPLE BEYOND END OF LIST?
   LHLD CURADR
   INX H
   MOV A,M
   CPI 0FFH
   JZ XOUT3         ; YES, PROCESS NEXT PRIME NUMBER
XOT2B:              ; NO, NOT END OF LIST
   ; ZERO OUT THE CURRENT MULTIPLE OF PRM
   LHLD CURADR
   XRA A
   MOV M,A
   INX H
   MOV M,A
   ; NOW POINT TO THE NEXT MULTIPLE OF PRM
   LHLD GAP
   MOV B,H
   MOV C,L
   LHLD CURADR
   DAD B
   SHLD CURADR
   JMP XOUT2             ; REPEAT INNER LOOP
; ADD 2 TO ODD NUMBER PRIME PRM
; ADD 4 TO GAP BETWEEN MULTIPLES
; ADD 2 TO STARTING ADDRESS IN SIEVE ARRAY
XOUT3:
   ; ADD 2 TO ODD NUMBER PRIME PRM
   LHLD PRM
   MVI C,02H
   MVI B,0H
   DAD B
   SHLD PRM
   ; ADD 4 TO GAP BETWEEN MULTIPLES
   LHLD GAP
   MVI C,4H
   MVI B,0H
   DAD B
   SHLD GAP
   ; ADD 2 TO STARTING ADDRESS IN SIEVE ARRAY
   ; STARTING ADDRESS IS THE ADDRESS OF THE
   ; CANDIDATE PRIME NUMBER PRM
   LHLD ADDR
   MVI C,2H
   MVI B,0H
   DAD B
   SHLD ADDR
   ; NO MORE ODD PRIME NUMBERS TO PROCESS?
   LHLD ADDR
   INX H
   MOV A,M
   CPI 0FFH
   JZ XOUT4           ; YES, PRINT OUT PRIME NUMBER LIST
XOT3B:            ; NO, IS NEW PRIME NUMBER CANDIDATE ZERO?
   LHLD ADDR
   MOV A,M
   ORA A
   JNZ XOUT1          ; NO, ZERO OUT ITS MULTIPLES
   INX H
   MOV A,M
   ORA A
   JNZ XOUT1          ; NO, ZERO OUT ITS MULTIPLES
   ;
   JMP XOUT3          ; YES, TRY THE NEXT CANDIDATE PRIME
; END OF XOUT ROUTINE, NOW PRINT OUT PRIME NUMBER LIST
XOUT4:                ; END OF XOUT ROUTINE
   POP H
   POP B
   POP PSW
   RET
;-------------------------------------------------------
; ROUTINE TO PRINT PRIME NUMBER LIST IN DECIMAL
; BYPASS ALL ODD NUMBERS ON LIST THAT ARE ZEROED OUT
;-------------------------------------------------------
SHW:
   PUSH PSW
   PUSH B
   PUSH H
   ; PRINT THE LOWEST 4 PRIME NUMBERS BY HAND: 2,3,5,7
   ; TO EASE THE DIVISION LOGIC IN PDEC
   MVI A,'2'
   CALL COUTSPC
   MVI A,'3'
   CALL COUTSPC
   MVI A,'5'
   CALL COUTSPC
   MVI A,'7'
   CALL COUTSPC
   LXI H,SV+8           ; POINT TO #11 IN SIEVE LIST
   SHLD ADDR
; MAIN PRINT LOOP
; ONE ITERATION FOR EACH VALID PRIME NUMBER
SHW2:
   LHLD ADDR         ; POINT TO CURRENT NUMBER IN LIST
   ; THE BC REGISTER CONTAINS THE PRIME NUMBER
   MOV C,M
   INX H
   MOV B,M
   INX H
   SHLD ADDR         ; POINT TO NEXT NUMBER IN LIST
   ; IS THE CANDIDATE PRIME NUMBER ZERO?
   MOV A,C
   ORA A
   JNZ SHW3          ; NO, CHECK FOR END OF LIST
   MOV A,B
   ORA A
   JZ SHW2           ; YES, CHECK IF END OF LIST
; CHECK IF END OF LIST IS REACHED 
SHW3:
   MOV A,B
   CPI 0FFH            ; END OF LIST?
   JZ SHW5             ; YES, GO TO END OF JOB
   CALL PDEC           ; NO, PRINT BC REGISTER IN DECIMAL
   MOV A,B
   CPI 0FFH            ; END OF LIST? (REDUNDANT CHECK)
   JNZ SHW2            ; NO, PRINT NEXT PRIME
SHW5:                  ; END OF PRIME NUMBER LIST
   POP H
   POP B
   POP PSW
   RET
;-------------------------------------------------------
; DEBUG ROUTINE TO PRINT SIEVE ARRAY IN HEX
;-------------------------------------------------------
DMPSV:
   PUSH PSW
   PUSH B
   PUSH H
   LXI H,SV
   SHLD DMPADR
DPSV1:
   LHLD DMPADR
   MOV A,M
   MOV B,A
   INX H
   MOV A,M
   CPI 0FFH
   JZ DPSV2
   CALL PUTHEX
   MOV A,B
   CALL PUTHEXA
   CALL PAUSE
   INX H
   SHLD DMPADR
   JMP DPSV1
DPSV2:
   LHLD DMPADR
   MOV A,M
   MOV B,A
   INX H
   MOV A,M
   CALL PUTHEX
   MOV A,B
   CALL PUTHEX
   CALL PUTEOL
   MVI A,'D'
   CALL COUT
   CALL PUTEOL
   CALL PAUSE
   POP H
   POP B
   POP PSW
   RET
;-------------------------------------------------------
; PRINT ADDR VARIABLE IN HEX, FOLLOWED BY SPACE
;-------------------------------------------------------
PUTADDR:
   PUSH PSW
   LDA ADDR+1
   CALL PUTHEX
   LDA ADDR
   CALL PUTHEXA
   POP PSW
   RET
;-------------------------------------------------------
; PRINT CURADR POINTER AND DATA THAT IT POINTS TO
;-------------------------------------------------------
PUTCURADR:
   PUSH PSW
   PUSH B
   PUSH H
   LDA CURADR+1
   CALL PUTHEX
   LDA CURADR
   CALL PUTHEXA
   LHLD CURADR
   MOV A,M
   MOV B,A
   INX H
   MOV A,M
   CALL PUTHEX
   MOV A,B
   CALL PUTHEXA
   CALL PAUSE
   POP H
   POP B
   POP PSW
   RET
;-------------------------------------------------------
; PRINT CONTENTS OF PRM VARIABLE
;-------------------------------------------------------
PUTPRM:
   PUSH PSW
   LDA PRM+1
   CALL PUTHEX
   LDA PRM
   CALL PUTHEXA
   POP PSW
   RET
;-------------------------------------------------------
; PRINT CONTENTS OF GAP VARIABLE
;-------------------------------------------------------
PUTGAP:
   PUSH PSW
   LDA GAP+1
   CALL PUTHEX
   LDA GAP
   CALL PUTHEXA
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
; THE NUMBER TO PRINT IS IN THE BC REGISTER
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
   ; STORE BC REGISTER IN VARIABLE NUM
   MOV A,C
   STA NUM
   MOV A,B
   STA NUM+1
   ; POINT TO EMPTY STACK
   LXI H,STK
   SHLD STKADR
   ; ONLY 4 DIGITS ARE PUSHED ONTO THE STACK
   MVI A,4
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
   ; 4 DIGITS ON STACK?
   LDA STKSZ
   DCR A
   STA STKSZ
   ORA A
   JNZ PDEC2         ; NO, DIVIDE BY 10 AGAIN
; YES, BYPASS LEADING ZEROS
PDEC3:
   LDA STK+3
   ORA A
   JNZ PDEC4
   LDA STK+2
   ORA A
   JNZ PDEC5
   LDA STK+1
   ORA A
   JNZ PDEC6
; AFTER BYPASSING LEADING ZEROS
; PRINT HIGH ORDER THOUSANDS DIGIT
PDEC4:
   LDA STK+3
   ADI 030H
   CALL COUT
; PRINT HIGH ORDER HUNDREDS DIGIT
PDEC5:
   LDA STK+2
   ADI 030H
   CALL COUT
; PRINT HIGH ORDER TENS DIGIT
PDEC6:
   LDA STK+1
   ADI 030H
   CALL COUT
; PRINT LOW ORDER UNITS DIGIT
   LDA STK
PDEC9:
   ADI 030H
   CALL COUT
   CALL PUTSPC          ; PRINT SPACE
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
PUTSTK:
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
; READ KEYBOARD WITH WAIT, WITHOUT ECHO
;----------------------------------------------------------
CIN:
   PUSH B
   PUSH D
   LXI D,KCIN
   CALL IOS
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

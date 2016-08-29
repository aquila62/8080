; CRC16.ASM - CCITT VERSION OF CRC16  VERSION 1.0.0
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
; THIS PROGRAM PERFORMS THE CCITT VERSION OF A 16 BIT CRC.
; CRC STANDS FOR CYCLICAL REDUNDANCY CHECK.
; IT IS USED FOR VALIDATING DATA TRANSMITTED OVER A
; TRANSMISSION LINE.
;
; USAGE:
;
; CRC16 STRING
;
; EXAMPLE:
;
; CRC16 123456789
; CRC16 THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG
;
; THIS PROGRAM PRINTS THE CRC AFTER EACH CHARACTER
; IN THE PARAMETER STRING IS READ.
;--------------------------------------------------------------

KCIN   EQU 0006H       ; CP/M JUMP VECTOR FOR KEY INPUT
KCOUT  EQU 0009H       ; CP/M JUMP VECTOR FOR CONSOLE OUTPUT
   ORG 100H            ; CP/M LOADS PROGRAM INTO TPA AT 100H
   JMP STRT            ; BYPASS DATA AREA
BYT:    DB 0           ; BYTE USED FOR BUILDING THE TABLE
KOUNT:  DB 0           ; LOOP COUNTER 8..1
LEN:    DB 0           ; LENGTH OF PARAMETER STRING
INDX:   DW 0           ; INDEX INTO THE CRC TABLE
CEE:    DW 0           ; WORD USED FOR BUILDING THE TABLE
POLY:   DW 1021H       ; CCITT POLYNOMIAL X^16 + X^12 + X^5 + 1
CRC:    DW 0           ; 16 BIT CRC
STRADR: DW 0           ; CURRENT POINTER IN PARAMETER STRING
TBLADR: DW 0           ; CURRENT POINTER IN CRC TABLE
TBLWRD: DW 0           ; CURRENT ENTRY IN CRC TABLE
STR:    DS 128         ; COPY OF PARAMETER STRING
TBL:    DS 512         ; 256 WORDS, CRC TABLE
;---------------------------------------------------
; TRANSLATE TABLE FOR PRINTING A 4-BIT NYBBLE IN HEX
;---------------------------------------------------
HXTBL:  DB '0123456789ABCDEF'
;---------------------------------------------------
STRT:                  ; PROGRAM STARTS HERE
   CALL BLD            ; BUILD CRC TABLE
   CALL GETPARM        ; COPY PARM STRING
   LDA LEN             ; IS PARM STRING EMPTY
   ORA A
   JZ EOJ              ; YES, TERMINATE
   MVI A,0FFH          ; INITIAL CRC VALUE IS 0FFFFH
   STA CRC
   STA CRC+1
   CALL CALC           ; CALCULATE THE CRC OF THE PARM STRING
   CALL PUTCRC         ; PRINT CRC IN HEX
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
;-----------------------------------------------
; CALCULATE THE 16 BIT CRC OF THE PARM STRING
;-----------------------------------------------
CALC:
   PUSH PSW
   PUSH H
   LXI H,STR           ; HL POINTS TO START OF STRING
   SHLD STRADR         ; SAVE CURRENT ADDRES WITHIN STRING
CALC2:
   MOV A,M             ; A = CURRENT CHARACTER IN STRING
   STA BYT             ; SAVE IN BYT
   CALL UPDT           ; UPDATE THE CRC
   INX H               ; POINT TO NEXT CHARACTER IN STRING
   LDA LEN             ; DECREMENT THE LOOP COUNTER
   DCR A
   STA LEN
   ORA A               ; LOOP COUNTER IS ZERO?
   JNZ CALC2           ; NO, REPEAT LOOP FOR REST OF STRING
   POP H
   POP PSW
   RET
;----------------------------------------------------
; UPDATE THE CRC BASED ON CURRENT CHARACTER IN STRING
;----------------------------------------------------
UPDT:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A              ; A = ZERO
   STA CEE+1          ; UPPER 8 BITS OF CEE IS ZERO
   LDA BYT            ; LOWER 8 BITS OF CEE IS CHARACTER
   STA CEE            ; CEE = CURRENT CHARACTER AS 16 BIT NUMBER
   LDA CEE            ; LOAD CURRENT CHARACTER INTO B REG
   MOV B,A
   LDA CRC+1          ; A = UPPER 8 BITS OF CRC
   XRA B              ; XOR CRC>>8 ^ CEE
   STA INDX           ; RESULT IS INDEX INTO THE CRC TABLE
   XRA A              ; UPPER 8 BITS OF INDEX IS ZERO
   STA INDX+1
   LDA CRC            ; CRC = CRC << 8
   STA CRC+1
   XRA A
   STA CRC
   LHLD INDX          ; BC = INDEX * 2
   MOV B,H
   MOV C,L
   DAD B
   MOV B,H
   MOV C,L
   LXI H,TBL         ; HL = TABLE[INDEX]
   DAD B             ; HL POINTS TO ENTRY IN TABLE
   MOV A,M           ; LOW ORDER 8 BITS OF TABLE[INDEX]
   STA TBLWRD
   MOV B,A
   LDA CRC           ; LOW 8 BITS OF CRC
   XRA B             ; CRC = (CRC>>8) ^ TABLE[INDEX]
   STA CRC           ; SAVE THE NEW CRC
   INX H
   MOV A,M
   STA TBLWRD+1
   MOV B,A
   LDA CRC+1
   XRA B             ; UPPER 8 BITS OF CRC
   STA CRC+1         ; CRC = (CRC >> 8) ^ TABLE[INDEX]
   POP H
   POP B
   POP PSW
   RET
;-----------------------------------------------
; BUILD THE CCITT CRC TABLE
; THE TABLE SPEEDS UP THE COMPUTATION OF THE CRC
;-----------------------------------------------
BLD:
   PUSH PSW
   PUSH B
   PUSH D
   PUSH H
   XRA A              ; BYT GOES 0..255
   STA BYT
   LXI H,TBL          ; TABADR IS CURRENT POSITION IN TABLE
   SHLD TBLADR
BLD2:                 ; OUTER LOOP FOR EACH ENTRY IN TABLE
   XRA A
   STA CRC
   STA CRC+1          ; SET CRC = 0
   STA CEE
   LDA BYT
   STA CEE+1          ; CEE = BYT IN HIGH ORDER 8 BITS
   MVI A,8H
   STA KOUNT          ; LOOP COUNTER = 8
BLD3:                 ; INNER LOOP BASED ON 8 BITS OF BYTE
   LDA (CEE+1)        ; XOR HIGH ORDER BIT OF CEE AND CRC
   MOV B,A
   LDA (CRC+1)
   XRA B
   ANI 80H
   ORA A              ; IS HIGH ORDER BIT ZERO?
   JZ BLD4            ; YES, GO TO BLD4
   STC                ; NO, HIGH ORDER BIT IS 1
   CMC                ; SHIFT CRC 1 BIT TO LEFT
   LDA CRC
   RAL
   STA CRC
   LDA CRC+1
   RAL
   STA CRC+1
   LDA POLY           ; XOR CRC AND POLYNOMIAL
   MOV B,A
   LDA CRC
   XRA B
   STA CRC
   LDA POLY+1
   MOV B,A
   LDA CRC+1
   XRA B
   STA CRC+1
   JMP BLD5          ; GO TO END OF LOOP TEST
BLD4:                ; HIGH ORDER BIT IS ZERO
   STC               ; SHIFT CRC 1 BIT TO LEFT
   CMC
   LDA CRC
   RAL
   STA CRC
   LDA CRC+1
   RAL
   STA CRC+1
BLD5:                ; END OF INNER LOOP
   STC               ; SHIFT CEE 1 BIT TO LEFT
   CMC
   LDA CEE
   RAL
   STA CEE
   LDA CEE+1
   RAL
   STA CEE+1
   LDA KOUNT         ; DECREMENT LOOP COUNTER
   DCR A
   STA KOUNT
   ORA A             ; LOOP COUNTER = ZERO?
   JNZ BLD3          ; NO, REPEAT LOOP 8 TIMES
   LHLD TBLADR       ; BC = ADDRESS OF ENTRY IN TABLE
   MOV B,H
   MOV C,L
   LDA CRC           ; STORE CRC IN TABLE
   STAX B
   INX B
   LDA CRC+1
   STAX B
   LHLD TBLADR       ; ADD 2 TO TABLE POINTER
   MVI C,2H
   XRA A
   MOV B,A
   DAD B
   SHLD TBLADR       ; SAVE NEW TABLE POINTER
   LDA BYT           ; TEST IF END OF OUTER LOOP
   CPI 0FFH
   JZ BLD6           ; YES, TERMINATE
   INR A             ; NO, INCREASE BYTE BY ONE
   STA BYT
   JMP BLD2          ; REPEAT OUTER LOOP 256 TIMES
BLD6:
   POP H
   POP D
   POP B
   POP PSW
   RET
;---------------------------------------------------------
; READ PARAMETER FROM INPUT BUFFER AREA AT 80H
; THE FORMAT IS:
; LENGTH BYTE
; SPACE DELIMITED STRING
;---------------------------------------------------------
GETPARM:
   PUSH PSW
   PUSH B
   PUSH D
   PUSH H
   LDA 80H          ; GET PARAMETER LENGTH
   STA LEN          ; SAVE IN LOOP COUNTER
   ORA A            ; EMPTY PARAMETER
   JZ GETP9         ; YES, QUIT
   MOV D,A          ; NO, D = LOOP COUNTER
   LXI B,STR        ; COPY HL TO BC (BUFFER TO WORK AREA)
   LXI H,81H        ; HL = BUFFER
GETP2:              ; LEADING SPACE LOOP
   MOV A,M          ; A = CHARACTER IN PARAMETER
   CPI 20H          ; IS IT A LEADING SPACE?
   JNZ GETP3        ; NO, COPY REST OF STRING
   LDA LEN          ; YES, DECREMENT LENGTH OF PARM
   DCR A
   STA LEN
   INX H            ; POINT TO NEXT CHARACTER
   DCR D            ; DECREMENT LOOP COUNTER
   MOV A,D          ; IS LOOP COUNTER ZERO?
   ORA A
   JZ GETP9         ; YES, FINISH
   JMP GETP2        ; NO, REPEAT LEADING SPACE LOOP
GETP3:              ; PARM STRING LOOP
   MOV A,M          ; GET CHARACTER IN PARM
   STAX B           ; SAVE IT IN WORK AREA
   INX H            ; POINT TO NEXT PARM CHARACTER
   INX B            ; POINT TO NEXT WORK AREA BYTE
   DCR D            ; DECREMENT THE LOOP COUNTER
   JNZ GETP3        ; REPEAT IF NOT ZERO
GETP9: 
   POP H
   POP D
   POP B
   POP PSW
   RET
;---------------------------------------------------------
; DEBUGGING ROUTINE:
; DUMP INPUT PARAMETER BUFFER IN HEX
;---------------------------------------------------------
DPBF:
   PUSH PSW
   PUSH B
   PUSH H
   LXI H,080H
   MOV B,M
   INX H
DPBF2:
   MOV A,M
   CALL PUTHEXA
   INX H
   DCR B
   MOV A,B
   ORA A
   JNZ DPBF2
   CALL PUTEOL
   POP H
   POP B
   POP PSW
   RET
;-------------------------------------------------------
; PRINT DASHES TO DELIMIT THE STATE PRINT OUT
;-------------------------------------------------------
PDSH:
   PUSH PSW
   PUSH B
   MVI B,10          ; PRINT 10 DASHES
PDSH2:
   MVI A,'-'
   CALL COUT
   DCR B
   MOV A,B
   CPI 0
   JNZ PDSH2
   CALL PUTEOL       ; PRINT END OF LINE
   POP B
   POP PSW
   RET
;---------------------------------------------------------
; PAUSE FOR KEYBOARD INPUT, QUIT IF 'Q'
;---------------------------------------------------------
PAUSE:
   PUSH PSW
   CALL CIN
   CPI 01AH
   JZ EOJ
   CPI 'Q'
   JZ EOJ
   POP PSW
   RET
;----------------------------------------------------------
; PRINT 16 BIT CRC IN HEX FOLLOWED BY ONE SPACE
;----------------------------------------------------------
PUTCRC:
   PUSH PSW
   LDA CRC+1
   CALL PUTHEX
   LDA CRC
   CALL PUTHEX
   CALL PUTSPC
   POP PSW
   RET
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
   MOV B,A
   ; SHIFT A REGISTER 4 BITS TO RIGHT
   ; 8080 DOES NOT HAVE A SHIFT INSTRUCTION
   STC
   CMC
   RAR
   STC
   CMC
   RAR
   STC
   CMC
   RAR
   STC
   CMC
   RAR
   CALL PUTNBL      ; PRINT THE HIGH ORDER 4 BITS IN HEX
   MOV A,B
   ANI 0FH
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
   MVI B,0
   MOV C,A
   LXI H,HXTBL
   DAD B
   MOV A,M
   CALL COUT
   POP H
   POP B
   POP PSW
   RET
;----------------------------------------------------------
; PRINT \R\N END OF LINE SEQUENCE
;----------------------------------------------------------
PUTEOL:
   PUSH PSW
   MVI A,13
   CALL COUT
   MVI A,10
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
; CP/M INPUT/OUTPUT BRANCH VECTOR
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
   END

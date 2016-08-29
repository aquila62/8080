; CRCNT.ASM - CCITT VERSION OF CRC16  VERSION 1.0.0
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
; CRC STANDS FOR CYCLIC REDUNDANCY CHECK.
; IT IS USED FOR VALIDATING DATA TRANSMITTED OVER A
; TRANSMISSION LINE.
;
; THIS PROGRAM DOES NOT USE A CRC TABLE.
;
; USAGE:
;
; CRCNT STRING
;
; EXAMPLE:
;
; CRCNT 123456789
; CRCNT THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG
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
POLY:   DW 1021H       ; CCITT POLYNOMIAL X^16 + X^12 + X^5 + 1
CRC:    DW 0           ; 16 BIT CRC
STRADR: DW 0           ; CURRENT POINTER IN PARAMETER STRING
STR:    DS 128         ; COPY OF PARAMETER STRING
;---------------------------------------------------
; TRANSLATE TABLE FOR PRINTING A 4-BIT NYBBLE IN HEX
;---------------------------------------------------
HXTBL:  DB '0123456789ABCDEF'
;---------------------------------------------------
STRT:                  ; PROGRAM STARTS HERE
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
   PUSH D
   PUSH H
   LDA BYT            ; CURRENT CHARACTER
   MOV B,A
   LDA CRC+1          ; A = UPPER 8 BITS OF CRC
   XRA B              ; XOR CRC ^ (BYTE << 8)
   STA CRC+1
   LHLD CRC           ; DE CONTAINS THE CRC
   MOV D,H
   MOV E,L
   MVI B,8H           ; B CONTAINS THE LOOP COUNTER
UPDT2:                ; LOOP FOR EACH BIT IN BYTE
   PUSH B             ; SAVE THE LOOP COUNTER
   MOV A,D            ; HIGH ORDER BIT OF CRC
   MVI B,80H
   ANA B              ; IS HIGH ORDER BIT 1?
   JZ UPDT3           ; NO, JUST SHIFT THE CRC << 1
   STC                ; YES, CLEAR THE CARRY FLAG
   CMC
   MOV A,E            ; SHIFT CRC 1 BIT TO LEFT
   RAL
   MOV E,A
   MOV A,D
   RAL
   MOV D,A            ; CRC HAS BEEN SHIFTED TO LEFT
   MOV A,E            ; XOR CRC WITH POLYNOMIAL 0X1021
   MVI B,21H
   XRA B
   MOV E,A
   MOV A,D
   MVI B,10H
   XRA B
   MOV D,A
   JMP UPDT4          ; PROCESS NEXT ITERATION
UPDT3:                ; HIGH ORDER BIT OF CRC = 0
   STC                ; CLEAR CARRY FLAG
   CMC
   MOV A,E            ; SHIFT CRC 1 BIT TO LEFT
   RAL
   MOV E,A
   MOV A,D
   RAL
   MOV D,A            ; CRC SHIFTED 1 BIT TO LEFT
UPDT4:                ; NEXT ITERATION
   POP B              ; RESTORE LOOP COUNTER
   DCR B              ; DECREMENT LOOP COUNTER
   JNZ UPDT2          ; ITERATE 8 TIMES
   MOV A,E            ; STORE DE IN CRC, RETURN TO CALC
   STA CRC
   MOV A,D
   STA CRC+1
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
; PRINT DE REGISTER IN HEXADECIMAL
;----------------------------------------------------------
PUTDE: PUSH PSW
   MOV A,D
   CALL PUTHEX
   MOV A,E
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
]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
#INCLUDE <STDIO.H>
#INCLUDE <STRING.H>

INT CALCRC(CHAR *PTR, INT COUNT)
   {
   INT CRC;
   CHAR I;
   CRC = 0XFFFF;
   I = COUNT;
   DO
      {
      INT J;
      CRC = CRC ^ (INT) *PTR++ << 8;
      J = 8;
      DO
         {
         IF (CRC & 0X8000)
            CRC = CRC << 1 ^ 0X1021;
         ELSE
            CRC = CRC << 1;
         } WHILE(--J);
      } WHILE (--I);
   RETURN (CRC);
   }

INT MAIN ()
   {
   INT CRC;
   CHAR STR[256];
   STRCPY(STR,"123456789");
   CRC = CALCRC(STR,STRLEN(STR));
   PRINTF("%04X\N", CRC & 0XFFFF);
   RETURN(0);
   } /* MAIN */

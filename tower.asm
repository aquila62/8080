; TOWER.ASM - TOWER OF HANOI  VERSION 1.0.0
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
; THIS PROGRAM PERFORMS THE ITERATIVE SOLUTION FOR THE TOWER
; OF HANOI COMPUTER PUZZLE.
;
; THE SOURCE TOWER A HAS 2-9 DISKS.
; THE TARGET TOWER IS TOWER C.
; TOWER B IS THE AUXILIARY TOWER.
;
; USAGE:
;
; TOWER [N]
;
; WHERE N IS THE NUMBER OF DISKS, 2-9
; DEFAULT IS THREE DISKS.
;
; THIS PROGRAM PRINTS IT'S STATE EVERY EVEN MOVE, 2,4,6,...
; THE NUMBER OF MOVES FOR EACH SOLUTION IS 2^N-1 WHERE N
; IS THE NUMBER OF DISKS.
; FOR AN EVEN NUMBER OF DISKS, THE 1 DISK MOVES TO THE RIGHT.
; FOR AN ODD  NUMBER OF DISKS, THE 1 DISK MOVES TO THE LEFT.
; AFTER THE 1 DISK MOVES, THERE IS EITHER ONE ALTERNATE MOVE
; LEFT, OR THERE ARE NO MOVES LEFT, AND THE SOLUTION IS REACHED.
;--------------------------------------------------------------

KCIN   EQU 0006H       ; CP/M JUMP VECTOR FOR KEY INPUT
KCOUT  EQU 0009H       ; CP/M JUMP VECTOR FOR CONSOLE OUTPUT
   ORG 100H            ; CP/M LOADS PROGRAM INTO TPA AT 100H
   JMP STRT            ; BYPASS DATA AREA
NUM:    DW 0,0         ; NUMBER TO PRINT IN DECIMAL
STKADR: DW 0,0         ; CURRENT POINTER IN STACK
STKSZ:  DB 9,0,0,0     ; MAXIMUM NUMBER OF DISKS IN THE STACK
; EACH STACK REPRESENTS A COLUMN OF DISKS IN THE REAL WORLD.
; THE ORDER OF DISKS IS LARGE TO SMALL, LEFT TO RIGHT.
; IN THE REAL WORLD, THE ORDER OF DISKS IS LARGE TO SMALL,
; BOTTOM TO TOP.
; SEE THE WIKIPEDIA ARTICLE ON THE TOWER OF HANOI FOR THE
; RULES OF THE ALGORITHM.
STKA:   DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
STKB:   DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
STKC:   DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
; SZ* IS THE SIZE OF EACH STACK.
; SZ* = 0 MEANS THAT THE STACK IS EMPTY.
; SZ* IS ALSO THE INDEX FOR THE TOP OF THE STACK.
; THE STACK IS POPULATED FROM INDEX 1 TO INDEX N.
; INDEX ZERO IS THE HEADER AND DOES NOT CONTAIN A DISK.
SZA:    DB 9,0
SZB:    DB 0,0
SZC:    DB 0,0
POPDSK: DB 0,0     ; CURRENT DISK POPPED FROM A STACK
; THE SOURCE DISK IS COMPARED TO THE TARGET DISK
; IF THE SOURCE DISK IS SMALLER THAN THE TARGET DISK,
; THEN THE SOURCE DISK IS PUSHED ON TOP OF THE TARGET STACK
; THE 1 DISK IS THE SMALLEST AND THE N DISK IS THE LARGEST.
SRCDSK: DB 0,0     ; THE SOURCE DISK THAT WILL BE PUSHED
TGTDSK: DB 0,0     ; THE DISK ON TOP OF THE TARGET STACK
RETC1:  DB 0,0     ; RETURN CODE FROM THE 1 MOVE
RETC2:  DB 0,0     ; RETURN CODE FROM THE ALTERNATE MOVE
KOUNT:  DW 0,0     ; MOVE COUNT
; 8 BIT DIVISION IS USED TO CONVERT FROM BINARY TO DECIMAL
; THE DIVISOR IS ALWAYS A CONSTANT 10
; THE QUOTIENT BECOMES THE DIVIDEND IN THE NEXT ITERATION
; OF THE ALGORITHM.
;-------------------- 8 BIT DIVISION
DVDND:     DW 0,0         ; DIVIDEND
TEN:       DB 10,0        ; CONSTANT 10
DIVISOR:   DB 0,0
QUOTIENT:  DW 0,0        
REMAINDER: DB 0,0,0,0     ; FINAL REMAINDER
RMDR:      DB 0,0,0,0     ; TEMPORARY REMAINDER
;---------------------------------------------------
; TRANSLATE TABLE FOR PRINTING A 4-BIT NYBBLE IN HEX
;---------------------------------------------------
HXTBL:  DB '0123456789ABCDEF'
; DECIMAL NUMBERS ARE PRINTED FROM A STACK 
STK: DS 16             ; DECIMAL NUMBER STACK
;---------------------------------------------------
STRT:                  ; PROGRAM STARTS HERE
   ; INITIALIZE THE MOVE COUNTER TO ZERO
   XRA A               ; A = 0
   STA KOUNT           ; SET 16-BIT KOUNT TO ZERO
   STA KOUNT+1
   ;----------------------------------------------------
   ; THE DEFAULT NUMBER OF DISKS IS 3
   CALL GETPARM        ; OPTIONAL PARM IS NUMBER OF DISKS
   CALL BLD            ; POPULATE THE FIRST TOWER WITH N DISKS
   ;----------------------------------------------------
   LDA STKSZ           ; IS NUMBER OF DISKS IS EVEN?
   CPI 2
   JZ MAIN2            ; YES, MOVE RIGHT
   CPI 4
   JZ MAIN2            ; YES, MOVE RIGHT
   CPI 6
   JZ MAIN2            ; YES, MOVE RIGHT
   CPI 8
   JZ MAIN2            ; YES, MOVE RIGHT
   CALL MVLF           ; NO, MOVE DISKS TO LEFT
   CALL SHW            ; PRINT FINAL DISK COLUMN
   JMP EOJ
MAIN2:
   CALL MVRT           ; MOVE DISKS TO RIGHT
   CALL SHW            ; PRINT FINAL DISK COLUMN
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
; POPULATE STACK A WITH N DISKS
; WHERE N DEFAULTS TO 3
; OR N IS THE RUN PARAMETER, 2-9
; STACKS B AND C ARE LEFT EMPTY
;---------------------------------------------
BLD:
   PUSH PSW
   PUSH B
   PUSH H
   ; 9 IS BIGGEST, 2 IS SMALLEST
   ; SOURCE = STKA, TARGET = STKC, AUXILIARY = STKB
   ; RUNTIME PARAMETER DETERMINES HOW MANY DISKS
   LXI H,STKA+1   ; INDEX 0 IS NOT USED
   LDA STKSZ      ; TOTAL NUMBER OF DISKS
   STA SZA        ; SET COLUMN A TO ALL THE DISKS
BLD2:
   MOV M,A        ; REG A CONTAINS THE DISK NUMBER
   INX H          ; REG HL POINTS TO THE TOP OF STACK A
   DCR A
   ORA A
   JNZ BLD2       ; LOOP N TIMES
   CALL SHW       ; PRINT INITIAL STATE OF PUZZLE
   POP H
   POP B
   POP PSW
   RET
;-------------------------------------------------------
; MOVE THE DISKS TO THE LEFT
; FIRST MOVE THE ONE DISK TO THE LEFT
; THEN MOVE ALTERNATE DISK TO AN AVAILABLE PILE
; TOTAL MOVES IS 2^N - 1, WHERE N IS #DISKS
;-------------------------------------------------------
MVLF:
   PUSH PSW
   PUSH B
   PUSH H
; IF A AND B ARE EMPTY,
; THEN THE MOVE FROM A TO C IS COMPLETE
; AND THE PUZZLE IS SOLVED
; EACH ITERATION IN THE MOVE LOOP CONTAINS A PAIR
; OF MOVES.
; THE FIRST MOVE IS FOR THE 1 DISK
; THE SECOND MOVE IS FOR THE ALTERNATE DISK
; THERE IS ONLY ONE VALID MOVE FOR THE ALTERNATE DISK
; OTHERWISE THE ALGORITHM IS COMPLETE
MVLF2:            ; MOVE LOOP FOR A PAIR OF MOVES
   LDA SZA
   ORA A          ; IS STACK A EMPTY?
   JNZ MVL2B      ; NO, MOVE THE 1 DISK
   LDA SZB        ; YES, LOOK AT STACK B
   ORA A          ; STACK B EMPTY?
   JZ MVLF9       ; YES, JOB DONE
MVL2B:            ; NO, MOVE THE 1 DISK FIRST
   LHLD KOUNT     ; INCREASE MOVED COUNT FOR THE 1 MOVE
   INX H
   SHLD KOUNT
   CALL M1AC      ; TRY MOVING THE 1 DISK FROM A TO C
   LDA RETC1      ; SUCCESSFUL MOVE?
   ORA A
   JNZ MVLF3      ; YES, PRINT STATE
   CALL M1CB      ; NO, TRY MOVING THE 1 DISK FROM C TO B
   LDA RETC1      ; SUCCESSFUL MOVE?
   ORA A
   JNZ MVLF3      ; YES, PRINT STATE
   CALL M1BA      ; NO, TRY MOVING THE 1 DISK FROM B TO A
MVLF3:
   CALL PTKT      ; PRINT MOVE KOUNT
   CALL SHW       ; PRINT STATE
   CALL PAUSE     ; CHECK FOR QUIT KEY
   JMP MVLF2      ; CONTINUE WITH NEXT PAIR OF MOVES
MVLF9:            ; END OF JOB, RETURN TO MAIN LINE
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------
; MOVE THE DISKS TO THE RIGHT
; FIRST MOVE THE ONE DISK TO THE RIGHT
; THEN MOVE ALTERNATE DISK TO AN AVAILABLE PILE
; TOTAL MOVES IS 2^N - 1, WHERE N IS #DISKS
;---------------------------------------------
MVRT:
   PUSH PSW
   PUSH B
   PUSH H
; IF A AND B ARE EMPTY,
; THEN THE MOVE FROM A TO C IS COMPLETE
; AND THE PUZZLE IS SOLVED
; EACH ITERATION IN THE MOVE LOOP CONTAINS A PAIR
; OF MOVES.
; THE FIRST MOVE IS FOR THE 1 DISK
; THE SECOND MOVE IS FOR THE ALTERNATE DISK
; THERE IS ONLY ONE VALID MOVE FOR THE ALTERNATE DISK
; OTHERWISE THE ALGORITHM IS COMPLETE
MVRT4:            ; MOVE LOOP FOR A PAIR OF MOVES
   LDA SZA
   ORA A          ; IS STACK A EMPTY?
   JNZ MVR4B      ; NO, MOVE THE 1 DISK
   LDA SZB        ; YES, LOOK AT STACK B
   ORA A          ; STACK B EMPTY?
   JZ MVRT9       ; YES, JOB DONE
MVR4B:            ; NO, MOVE THE 1 DISK FIRST
   LHLD KOUNT     ; INCREASE MOVED COUNT FOR THE 1 MOVE
   INX H
   SHLD KOUNT
   CALL M1AB      ; TRY MOVING THE 1 DISK FROM A TO B
   LDA RETC1
   ORA A
   JNZ MVRT5      ; YES, PRINT STATE
   CALL M1BC      ; NO, TRY MOVING THE 1 DISK FROM B TO C
   LDA RETC1      ; SUCCESSFUL MOVE?
   ORA A
   JNZ MVRT5      ; YES, PRINT STATE
   CALL M1CA      ; NO, TRY MOVING THE 1 DISK FROM C TO A
MVRT5:
   CALL PTKT      ; PRINT MOVE KOUNT
   CALL SHW       ; PRINT STATE
   CALL PAUSE     ; CHECK FOR QUIT KEY
   JMP MVRT4      ; CONTINUE WITH NEXT PAIR OF MOVES
MVRT9:            ; END OF JOB, RETURN TO MAIN LINE
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------
; 1 MOVES
; THERE ARE 6 POSSIBLE 1 MOVES
; IF THE 1 MOVE IS SUCCESSFUL, THEN THE ALTERNATE
; MOVE IS ATTEMPTED.
; IF THERE IS NO ALTERNATE MOVE, THEN THE
; ALGORITHM IS COMPLETE.
;---------------------------------------------
; MOVE 1 DISK FROM A TO B
;---------------------------------------------
M1AB:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A              ; A = 0
   STA SRCDSK         ; INITIALIZE SOURCE DISK
   STA RETC1          ; INITIALIZE 1 MOVE RETURN CODE
   STA RETC2          ; INITIALIZE ALTERNATE MOVE RETURN CODE
   ; IS THE SOURCE STACK A EMPTY?
   LXI H,STKA
   MVI B,0
   LDA SZA
   ORA A
   JZ MV1AB9        ; YES, RETURN CODE DEFAULTS TO ZERO
   ; IF SOURCE STACK A IS NOT EMPTY, CHECK TO SEE IF
   ; THE TOP OF THE A STACK IS A 1 DISK
   MOV C,A
   DAD B
   MOV A,M
   CPI 1            ; 1 DISK?
   JNZ MV1AB9       ; NO, DO NOT MOVE A TO B
   STA SRCDSK       ; YES, MOVE 1 DISK FROM A TO B
   CALL POPA        ; POP THE A STACK
   CALL PUSHB       ; PUSH THE 1 DISK ONTO THE B STACK
   MVI A,1
   STA RETC1        ; SET THE 1 MOVE RETURN CODE TO ONE
   CALL MVAC        ; TRY MOVING THE ALTERNATE DISK FROM A TO C
   LDA RETC2        ; SUCCESSFUL MOVE?
   ORA A
   JNZ MV1AB9       ; YES, DONE
   CALL MVCA        ; NO, TRY MOVING THE ALTERNATE DISK FROM C TO A
MV1AB9:
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------
; ALL THE OTHER DISK MOVE PAIRS ARE SIMILAR TO MV1AB ABOVE
; 1 MOVES TO THE LEFT  ARE MV1AC, MV1CB, AND MV1BA
; 1 MOVES TO THE RIGHT ARE MV1AB, MV1BC, AND MV1CA
;---------------------------------------------
; MOVE 1 DISK FROM A TO C
;---------------------------------------------
M1AC:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A
   STA SRCDSK
   STA RETC1
   STA RETC2
   LXI H,STKA
   MVI B,0
   LDA SZA
   ORA A
   JZ MV1AC9
   MOV C,A
   DAD B
   MOV A,M
   CPI 1
   JNZ MV1AC9
   STA SRCDSK
   CALL POPA
   CALL PUSHC
   MVI A,1
   STA RETC1
   CALL MVAB
   LDA RETC2
   ORA A
   JNZ MV1AC9
   CALL MVBA
   LDA RETC2
MV1AC9:
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------
; MOVE 1 DISK FROM B TO A
;---------------------------------------------
M1BA:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A
   STA SRCDSK
   STA RETC1
   STA RETC2
   LXI H,STKB
   MVI B,0
   LDA SZB
   ORA A
   JZ MV1BA9
   MOV C,A
   DAD B
   MOV A,M
   CPI 1
   JNZ MV1BA9
   STA SRCDSK
   CALL POPB
   CALL PUSHA
   MVI A,1
   STA RETC1
   CALL MVBC
   LDA RETC2
   ORA A
   JNZ MV1BA9
   CALL MVCB
MV1BA9:
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------
; MOVE 1 DISK FROM B TO C
;---------------------------------------------
M1BC:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A
   STA SRCDSK
   STA RETC1
   STA RETC2
   LXI H,STKB
   MVI B,0
   LDA SZB
   ORA A
   JZ MV1BC9
   MOV C,A
   DAD B
   MOV A,M
   CPI 1
   JNZ MV1BC9
   STA SRCDSK
   CALL POPB
   CALL PUSHC
   MVI A,1
   STA RETC1
   CALL MVBA
   LDA RETC2
   ORA A
   JNZ MV1BC9
   CALL MVAB
MV1BC9:
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------
; MOVE 1 DISK FROM C TO A
;---------------------------------------------
M1CA:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A
   STA SRCDSK
   STA RETC1
   STA RETC2
   LXI H,STKC
   MVI B,0
   LDA SZC
   ORA A
   JZ MV1CA9
   MOV C,A
   DAD B
   MOV A,M
   CPI 1
   JNZ MV1CA9
   STA SRCDSK
   CALL POPC
   CALL PUSHA
   MVI A,1
   STA RETC1
   CALL MVCB
   LDA RETC2
   ORA A
   JNZ MV1CA9
   CALL MVBC
MV1CA9:
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------
; MOVE 1 DISK FROM C TO B
;---------------------------------------------
M1CB:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A
   STA SRCDSK
   STA RETC1
   STA RETC2
   LXI H,STKC
   MVI B,0
   LDA SZC
   ORA A
   JZ MV1CB9
   MOV C,A
   DAD B
   MOV A,M
   CPI 1
   JNZ MV1CB9
   STA SRCDSK
   CALL POPC
   CALL PUSHB
   MVI A,1
   STA RETC1
   CALL MVCA
   LDA RETC2
   ORA A
   JNZ MV1CB9
   CALL MVAC
MV1CB9:
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------
; BELOW ARE THE ALTERNATE MOVES
; FOR EACH MOVE, THERE IS ONLY ONE VALID ALTERNATE MOVE
; OR THE ALGORITHM IS COMPLETE
; RETURN CODE 2 IS ZERO IF NOT SUCCESSFUL
; RETURN CODE 2 IS ONE IF SUCCESSFUL
;---------------------------------------------
; MOVE NEXT DISK FROM A TO B
;---------------------------------------------
MVAB:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A            ; A = 0
   STA SRCDSK       ; INITIALIZE SOURCE DISK TO ZERO
   STA TGTDSK       ; INITIALIZE TARGET DISK TO ZERO
   STA RETC2        ; RETURNN CODE 2 DEFAULTS TO ZERO
   LXI H,STKA       ; IS STACK A EMPTY?
   MVI B,0
   LDA SZA
   ORA A
   JZ MVAB3         ; YES, DO NOT MAKE ALTERNATE MOVE A TO B
   MOV C,A
   DAD B
   MOV A,M
   ORA A            ; IS THE SOURCE DISK 1-N?
   JZ MVAB3         ; NO, DO NOT MAKE ALTERNATE MOVE A TO B
   STA SRCDSK       ; SAVE THE SOURCE DISK
   LXI H,STKB
   MVI B,0
   LDA SZB
   MOV C,A
   DAD B
   MOV A,M
   STA TGTDSK       ; SAVE THE TARGET DISK
   LDA TGTDSK
   ORA A
   JZ MVAB2
   MOV B,A
   LDA SRCDSK
   CMP B            ; SOURCE DISK GREATER THAN TARGET DISK?
   JP MVAB3         ; YES, DO NOT MOVE A TO B
MVAB2:              ; NO, SOURCE DISK SMALLER THAN TARGET DISK
   CALL POPA        ; POP THE SOURCE DISK STACK
   LDA SRCDSK
   CALL PUSHB       ; PUSH SOURCE DISK ONTO TARGET STACK
   MVI A,1
   STA RETC2        ; RETURN CODE 2 IS SUCCESSFUL
   LHLD KOUNT       ; ADD 1 TO THE MOVE COUNT
   INX H
   SHLD KOUNT
MVAB3:
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------
; MOVE NEXT DISK FROM A TO C
;---------------------------------------------
MVAC:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A
   STA SRCDSK
   STA TGTDSK
   STA RETC2
   LXI H,STKA
   MVI B,0
   LDA SZA
   ORA A
   JZ MVAC3
   MOV C,A
   DAD B
   MOV A,M
   ORA A
   JZ MVAC3
   STA SRCDSK
   LXI H,STKC
   MVI B,0
   LDA SZC
   MOV C,A
   DAD B
   MOV A,M
   STA TGTDSK
   LDA TGTDSK
   ORA A
   JZ MVAC2
   MOV B,A
   LDA SRCDSK
   CMP B
   JP MVAC3
MVAC2:
   CALL POPA
   LDA SRCDSK
   CALL PUSHC
   MVI A,1
   STA RETC2
   LHLD KOUNT
   INX H
   SHLD KOUNT
MVAC3:
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------
; MOVE NEXT DISK FROM B TO A
;---------------------------------------------
MVBA:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A
   STA SRCDSK
   STA TGTDSK
   STA RETC2
   LXI H,STKB
   MVI B,0
   LDA SZB
   ORA A
   JZ MVBA3
   MOV C,A
   DAD B
   MOV A,M
   ORA A
   JZ MVBA3
   STA SRCDSK
   LXI H,STKA
   MVI B,0
   LDA SZA
   MOV C,A
   DAD B
   MOV A,M
   STA TGTDSK
   LDA TGTDSK
   ORA A
   JZ MVBA2
   MOV B,A
   LDA SRCDSK
   CMP B
   JP MVBA3
MVBA2:
   CALL POPB
   LDA SRCDSK
   CALL PUSHA
   MVI A,1
   STA RETC2
   LHLD KOUNT
   INX H
   SHLD KOUNT
MVBA3:
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------
; MOVE NEXT DISK FROM B TO C
;---------------------------------------------
MVBC:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A
   STA SRCDSK
   STA TGTDSK
   STA RETC2
   LXI H,STKB
   MVI B,0
   LDA SZB
   ORA A
   JZ MVBC3
   MOV C,A
   DAD B
   MOV A,M
   ORA A
   JZ MVBC3
   STA SRCDSK
   LXI H,STKC
   MVI B,0
   LDA SZC
   MOV C,A
   DAD B
   MOV A,M
   STA TGTDSK
   LDA TGTDSK
   ORA A
   JZ MVBC2
   MOV B,A
   LDA SRCDSK
   CMP B
   JP MVBC3
MVBC2:
   CALL POPB
   LDA SRCDSK
   CALL PUSHC
   MVI A,1
   STA RETC2
   LHLD KOUNT
   INX H
   SHLD KOUNT
MVBC3:
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------
; MOVE NEXT DISK FROM C TO A
;---------------------------------------------
MVCA:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A
   STA SRCDSK
   STA TGTDSK
   STA RETC2
   LXI H,STKC
   MVI B,0
   LDA SZC
   ORA A
   JZ MVCA3
   MOV C,A
   DAD B
   MOV A,M
   ORA A
   JZ MVCA3
   STA SRCDSK
   LXI H,STKA
   MVI B,0
   LDA SZA
   MOV C,A
   DAD B
   MOV A,M
   STA TGTDSK
   LDA TGTDSK
   ORA A
   JZ MVCA2
   MOV B,A
   LDA SRCDSK
   CMP B
   JP MVCA3
MVCA2:
   CALL POPC
   LDA SRCDSK
   CALL PUSHA
   MVI A,1
   STA RETC2
   LHLD KOUNT
   INX H
   SHLD KOUNT
MVCA3:
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------
; MOVE NEXT DISK FROM C TO B
;---------------------------------------------
MVCB:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A
   STA SRCDSK
   STA TGTDSK
   STA RETC2
   LXI H,STKC
   MVI B,0
   LDA SZC
   ORA A
   JZ MVCB3
   MOV C,A
   DAD B
   MOV A,M
   ORA A
   JZ MVCB3
   STA SRCDSK
   LXI H,STKB
   MVI B,0
   LDA SZB
   MOV C,A
   DAD B
   MOV A,M
   STA TGTDSK
   LDA TGTDSK
   ORA A
   JZ MVCB2
   MOV B,A
   LDA SRCDSK
   CMP B
   JP MVCB3
MVCB2:
   CALL POPC
   LDA SRCDSK
   CALL PUSHB
   MVI A,1
   STA RETC2
   LHLD KOUNT
   INX H
   SHLD KOUNT
MVCB3:
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
;-------------------------------------------------------
; PRINT KOUNT IN DECIMAL
; CONVERT THE MOVE COUNT FROM BINARY TO DECIMAL
; THE COUNT IS SUCCESSIVELY DIVIDED BY 10
; THE REMAINDER FOR EACH DIVISION IS ADDED TO A STACK
; THE DECIMAL DIGITS ARE PRINTED FROM THE STACK
; LEADING ZEROS ARE NOT PRINTED
; THE NUMBER IS FOLLOWED BY END OF LINE.
;-------------------------------------------------------
PTKT:
   PUSH PSW
   PUSH B
   PUSH H
   CALL CLRSTK
   LHLD KOUNT
   SHLD DVDND
   LDA TEN
   STA DIVISOR
   CALL DIV8
   LDA REMAINDER
   STA STK
   ;------------------
   LHLD QUOTIENT
   SHLD DVDND
   LDA TEN
   STA DIVISOR
   CALL DIV8
   LDA REMAINDER
   STA STK+1
   ;------------------
   LHLD QUOTIENT
   SHLD DVDND
   LDA TEN
   STA DIVISOR
   CALL DIV8
   LDA REMAINDER
   STA STK+2
   ;--------------------
   LDA STK+2
   ORA A
   JNZ PTKT2
   LDA STK+1
   ORA A
   JNZ PTKT3
   JMP PTKT4
PTKT2:
   LDA STK+2
   ADI 030H
   CALL COUT
PTKT3:
   LDA STK+1
   ADI 030H
   CALL COUT
PTKT4:
   LDA STK
   ADI 030H
   CALL COUT
   CALL PUTEOL
   POP H
   POP B
   POP PSW
   RET
;-------------------------------------------------------
; ROUTINE TO PRINT THREE TOWERS OF HANOI
; THIS ROUTINE PRINTS THE STATE OF THE ALGORITHM
; AFTER EACH PAIR OF MOVES
; THE FINAL STATE IS PRINTED AFTER A SINGLE 1 MOVE
; TO THE TARGET COLUMN.
;-------------------------------------------------------
SHW:
   PUSH PSW
   PUSH B
   PUSH H
   MVI A,'A'
   CALL COUTSPC
   LXI H,STKA+1
SHW2:                ; STACK LOOP FROM BOTTOM TO TOP
   MOV A,M
   ORA A             ; DISK = ZERO?
   JZ SHW2B          ; YES, END OF LOOP
   ADI 030H          ; PRINT IN ASCII
   CALL COUTSPC      ; PRINT DISK NUMBER FOLLOWED BY SPACE
   INX H             ; POINT TO NEXT DISK HIGHER ON STACK
   JMP SHW2          ; REPEAT STACK LOOP
SHW2B:
   CALL PUTEOL
   MVI A,'B'
   CALL COUTSPC
   LXI H,STKB+1
SHW3:
   MOV A,M
   ORA A
   JZ SHW3B
   ADI 030H
   CALL COUTSPC
   INX H
   JMP SHW3
SHW3B:
   CALL PUTEOL
   MVI A,'C'
   CALL COUTSPC
   LXI H,STKC+1
SHW4:
   MOV A,M
   ORA A
   JZ SHW4B
   ADI 030H
   CALL COUTSPC
   INX H
   JMP SHW4
SHW4B:
   CALL PUTEOL         ; PRINT END OF LINE AFTER EACH STACK
   CALL PDSH           ; PRINT DASHES TO DELIMIT THE STATE
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------------------
; PUSH THE A STACK FROM THE CONTENTS OF SRCDSK
;---------------------------------------------------------
PUSHA:
   PUSH PSW
   PUSH B
   PUSH H
   LXI H,STKA
   MVI B,0
   LDA SZA
   MOV C,A
   INR C
   DAD B
   LDA SRCDSK
   MOV M,A
   LDA SZA
   INR A
   STA SZA
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------------------
; PUSH THE B STACK FROM THE CONTENTS OF SRCDSK
;---------------------------------------------------------
PUSHB:
   PUSH PSW
   PUSH B
   PUSH H
   LXI H,STKB
   MVI B,0
   LDA SZB
   MOV C,A
   INR C
   DAD B
   LDA SRCDSK
   MOV M,A
   LDA SZB
   INR A
   STA SZB
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------------------
; PUSH THE C STACK FROM THE CONTENTS OF SRCDSK
;---------------------------------------------------------
PUSHC:
   PUSH PSW
   PUSH B
   PUSH H
   LXI H,STKC
   MVI B,0
   LDA SZC
   MOV C,A
   INR C
   DAD B
   LDA SRCDSK
   MOV M,A
   LDA SZC
   INR A
   STA SZC
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------------------
; POP THE A STACK INTO A VARIABLE CALLED POPDSK
; IF UNDERFLOW, GO TO END OF JOB
;---------------------------------------------------------
POPA:
   PUSH PSW
   PUSH B
   PUSH H
   LDA SZA
   ORA A                ; UNDERFLOW?
   JNZ POPA2            ; NO, POP THE STACK
   CALL COUT            ; YES, GO TO END OF JOB
   JMP EOJ
POPA2:
   LXI H,STKA
   MVI B,0
   LDA SZA
   MOV C,A
   DAD B
   MOV A,M
   STA POPDSK           ; SAVE THE POPPED DISK
   XRA A                ; MOVE ZERO TO TOP OF STACK
   MOV M,A
   LDA SZA              ; LOWER THE STACK COUNTER
   DCR A
   STA SZA
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------------------
; POP THE B STACK INTO A VARIABLE CALLED POPDSK
; IF UNDERFLOW, GO TO END OF JOB
;---------------------------------------------------------
POPB:
   PUSH PSW
   PUSH B
   PUSH H
   LDA SZB
   ORA A
   JNZ POPB2
   CALL COUT
   JMP EOJ
POPB2:
   LXI H,STKB
   MVI B,0
   LDA SZB
   MOV C,A
   DAD B
   MOV A,M
   STA POPDSK
   XRA A
   MOV M,A
   LDA SZB
   DCR A
   STA SZB
   POP H
   POP B
   POP PSW
   RET
;---------------------------------------------------------
; POP THE C STACK INTO A VARIABLE CALLED POPDSK
; IF UNDERFLOW, GO TO END OF JOB
;---------------------------------------------------------
POPC:
   PUSH PSW
   PUSH B
   PUSH H
   LDA SZC
   ORA A
   JNZ POPC2
   CALL COUT
   JMP EOJ
POPC2:
   LXI H,STKC
   MVI B,0
   LDA SZC
   MOV C,A
   DAD B
   MOV A,M
   STA POPDSK
   XRA A
   MOV M,A
   LDA SZC
   DCR A
   STA SZC
   POP H
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
   PUSH H
   MVI A,3           ; DEFAULT IS 3 DISKS
   STA STKSZ
   LXI H,080H        ; PARAMETER LENGTH AT LOCATION HEX 80
   MOV A,M
   ORA A             ; ZERO LENGTH?
   JZ GTPM9          ; YES, THERE IS NO PARAMETER
   INX H             ; POINT TO PARAMETER STRING
GTPM2:
   MOV A,M
   ORA A             ; END OF PARAMETER STRING?
   JZ GTPM9          ; YES, FINISH
   CPI 020H
   JZ GTPM3
   SUI 030H          ; CONVERT ASCII TO BINARY
   CPI 2             ; VALIDATE 2-9
   JM GTPM9          ; USE DEFAULT, IF INVALID PARAMETER
   CPI 10
   JP GTPM9
   STA STKSZ
   JMP GTPM9
GTPM3:
   INX H
   JMP GTPM2
GTPM9:
   POP H
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
; PRINT CONTENTS OF VARIABLE NUM
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
;---------------------------------------------------------
CLRSTK:
   PUSH PSW
   XRA A
   STA STK
   STA STK+1
   STA STK+2
   STA STK+3
   POP PSW
   RET
;---------------------------------------------------------
; PRINT THE DECIMAL DIGIT STACK
; IN BIG ENDIAN FORMAT
;---------------------------------------------------------
PUTSTK:
   PUSH PSW
   PUSH H
   LXI H,STK+3
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

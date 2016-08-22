; TWR.ASM - TOWER OF HANOI  VERSION 1.0.0
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
; THIS PROGRAM IMPLEMENTS THE RECURSIVE SOLUTION TO THE TOWER
; OF HANOI COMPUTER PUZZLE IN 8080 ASSEMBLER.
;
; THE SOURCE TOWER A HAS 2-9 DISKS.
; THE TARGET TOWER IS TOWER C.
; TOWER B IS THE AUXILIARY TOWER.
;
; USAGE:
;
; TWR [N]
;
; WHERE N IS THE NUMBER OF DISKS, 2-9
; DEFAULT IS THREE DISKS.
;
; THE OBJECT OF THIS PROGRAM IS TO MOVE ALL THE DISKS
; ON STACK A TO STACK C.
; SEE WIKIPEDIA FOR THE RULES ABOUT THE TOWER OF HANOI.
;--------------------------------------------------------------

KCIN   EQU 0006H       ; CP/M JUMP VECTOR FOR KEY INPUT
KCOUT  EQU 0009H       ; CP/M JUMP VECTOR FOR CONSOLE OUTPUT
   ORG 100H            ; CP/M LOADS PROGRAM INTO TPA AT 100H
   JMP STRT            ; BYPASS DATA AREA
NUM:    DW 0,0         ; NUMBER TO PRINT IN DECIMAL
STKADR: DW 0,0         ; CURRENT POINTER IN STACK
STKSZ:  DB 9,0,0,0     ; MAXIMUM NUMBER OF DISKS IN THE STACK
STKA:   DB 9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0       ; SOURCE STACK
STKB:   DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0       ; AUXILIARY STACK
STKC:   DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0       ; TARGET STACK
SZA:    DB 9,0         ; INDEX TO STACK A
SZB:    DB 0,0         ; INDEX TO STACK B
SZC:    DB 0,0         ; INDEX TO STACK C
POPDSK: DB 0,0         ; OUTPUT OF THE POP ROUTINES
SRCDSK: DB 0,0         ; SOURCE DISK TO BE MOVED
TGTDSK: DB 0,0         ; TOP DISK ON TARGET STACK 
RETCD:  DB 0,0         ; RETURN CODE 0=FAIL 1=SUCCESS
KOUNT:  DW 0,0         ; MOVE COUNTER
PARM1:  DW 0,0         ; TEMPORARY PARM 1
PARM2:  DW 0,0         ; TEMPORARY PARM 2
PARM3:  DW 0,0         ; TEMPORARY PARM 3
PARM4:  DW 0,0         ; TEMPORARY PARM 4
;-------------------- 8 BIT DIVISION PARAMETERS
DVDND:     DW 0,0      ; DIVIDEND
TEN:       DB 10,0     ; CONSTANT 10
DIVISOR:   DB 0,0
QUOTIENT:  DW 0,0
REMAINDER: DB 0,0,0,0
RMDR:      DB 0,0,0,0
;---------------------------------------------------
; TRANSLATE TABLE FOR PRINTING A 4-BIT NYBBLE IN HEX
;---------------------------------------------------
HXTBL:  DB '0123456789ABCDEF'
UFMSG:  DB 'STACK UNDERFLOW',13,10,0
; DECIMAL NUMBERS ARE PRINTED FROM A STACK 
STK: DS 16             ; DECIMAL NUMBER STACK
;---------------------------------------------------
STRT:                  ; PROGRAM STARTS HERE
   ; INITIALIZE THE MOVE COUNTER TO ZERO
   XRA A               ; A = 0
   STA KOUNT           ; KOUNT = 0
   STA KOUNT+1
   LXI H,8000H         ; SET STACK ADDRESS AT 8000H
   SPHL                ; SP = HL = 8000H
   ;----------------------------------------------------
   CALL GETPARM        ; OPTIONAL PARM IS NUMBER OF DISKS
   CALL BLD            ; FILL THE SIEVE ARRAY WITH ODD NUMBERS
   ;----------------------------------------------------
   ; MVDSK(N,SRC,TGT,AUX);
   ;----------------------------------------------------
   LXI H,STKB          ; 4TH PARM = AUXILIARY STACK
   PUSH H
   LXI H,STKC          ; 3RD PARM = TARGET STACK
   PUSH H
   LXI H,STKA          ; 2ND PARM = SOURCE STACK
   PUSH H
   LHLD STKSZ          ; 1ST PARM = NUMBER OF DISKS
   PUSH H
   CALL MVDSK          ; CALL RECURSIVE ROUTINE MVDSK
   POP B               ; POP 1ST PARM
   POP B               ; POP 2ND PARM
   POP B               ; POP 3RD PARM
   POP B               ; POP 4TH PARM
   ;----------------------------------------------------
   ; 2^N-1 MOVES HAVE BEEN MADE
   ; NOW SHOW THE FINAL STATE
   ;----------------------------------------------------
   CALL SHW            ; PRINT FINAL DISK STATE
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
; STACKS B AND C ARE EMPTY
;---------------------------------------------
BLD:
   PUSH PSW
   PUSH B
   PUSH H
   ; 9 IS BIGGEST, 2 IS SMALLEST
   ; SOURCE = STKA, TARGET = STKC, AUXILIARY = STKB
   ; RUNTIME PARAMETER DETERMINES HOW MANY DISKS
   XRA A            ; A = 0
   STA STKB         ; STACK B IS EMPTY
   STA STKC         ; STACK C IS EMPTY
   LDA STKSZ        ; NUMBER OF DISKS
   STA STKA         ; STORE IN STACK A HEADER
   LXI H,STKA+1     ; LARGEST DISK ADDRESS
BLD2:
   MOV M,A
   INX H
   DCR A
   ORA A
   JNZ BLD2
   CALL SHW
   POP H
   POP B
   POP PSW
   RET
;-------------------------------------------------------
; RECURSIVE MOVE ROUTINE
; SEE WIKIPEDIA ARTICLE ON THE TOWER OF HANOI
; PARAMETERS:
;    NUMBER OF DISKS
;    SOURCE STACK
;    TARGET STACK
;    AUXILIARY STACK
; STEP 1. MVDSK(N-1,SOURCE,AUXILIARY,TARGET);
; STEP 2. MOVE DISK N FROM SOURCE TO TARGET
; STEP 3. MVDSK(N-1,AUXILIARY,TARGET,SOURCE);
;-------------------------------------------------------
MVDSK:
   PUSH PSW
   PUSH B
   PUSH D
   PUSH H
   ;-------------------------------------------------
   ; IF (N < 1) RETURN;
   ;-------------------------------------------------
   LXI H,10          ; NUMBER OF DISKS INPUT PARM
   DAD SP            ; SP + 10 = 1ST INPUT PARM
   MOV C,M           ; LOAD NUMBER OF DISKS (1-9)
   MVI B,0           ; B = 0
   MOV A,C           ; A = SMALL NUMBER 1-9
   ORA A             ; A EQUAL OR LESS THAN ZERO?
   JZ MDSK9          ; YES, N == 0, RETURN
   JM MDSK9          ; YES, N < 0,  RETURN
   ;-------------------------------------------------
   ; N > 0:
   ; STEP 1. MVDSK(N-1,SRC,AUX,TGT);
   ;-------------------------------------------------
   LXI H,10+4        ; TARGET STACK INPUT PARM
   DAD SP            ; SP + 10 + 4 = 3RD INPUT PARM
   MOV C,M           ; LOAD THE TARGET STACK ADDRESS
   INX H             ; POINT TO HIGH ORDER BYTE
   MOV B,M           ; LOAD THE TARGET STACK ADDRESS
   MOV H,B           ; HL = BC
   MOV L,C
   SHLD PARM4        ; NEW 4TH PARM (NEW AUXILIARY STACK)
   ;-------------------------------------------------
   LXI H,10+6        ; AUXILIARY STACK INPUT PARM
   DAD SP            ; SP + 10 + 6 = 4TH INPUT PARM
   MOV C,M           ; LOAD THE AUXILIARY STACK ADDRESS
   INX H             ; POINT TO HIGH ORDER BYTE
   MOV B,M           ; LOAD THE AUXILIARY STACK ADDRESS
   MOV H,B           ; HL = BC
   MOV L,C
   SHLD PARM3        ; NEW 3RD PARM (NEW TARGET STACK)
   ;-------------------------------------------------
   LXI H,10+2        ; SOURCE STACK INPUT PARM
   DAD SP            ; SP + 10 + 2 = 2ND INPUT PARM
   MOV C,M           ; LOAD THE SOURCE STACK ADDRESS
   INX H             ; POINT TO HIGH ORDER BYTE
   MOV B,M           ; LOAD THE SOURCE STACK ADDRESS
   MOV H,B           ; HL = BC
   MOV L,C
   SHLD PARM2        ; NEW 2ND PARM (NEW SOURCE STACK)
   ;-------------------------------------------------
   LXI H,10          ; NUMBER OF DISKS INPUT PARM
   DAD SP            ; SP + 10 = 1ST INPUT PARM
   MOV C,M           ; LOAD NUMBER OF DISKS
   MVI B,0           ; B = 0
   DCX B             ; NUMBER OF DISKS MINUS ONE
   MOV H,B           ; HL = BC
   MOV L,C
   SHLD PARM1        ; NEW 1ST PARM (NUMBER OF DISKS = N-1)
   ;--------------------------------------------------
   ; PUSH THE 4 PARAMETERS AND MAKE THE RECURSIVE CALL
   ;--------------------------------------------------
   LHLD PARM4        ; PUSH PARM4
   PUSH H
   LHLD PARM3        ; PUSH PARM3
   PUSH H
   LHLD PARM2        ; PUSH PARM2
   PUSH H
   LHLD PARM1        ; PUSH PARM1
   PUSH H
   CALL MVDSK        ; RECURSIVED CALL
   POP B             ; POP 1ST PARM
   POP B             ; POP 2ND PARM
   POP B             ; POP 3RD PARM
   POP B             ; POP 4TH PARM
   ;-------------------------------------------------
   ; STEP 2. TARGET.APPEND(SOURCE.POP());
   ;-------------------------------------------------
   LXI H,10+2        ; SOURCE STACK INPUT PARM
   DAD SP            ; SP + 10 + 2 = 2ND INPUT PARM
   MOV C,M           ; LOAD THE SOURCE STACK ADDRESS
   INX H             ; POINT TO HIGH ORDER BYTE
   MOV B,M           ; LOAD THE SOURCE STACK ADDRESS
   MOV H,B           ; LOAD SOURCE STACK ADDRESS INTO HL
   MOV L,C           ; LOAD SOURCE STACK ADDRESS INTO HL
   MOV A,M           ; LOAD HEADER OF SOURCE STACK INTO A REG
   MOV C,A           ; BC = OFFSET INTO SOURCE STACK
   MVI B,0
   DAD B             ; HL + BC = TOP OF SOURCE STACK ADDRESS
   MOV A,M           ; GET DISK FROM TOP OF SOURCE STACK
   STA POPDSK        ; SAVE THE DISK IN POPDSK
   XRA A             ; A = 0
   MOV M,A           ; TOP OF SOURCE STACK = 0
   ;-------------------------------------------------
   ; GET SOURCE STACK HEADER ADDRESS
   ;-------------------------------------------------
   LXI H,10+2        ; SOURCE STACK INPUT PARM
   DAD SP            ; SP + 10 + 2 = 2ND INPUT PARM
   MOV C,M           ; LOAD THE SOURCE STACK ADDRESS
   INX H             ; POINT TO HIGH ORDER BYTE
   MOV B,M           ; LOAD THE SOURCE STACK ADDRESS
   MOV H,B           ; LOAD SOURCE STACK ADDRESS INTO HL
   MOV L,C           ; LOAD SOURCE STACK ADDRESS INTO HL
   MOV A,M           ; LOAD HEADER OF SOURCE STACK INTO A REG
   ORA A             ; IS STACK LENGTH ZERO?
   JNZ MDSK2         ; NO, NO UNDERFLOW
   CALL UNDRFLOW     ; YES, PRINT UNDERFLOW MESSAGE
   JMP EOJ           ; YES, TERMINATE
MDSK2:
   DCR A             ; SUBTRACT 1 FROM STACK LENGTH
   MOV M,A           ; STORE SOURCE STACK HEADER
   ;-------------------------------------------------
   ; PUSH SOURCE DISK ON TARGET STACK
   ;-------------------------------------------------
   LXI H,10+4        ; TARGET STACK INPUT PARM
   DAD SP            ; SP + 10 + 4 = 3RD INPUT PARM
   MOV C,M           ; LOAD THE TARGET STACK ADDRESS
   INX H             ; POINT TO HIGH ORDER BYTE
   MOV B,M           ; LOAD THE TARGET STACK ADDRESS
   MOV H,B           ; LOAD TARGET STACK ADDRESS INTO HL
   MOV L,C           ; LOAD TARGET STACK ADDRESS INTO HL
   MOV A,M           ; LOAD TARGET STACK LENGTH INTO A REG
   MVI B,0           ; BC = OFFSET TO TOP OF TARGET STACK
   MOV C,A
   DAD B             ; TOP OF TARGET STACK
   INX H             ; EXTEND TARGET STACK
   LDA POPDSK        ; A = SOURCE DISK
   MOV M,A           ; SAVE SOURCE DISK ON TOP OF TARGET STACK
   ;-------------------------------------------------
   ; INCREASE TARGET STACK LENGTH HEADER
   ;-------------------------------------------------
   LXI H,10+4        ; TARGET STACK INPUT PARM
   DAD SP            ; SP + 10 + 4 = 3RD INPUT PARM
   MOV C,M           ; LOAD THE TARGET STACK ADDRESS
   INX H             ; POINT TO HIGH ORDER BYTE
   MOV B,M           ; LOAD THE TARGET STACK ADDRESS
   MOV H,B           ; LOAD TARGET STACK ADDRESS INTO HL
   MOV L,C           ; LOAD TARGET STACK ADDRESS INTO HL
   MOV A,M           ; LOAD TARGET STACK LENGTH INTO A REG
   INR A             ; ADD 1 TO STACK LENGTH
   MOV M,A           ; SAVE NEW LENGTH IN TARGET STACK HEADER
   ;-------------------------------------------------
   LHLD KOUNT        ; HL = MOVE COUNT
   INX H             ; HL += 1
   SHLD KOUNT        ; KOUNT += 1
   CALL PUTKOUNT     ; PRINT THE MOVE COUNT
   CALL SHW          ; PRINT TOWER OF HANOI STATE
   CALL PAUSE        ; PAUSE FOR KEYBOARD INPUT
   ;-------------------------------------------------
   ; STEP 3. MVDSK(N-1,AUX,TGT,SRC);
   ;-------------------------------------------------
   LXI H,10+2        ; SOURCE STACK INPUT PARM
   DAD SP            ; SP + 10 + 2 = 2ND INPUT PARM
   MOV C,M           ; LOAD THE SOURCE STACK ADDRESS
   INX H             ; POINT TO HIGH ORDER BYTE
   MOV B,M           ; LOAD THE SOURCE STACK ADDRESS
   MOV H,B           ; HL = BC
   MOV L,C
   SHLD PARM4        ; NEW 4TH PARM (NEW AUXILIARY STACK)
   ;-------------------------------------------------
   LXI H,10+4        ; TARGET STACK INPUT PARM
   DAD SP            ; SP + 10 + 4 = 3RD INPUT PARM
   MOV C,M           ; LOAD THE TARGET STACK ADDRESS
   INX H             ; POINT TO HIGH ORDER BYTE
   MOV B,M           ; LOAD THE TARGET STACK ADDRESS
   MOV H,B           ; HL = BC
   MOV L,C
   SHLD PARM3        ; NEW 3RD PARM (NEW TARGET STACK)
   ;-------------------------------------------------
   LXI H,10+6        ; AUXILIARY STACK INPUT PARM
   DAD SP            ; SP + 10 + 6 = 4TH INPUT PARM
   MOV C,M           ; LOAD THE AUXILIARY STACK ADDRESS
   INX H             ; POINT TO HIGH ORDER BYTE
   MOV B,M           ; LOAD THE AUXILIARY STACK ADDRESS
   MOV H,B           ; HL = BC
   MOV L,C
   SHLD PARM2        ; NEW 2ND PARM (NEW SOURCE STACK)
   ;-------------------------------------------------
   LXI H,10          ; NUMBER OF DISKS INPUT PARM
   DAD SP            ; SP + 10 = 1ST INPUT PARM
   MOV C,M           ; LOAD NUMBER OF DISKS (1-9)
   MVI B,0           ; B = 0
   DCX B             ; NUMBER OF DISKS MINUS ONE
   MOV H,B           ; HL = BC
   MOV L,C
   SHLD PARM1        ; NEW 1ST PARM (NEW N-1)
   ;--------------------------------------------------
   ; PUSH THE 4 PARAMETERS AND MAKE THE RECURSIVE CALL
   ;--------------------------------------------------
   LHLD PARM4        ; PUSH 4TH PARM
   PUSH H
   LHLD PARM3        ; PUSH 3RD PARM
   PUSH H
   LHLD PARM2        ; PUSH 2ND PARM
   PUSH H
   LHLD PARM1        ; PUSH 1ST PARM
   PUSH H
   CALL MVDSK        ; RECURSIVED CALL
   POP B             ; POP 1ST PARM
   POP B             ; POP 2ND PARM
   POP B             ; POP 3RD PARM
   POP B             ; POP 4TH PARM
   ;--------------------------------------------------
   ; RETURN FROM RECURSIVE CALL
   ;--------------------------------------------------
MDSK9:
   POP H
   POP D
   POP B
   POP PSW
   RET
;-------------------------------------------------------
; PRINT UNDERFLOW MESSAGE
;-------------------------------------------------------
UNDRFLOW:
   PUSH PSW
   PUSH B
   PUSH H
   LXI H,UFMSG          ; HL = ADDRESS OF UNDERFLOW MESSAGE
UFLW2:
   MOV A,M              ; A = NEXT CHARACTER TO PRINT
   ORA A                ; IS A = 0?
   JZ UFLW3             ; YES, STRING PRINTED
   CALL COUT            ; NO, PRINT CHARACTER
   JMP UFLW2            ; REPEAT UNTIL END OF STRING
UFLW3:                  ; RETURN AFTER PRINTING STRING
   POP H
   POP B
   POP PSW
   RET
;-------------------------------------------------------
; PRINT 10 DASHES IN BETWEEN STATES
;-------------------------------------------------------
PDSH:
   PUSH PSW
   PUSH B
   MVI B,10         ; B = LOOP COUNTER
PDSH2:
   MVI A,'-'        ; A = CHARACTER TO PRINT
   CALL COUT        ; PRINT CHARACTER
   DCR B            ; B--
   JNZ PDSH2        ; LOOP B TIMES
   CALL PUTEOL      ; PRINT END OF LINE
   POP B
   POP PSW
   RET
;-------------------------------------------------------
; PRINT MOVE KOUNT IN DECIMAL
;-------------------------------------------------------
PUTKOUNT:
   PUSH PSW
   PUSH B
   PUSH H
   CALL CLRSTK           ; CLEAR DECIMAL STACK
   LHLD KOUNT            ; DIVIDEND = MOVE COUNT
   SHLD DVDND
   LDA TEN               ; DIVISOR = CONSTANT TEN
   STA DIVISOR
   CALL DIV8             ; 8-BIT DIVIDE
   LDA REMAINDER         ; STK[0] = REMAINDER
   STA STK
   ;---------------------------------------------
   LHLD QUOTIENT         ; NEW DIVIDEND = PREVIOUS QUOTIENT
   SHLD DVDND
   LDA TEN               ; DIVISOR = CONSTANT TEN
   STA DIVISOR
   CALL DIV8             ; 8-BIT DIVIDE
   LDA REMAINDER         ; STK[1] = REMAINDER
   STA STK+1
   ;---------------------------------------------
   LHLD QUOTIENT         ; NEW DIVIDEND = PREVIOUS QUOTIENT
   SHLD DVDND
   LDA TEN               ; DIVISOR = CONSTANT TEN
   STA DIVISOR
   CALL DIV8             ; 8-BIT DIVIDE
   LDA REMAINDER         ; STK[2] = REMAINDER
   STA STK+2
   ;---------------------------------------------
   ; NOW PRINT THE COUNT IN DECIMAL BY POPPING
   ; THE DECIMAL STACK FOR EACH REMAINDER DIGIT
   ; DO NOT PRINT LEADING ZEROS
   ;---------------------------------------------
   LDA STK+2             ; IS STK[2] ZERO?
   ORA A
   JNZ PKT2              ; NO, PRINT STK[2] TO STK[0]
   LDA STK+1             ; IS STK[1] ZERO?
   ORA A
   JNZ PKT3              ; NO, PRINT STK[1] TO STK[0]
   JMP PKT4              ; YES, PRINT ONLY STK[0]
PKT2:                    ; PRINT STK[2] TO STK[0]
   LDA STK+2             ; CONVERT STK[2] TO ASCII
   ADI 030H
   CALL COUT             ; PRINT STK[2] IN ASCII
PKT3:                    ; PRINT STK[1] TO STK[0]
   LDA STK+1             ; CONVERT STK[1] TO ASCII
   ADI 030H
   CALL COUT             ; PRINT STK[1] IN ASCII
PKT4:                    ; PRINT ONLY STK[0]
   LDA STK               ; CONVERT STK[0] TO ASCII
   ADI 030H
   CALL COUT             ; PRINT STK[0] IN ASCII
   CALL PUTEOL           ; PRINT END OF LINE
   POP H
   POP B
   POP PSW
   RET
;-------------------------------------------------------
; ROUTINE TO PRINT THE STATE OF THE THREE TOWERS OF HANOI
; THIS ROUTINE PRINTS THE STATE OF THE ALGORITHM
; DURING STEP 2 OF EACH RECURSIVE CALL.
; THE INITIAL STATE AND THE FINAL STATE ARE ALSO PRINTED.
; THE DISKS ARE NUMBERED FROM N TO 1.
; N IS THE LARGEST DISK, AND 1 IS THE SMALLEST DISK.
; THE TOP OF THE STACK IS FOLLOWED BY A ZERO DISK.
; POSITION ZERO OF THE STACK IS A HEADER CONTAINING
; THE INDEX OF THE TOP OF THE STACK.
;-------------------------------------------------------
SHW:
   PUSH PSW
   PUSH B
   PUSH H
   MVI A,'A'         ; PRINT 'A'
   CALL COUTSPC      ; FOLLOWED BY SPACE
   LXI H,STKA+1      ; DISKS ARE STACKED FROM STK[1] TO STK[K] 
SHW2:                ; STACK A LOOP FROM BOTTOM TO TOP
   MOV A,M           ; A = CURRENT DISK IN STACK
   ORA A             ; DISK = ZERO?
   JZ SHW2B          ; YES, END OF LOOP
   ADI 030H          ; NO, PRINT IN ASCII
   CALL COUTSPC      ; PRINT DISK NUMBER FOLLOWED BY SPACE
   INX H             ; POINT TO NEXT DISK HIGHER ON STACK
   JMP SHW2          ; REPEAT STACK A LOOP
SHW2B:               ; END OF STACK A LOOP
   CALL PUTEOL       ; PRINT END OF LINE
   MVI A,'B'         ; PRINT 'B'
   CALL COUTSPC      ; FOLLOWED BY SPACE
   LXI H,STKB+1      ; DISKS ARE STACKED FROM STK[1] TO STK[K] 
SHW3:                ; STACK B LOOP FROM BOTTOM TO TOP
   MOV A,M           ; A = CURRENT DISK IN STACK
   ORA A             ; DISK = ZERO?
   JZ SHW3B          ; YES, END OF LOOP
   ADI 030H          ; NO, PRINT IN ASCII
   CALL COUTSPC      ; PRINT DISK NUMBER FOLLOWED BY SPACE
   INX H             ; POINT TO NEXT DISK HIGHER ON STACK
   JMP SHW3          ; REPEAT STACK B LOOP
SHW3B:               ; END OF STACK B LOOP
   CALL PUTEOL       ; PRINT END OF LINE
   MVI A,'C'         ; PRINT 'C'
   CALL COUTSPC      ; FOLLOWED BY SPACE
   LXI H,STKC+1      ; DISKS ARE STACKED FROM STK[1] TO STK[K] 
SHW4:                ; STACK C LOOP FROM BOTTOM TO TOP
   MOV A,M           ; A = CURRENT DISK IN STACK
   ORA A             ; DISK = ZERO?
   JZ SHW4B          ; YES, END OF LOOP
   ADI 030H          ; NO, PRINT IN ASCII
   CALL COUTSPC      ; PRINT DISK NUMBER FOLLOWED BY SPACE
   INX H             ; POINT TO NEXT DISK HIGHER ON STACK
   JMP SHW4          ; REPEAT STACK C LOOP
SHW4B:               ; END OF STACK C LOOP
   CALL PUTEOL       ; PRINT END OF LINE
   CALL PDSH         ; PRINT DASHES AT END OF STATE
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
   STA STKSZ         ; STKSZ = TOTAL NUMBER OF DISKS
   LXI H,080H        ; PARAMETER LENGTH AT LOCATION HEX 80
   MOV A,M           ; A = LENGTH OF PARAMETER STRING
   ORA A             ; ZERO LENGTH?
   JZ GTPM9          ; YES, THERE IS NO PARAMETER (USE DEFAULT)
   INX H             ; POINT TO START OF PARAMETER STRING
GTPM2:               ; PARAMETER STRING LOOP
   MOV A,M           ; A = CURRENT CHARACTER IN STRING
   ORA A             ; IS A = ZERO? (END OF PARAMETER STRING)
   JZ GTPM9          ; YES, FINISH
   CPI 020H          ; BYPASS LEADING WHITE SPACE
   JZ GTPM3          ; WHITE SPACE, LOOK AT NEXT CHARACTER
   SUI 030H          ; NOT WHITE SPACE, CONVERT ASCII TO BINARY
   CPI 2             ; VALIDATE 2-9
   JM GTPM9          ; USE DEFAULT, IF LESS THAN 2
   CPI 10            ; PARAMETER GREATER THAN 9?
   JP GTPM9          ; YES, USE DEFAULT
   STA STKSZ         ; VALID PARAMETER, SAVE IN STKSZ
   JMP GTPM9         ; FINISH
GTPM3:               ; LOOK AT NEXT CHARACTER IN STRING
   INX H             ; POINT TO NEXT CHARACTER
   JMP GTPM2         ; REPEAT STRING LOOP
GTPM9:               ; END OF GETPARM ROUTINE
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

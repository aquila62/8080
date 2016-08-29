; CRC16F.ASM - COMPUTE THE CCITT CRC FOR A FILE  VERSION 1.0.0
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

;---------------------------------------------------------
; THE STRUCTURE OF THIS PROGRAM WAS INSPIRED BY THE DIGITAL
; RESEARCH SAMPLE FILE COPY PROGRAM.
;
; IT HASN BEEN MODIFIED TO DUMP A FILE IN HEX AND ASCII.
;
; AT THE CCP LEVEL, THE COMMAND USAGE:
;
; CRC16F A:X.Y
;
; EXAMPLE:
;
; CRC16F CRC16F.COM
;
;---------------------------------------------------------
; THIS PROGRAM PERFORMS THE CCITT VERSION OF A 16 BIT CRC.
; CRC STANDS FOR CYCLICAL REDUNDANCY CHECK.
; IT IS USED FOR VALIDATING DATA TRANSMITTED OVER A
; TRANSMISSION LINE.
;
; USAGE:
;
; CRC16 FILENAME
;
; EXAMPLE:
;
; CRC16 CRC16.COM
;
; THIS PROGRAM PRINTS THE 16 BIT CCITT CRC OF A FILE
;--------------------------------------------------------------

KCIN    EQU 0006H     ; KEYBOARD INPUT ROUTINE
KCOUT   EQU 0009H     ; CONSOLE OUTPUT ROUTINE
BOOT    EQU 0000H     ; SYSTEM REBOOT
BDOS    EQU 0005H     ; BDOS ENTRY POINT
FCB1    EQU 005CH     ; FIRST FILE NAME
SFCB    EQU FCB1      ; SOURCE FCB
DBUFF   EQU 0080H     ; DEFAULT BUFFER
TPA     EQU 0100H     ; BEGINNING OF TPA TRANSIENT PROGRAM AREA
;
PRINTF  EQU 9         ; PRINT BUFFER FUNC#
OPENF   EQU 15        ; OPEN FILE FUNC#
CLSF    EQU 16        ; CLOSE FILE FUNC#
DLTF    EQU 19        ; DELETE FILE FUNC#
READF   EQU 20        ; SEQUENTIAL READ FUNC#
WRTF    EQU 21        ; SEQUENTIAL WRITE FUNC#
MAKEF   EQU 22        ; MAKE FILE FUNC#  (CREATE FILE)
;
	ORG TPA            ; BEGINNING OF TPA
	JMP STRT
;
; CONSOLE MESSAGES
;
NOFILE:
	DB 'FILE NOT FOUND.$'
	DB 0
HXTBL:  DB '0123456789ABCDEF'
	DB 0,0,0,0
;
; DATA AREAS
;
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
     	DW 0,0,0,0
STRT:
	LXI SP,STKEND      ; SET LOCAL STACK
	CALL BLD           ; BUILD THE CRC TABLE
	MVI A,0FFH         ; SET CRC TO 0FFFFH
	STA CRC
	STA CRC+1
;
; SOURCE FCB IS READY
;
	LXI D,SFCB         ; SOURCE FILE
	CALL OPEN          ; ERROR IF 255
	LXI D,NOFILE       ; READY MESSAGE
	INR A              ; 255 BECOMES 0
	CPI 0H
	JZ FINIS           ; DONE IF NO FILE
;
; SOURCE FILE OPEN
; DUMP IN HEX UNTIL END OF FILE ON SOURCE
; END OF FILE IS A READ LENGTH OF ZERO
;
COPY:
	LXI D,SFCB         ; SOURCE
	CALL READ          ; READ NEXT RECORD
	CPI 0H             ; END OF FILE?
	JNZ EOJ            ; PRINT CRC AND REBOOT CP/M
;
; NOT END OF FILE, DUMP THE RECORD WITH 8 LINES OF HEX
;
	STA LEN            ; SAVE THE LENGTH OF THE BUFFER
	CALL CALC
	JMP COPY
;
; END OF JOB
;
EOJ:
	CALL PUTCRC
	JMP BOOT
;
; WRITE MESSAGE GIVEN IN DE, REBOOT
;
FINIS:
	MVI C,PRINTF
	CALL BDOS          ; WRITE MESSAGE
	JMP BOOT           ; REBOOT SYSTEM
;
; SYSTEM INTERFACE SUBROUTINES
; (ALL RETURN DIRECTLY FROM BDOS)
;
OPEN:
	MVI C,OPENF
	JMP BDOS
;
CLOSE:
	MVI C,CLSF
	JMP BDOS
;
DELETE:
	MVI C,DLTF
	JMP BDOS
;
READ:
	MVI C,READF
	JMP BDOS
;
WRITE:
	MVI C,WRTF
	JMP BDOS
;
MAKE:
	MVI C,MAKEF
	JMP BDOS
;-----------------------------------------------
; CALCULATE THE 16 BIT CRC OF THE PARM STRING
;-----------------------------------------------
CALC:
   PUSH PSW
   PUSH H
   LXI H,80H           ; HL POINTS TO START OF BUFFER
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
;
; PRINT A REGISTER IN HEX
;
PUTHXA:
   CALL PUTHEX
   CALL PUTSPC
   RET
;
; PRINT BC REGISTER IN HEX
;
PUTBC:
   PUSH PSW
   PUSH B
   MOV A,B
   CALL PUTHEX
   MOV A,C
   CALL PUTHEX
   CALL PUTSPC
   POP B
   POP PSW
   RET
;
; PRINT HL REGISTER IN HEX
;
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
; PRINT ASTERISK FOLLOWED BY ONE SPACE
;----------------------------------------------------------
PUTAST:
   PUSH PSW
   MVI A,'*'
   CALL COUT
   MVI A,20H
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
STACK:   DS 1024         ; 512 LEVEL STACK
STKEND:  DS 8            ; END OF STACK
   END                   ; END OF PROGRAM

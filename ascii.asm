; ASCII.ASM - PRINT ASCII CHARACTERS IN SEQUENCE  VERSION 1.0.0
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
; THIS PROGRAM PRINTS THE ASCII CHARACTER SET ; WITH 16 CHARACTERS PER LINE.
; FROM THIS OUTPUT IT IS POSSIBLE TO DETERMINE THE
; HEX CODE FOR EACH CHARACTER.
; THE FIRST LINE STARTS AT HEX 020H.
;--------------------------------------------------------------

KCIN   EQU 0006H       ; CP/M BRANCH VECTOR FOR KEYBOARD INPUT
KCOUT  EQU 0009H       ; CP/M BRANCH VECTOR FOR CONSOLE  OUTPUT
   ORG 100H            ; CP/M TPA STARTING ADDRESS
   ;-----------------------------------------------------------
   ; INITIALIZE KOUNT TO ZERO
   ; KOUNT IS USED TO DETERMINE END OF LINE
   ;-----------------------------------------------------------
   XRA A
   STA KOUNT
   ;-----------------------------------------------------------
   ; START ASCII PRINT OUT AT SPACE
   ; BINDGT CONTAINS THE ASCII CHARACTER TO PRINT
   ;-----------------------------------------------------------
   MVI A,020H
   STA BINDGT               ; STARTING ASCII CHARACTER
LP:                         ; ASCII CHARACTER LOOP
   LDA BINDGT               ; LOAD CURRENT CHARACTER INTO A
   CPI 128                  ; TEST FOR END OF ASCII ALPHABET
   JP EOJ                   ; IF END OF ALPHABET, STOP RUN
   CALL COUT                ; PRINT CURRENT CHARACTER
   CALL PUTSPC              ; PRINT ONE SPACE
   ;----------------------------------------------------
   ; POINT TO NEXT ASCII CHARACTER
   ;----------------------------------------------------
   INR A
   STA BINDGT
   ;----------------------------------------------------
   ; INCREMENT KOUNT
   ; IF KOUNT == 16, PRINT END OF LINE SEQUENCE
   ; OTHERWISE REPEAT LOOP
   ;----------------------------------------------------
   LDA KOUNT
   INR A
   STA KOUNT
   CPI 16
   JM LP
   XRA A
   STA KOUNT
   CALL PUTEOL
   JMP LP
EOJ:                       ; END OF JOB
   JMP 0H                  ; RESET CP/M
   NOP
   NOP
   NOP
   NOP
;----------------------------------------------------------
; PRINT A REGISTER IN HEXADECIMAL
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
   MOV A,H
   CALL PUTHEX
   MOV A,L
   CALL PUTHEX
   CALL PUTSPC
   POP PSW
   RET
;----------------------------------------------------------
; PRINT A REGISTER IN HEXADECIMAL
;----------------------------------------------------------
PUTHEX:
   PUSH PSW
   PUSH B
   MOV B,A
   STC           ; SET CARRY
   CMC           ; CLEAR CARRY
   RAL
   CMC
   RAL
   CMC
   RAL
   CMC
   RAL
   CALL PUTNBL
   MOV A,B
   ANI 0FH
   CALL PUTNBL
   POP B
   POP PSW
   RET
;----------------------------------------------------------
; PRINT 4-BIT NIBBLE IN HEXADECIMAL
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
BINDGT: DB 0,0          ; CURRENT ASCII CHARACTER
KOUNT:  DB 0,0          ; COUNT FOR END OF LINE
HXTBL:  DB '0123456789ABCDEF'      ; HEX TRANSLATE TABLE
   NOP
   NOP
   NOP
   NOP
   END

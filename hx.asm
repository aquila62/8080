; HX.ASM - PRINT MEMORY IN HEX   VERSION 1.0.0
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

; USAGE:

; HX [ADDRESS]

; WHERE ADDRESS IS IN HEX

; EXAMPLE:

; HX 0100

; DEFAULT ADDRESS IS ZERO

KCIN   EQU 0006H
KCOUT  EQU 0009H
   ORG 100H
   XRA A
   STA KOUNT
   CALL GETPARM
LP:
   CALL PUTADDR
LP2:
   LHLD ADDR
   MOV A,M
   CALL PUTHXA
   INX H
   SHLD ADDR
   LDA KOUNT
   INR A
   STA KOUNT
   CPI 16
   JM LP2
   CALL PUTEOL
   XRA A
   STA KOUNT
   CALL CIN
   CPI 01AH
   JZ EOJ
   JMP LP
EOJ:
   JMP 0H
   NOP
   NOP
   NOP
   NOP
GETPARM:
   PUSH PSW
   PUSH B
   PUSH H
   XRA A
   MOV H,A
   MOV L,A
   SHLD ADDR
   LDA 080H
   DCR A
   STA LEN
   ADI 081H
   STA ENDPRM
   LXI H,081H
   SHLD PRMCH
BYP1:
   LHLD PRMCH
   MOV A,M
   CPI 020H
   JNZ PRM2
   INX H
   SHLD PRMCH
   JMP BYP1
PRM2:
   LHLD PRMCH
   MOV A,M
   CPI 0
   JZ PRM3
   CPI 020H
   JZ PRM3
   CALL HX2BN
   LHLD ADDR
   ;-------------
   STC
   CMC
   MOV A,L
   RAL
   MOV L,A
   MOV A,H
   RAL
   MOV H,A
   ;-------------
   STC
   CMC
   MOV A,L
   RAL
   MOV L,A
   MOV A,H
   RAL
   MOV H,A
   ;-------------
   STC
   CMC
   MOV A,L
   RAL
   MOV L,A
   MOV A,H
   RAL
   MOV H,A
   ;-------------
   STC
   CMC
   MOV A,L
   RAL
   MOV L,A
   MOV A,H
   RAL
   MOV H,A
   ;-------------
   SHLD ADDR
   LDA BINDGT
   MOV C,A
   MVI B,0H
   LHLD ADDR
   DAD B
   SHLD ADDR
   LHLD PRMCH
   INX H
   SHLD PRMCH
   LDA OFST
   INR A
   STA OFST
   JMP PRM2
PRM3:
   POP H
   POP B
   POP PSW
   RET
HX2BN:               ; OUTPUT IN BINDGT
   PUSH PSW
   PUSH H
   CPI 030H
   JM BADHX
   CPI 067H
   JP BADHX
   CPI 061H
   JM HXBN2
   SUI 20H
HXBN2:
   CPI 047H
   JP BADHX
   CPI 041H
   JM HXBN3
   SUI 07H
HXBN3:
   CPI 040H
   JP BADHX
   SUI 030H
   LXI H,BINTB
   MOV C,A
   MVI B,0
   DAD B
   MOV A,M
   STA BINDGT
   POP H
   POP PSW
   RET
BADHX:
   POP H
   POP PSW
   CALL COUT
   CALL PUTSPC
   MVI A,'?'
   CALL COUT
   JMP EOJ
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
; PRINT ADDR IN HEXADECIMAL
;----------------------------------------------------------
PUTADDR:
   PUSH PSW
   PUSH H
   LDA ADDR+1
   CALL PUTHEX
   LDA ADDR
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
   PUSH H
   LXI D,KCIN
   CALL IOS
   POP H
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
ADDR:   DW 0
PRMCH:  DW 0
ENDPRM: DW 0
BINDGT: DB 0,0
KOUNT:  DB 0,0
HXTBL:  DB '0123456789ABCDEF'
BINTB:  DB 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
LEN:    DB 0,0
OFST:   DB 0,0
PRMEND: DB 0,0
   NOP
   NOP
   NOP
   NOP
   END
